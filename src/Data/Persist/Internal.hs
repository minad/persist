{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Persist.Internal
  ( (:!:) (..)

    -- * The Get type
  , Get (..)
  , GetEnv (..)
  , GetException (..)
  , getOffset
  , failGet
  , runGet
  , runGetIO
  , unsafeGetPrefix

    -- * The Put type
  , Put (..)
  , PutEnv (..)
  , PutException (..)
  , Chunk (..)
  , evalPut
  , evalPutIO
  , grow

    -- * Size reservations
  , PutSize (..)
  ) where

import Control.Exception
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Data.Foldable (foldl', foldlM)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word
import Foreign
  ( ForeignPtr
  , Ptr
  , allocaBytes
  , free
  , mallocBytes
  , minusPtr
  , plusPtr
  , withForeignPtr
  )
import Foreign.Marshal.Utils (copyBytes)
import System.IO.Unsafe

#include "MachDeps.h"

data a :!: b = !a :!: !b
infixl 2 :!:

data GetEnv = GetEnv
  { buf :: !(ForeignPtr Word8)
  , begin :: {-# UNPACK #-} !(Ptr Word8)
  , end :: {-# UNPACK #-} !(Ptr Word8)
  , tmp :: {-# UNPACK #-} !(Ptr Word8)
  }

newtype Get a = Get
  { unGet :: GetEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a)
  }

instance Functor Get where
  fmap f m = Get $ \e p -> do
    p' :!: x <- m.unGet e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Get where
  pure a = Get $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Get $ \e p -> do
    p' :!: f' <- f.unGet e p
    p'' :!: a' <- a.unGet e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Get where
  m >>= f = Get $ \e p -> do
    p' :!: x <- m.unGet e p
    (f x).unGet e p'
  {-# INLINE (>>=) #-}

#if !MIN_VERSION_base(4,11,0)
  fail = Fail.fail
  {-# INLINE fail #-}
#endif

data GetException
  = LengthException Int String
  | CharException Int String
  | EOFException Int String
  | GenericGetException Int String
  deriving (Eq, Show)

instance Exception GetException

data PutException
  = PutSizeMissingStartChunk
  deriving (Eq, Show)

instance Exception PutException

instance Fail.MonadFail Get where
  fail msg = failGet GenericGetException ("Failed reading: " <> msg)
  {-# INLINE fail #-}

getOffset :: Get Int
getOffset = Get $ \e p -> pure $! p :!: (p `minusPtr` e.begin)
{-# INLINE getOffset #-}

failGet :: (Int -> String -> GetException) -> String -> Get a
failGet ctor msg = do
  offset <- getOffset
  Get $ \_ _ -> throwIO (ctor offset msg)

runGetIO :: Get a -> ByteString -> IO a
runGetIO m s = run
 where
  run = withForeignPtr buf $ \p -> allocaBytes 8 $ \t -> do
    let env = GetEnv {buf, begin = p, end = p `plusPtr` (pos + len), tmp = t}
    _ :!: r <- m.unGet env (p `plusPtr` pos)
    pure r
  (B.PS buf pos len) = s

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> ByteString -> Either String a
runGet m s = unsafePerformIO $ catch (Right <$!> runGetIO m s) handler
 where
  handler (e :: GetException) = pure $ Left $ displayException e
{-# NOINLINE runGet #-}

unsafeGetPrefix :: Int -> Get a -> Get a
unsafeGetPrefix prefixLength baseGet = Get $ \env p -> do
  let p' = p `plusPtr` prefixLength
      env' = (\GetEnv {..} -> GetEnv {end = p', ..}) env
  _ :!: r <- baseGet.unGet env' p
  pure $ p' :!: r
{-# INLINE unsafeGetPrefix #-}

data Chunk = Chunk
  { begin :: {-# UNPACK #-} !(Ptr Word8)
  , end :: {-# UNPACK #-} !(Ptr Word8)
  }

data PutEnv = PutEnv
  { chunks :: !(IORef (NonEmpty Chunk))
  , end :: !(IORef (Ptr Word8))
  , tmp :: {-# UNPACK #-} !(Ptr Word8)
  }

newtype Put a = Put
  {unPut :: PutEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a)}

instance Functor Put where
  fmap f m = Put $ \e p -> do
    p' :!: x <- m.unPut e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Put where
  pure a = Put $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Put $ \e p -> do
    p' :!: f' <- f.unPut e p
    p'' :!: a' <- a.unPut e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Put where
  m >>= f = Put $ \e p -> do
    p' :!: x <- m.unPut e p
    (f x).unPut e p'
  {-# INLINE (>>=) #-}

data PutSize a = PutSize
  { sizePtr :: !(Ptr Word8)
  , sizeStart :: !(Ptr Word8)
  , chunkStart :: !(Ptr Word8)
  }

minChunkSize :: Int
minChunkSize = 0x10000
{-# INLINE minChunkSize #-}

newChunk :: Int -> IO Chunk
newChunk size = do
  let n = max size minChunkSize
  p <- mallocBytes n
  pure $! Chunk p $ p `plusPtr` n
{-# INLINE newChunk #-}

-- | Ensure that @n@ bytes can be written.
grow :: Int -> Put ()
grow n
  | n < 0 = error "grow: negative length"
  | otherwise = Put $ \e p -> do
      end <- readIORef e.end
      if end `minusPtr` p >= n
        then
          pure $! p :!: ()
        else
          doGrow e p n
{-# INLINE grow #-}

doGrow :: PutEnv -> Ptr Word8 -> Int -> IO (Ptr Word8 :!: ())
doGrow e p n = do
  k <- newChunk n
  modifyIORef' e.chunks $ \case
    (c :| cs) ->
      let !c' = (\Chunk {..} -> Chunk {end = p, ..}) c
       in k :| c' : cs
  writeIORef e.end $! k.end
  pure $! k.begin :!: ()
{-# NOINLINE doGrow #-}

chunksLength :: [Chunk] -> Int
chunksLength = foldl' (\s c -> s + c.end `minusPtr` c.begin) 0
{-# INLINE chunksLength #-}

catChunks :: [Chunk] -> IO ByteString
catChunks chks = B.create (chunksLength chks) $ \p ->
  void
    $ foldlM
      ( \q c -> do
          let n = c.end `minusPtr` c.begin
          copyBytes q c.begin n
          free c.begin
          pure (q `plusPtr` n)
      )
      p
    $ reverse chks
{-# INLINE catChunks #-}

evalPutIO :: Put a -> IO (a, ByteString)
evalPutIO p = do
  k <- newChunk 0
  chunks <- newIORef (k :| [])
  curEnd <- newIORef k.end
  p' :!: r <- allocaBytes 8 $ \tmp ->
    p.unPut PutEnv {chunks, end = curEnd, tmp} k.begin
  cs <- readIORef chunks
  s <- case cs of
    (x :| xs) ->
      let !x' = (\Chunk {..} -> Chunk {end = p', ..}) x
       in catChunks $ x' : xs
  pure (r, s)

evalPut :: Put a -> (a, ByteString)
evalPut p = unsafePerformIO $ evalPutIO p
{-# NOINLINE evalPut #-}
