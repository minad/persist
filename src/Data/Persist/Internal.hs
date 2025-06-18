{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Persist.Internal (
      (:!:)(..)
    -- * The Get type
    , Get(..)
    , GetEnv(..)
    , GetException(..)
    , getOffset
    , failGet
    , runGet
    , runGetIO

    -- * The Put type
    , Put(..)
    , PutEnv(..)
    , Chunk(..)
    , evalPut
    , evalPutIO
    , grow
) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable (foldl', foldlM)
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word
import Foreign (ForeignPtr, Ptr, plusPtr, minusPtr,
                withForeignPtr, mallocBytes, free, allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import System.IO.Unsafe
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B

#include "MachDeps.h"

data a :!: b = !a :!: !b
infixl 2 :!:

data GetEnv = GetEnv
  { geBuf   :: !(ForeignPtr Word8)
  , geBegin :: {-#UNPACK#-}!(Ptr Word8)
  , geEnd   :: {-#UNPACK#-}!(Ptr Word8)
  , geTmp   :: {-#UNPACK#-}!(Ptr Word8)
  }

newtype Get a = Get
  { unGet :: GetEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a)
  }

instance Functor Get where
  fmap f m = Get $ \e p -> do
    p' :!: x <- unGet m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Get where
  pure a = Get $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Get $ \e p -> do
    p' :!: f' <- unGet f e p
    p'' :!: a' <- unGet a e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Get where
  m >>= f = Get $ \e p -> do
    p' :!: x <- unGet m e p
    unGet (f x) e p'
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

instance Fail.MonadFail Get where
  fail msg = failGet GenericGetException ("Failed reading: " <> msg)
  {-# INLINE fail #-}

getOffset :: Get Int
getOffset = Get $ \e p -> pure $! p :!: (p `minusPtr` geBegin e)
{-# INLINE getOffset #-}

failGet :: (Int -> String -> GetException) -> String -> Get a
failGet ctor msg = do
  offset <- getOffset
  Get $ \_ _ -> throwIO (ctor offset msg)

runGetIO :: Get a -> ByteString -> IO a
runGetIO m s = run
  where run = withForeignPtr buf $ \p -> allocaBytes 8 $ \t -> do
          let env = GetEnv { geBuf = buf, geBegin = p, geEnd = p `plusPtr` (pos + len), geTmp = t }
          _ :!: r <- unGet m env (p `plusPtr` pos)
          pure r
        (B.PS buf pos len) = s

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> ByteString -> Either String a
runGet m s = unsafePerformIO $ catch (Right <$!> runGetIO m s) handler
  where handler (e :: GetException) = pure $ Left $ displayException e
{-# NOINLINE runGet #-}

data Chunk = Chunk
  { chkBegin :: {-#UNPACK#-}!(Ptr Word8)
  , chkEnd   :: {-#UNPACK#-}!(Ptr Word8)
  }

data PutEnv = PutEnv
  { peChks :: !(IORef (NonEmpty Chunk))
  , peEnd  :: !(IORef (Ptr Word8))
  , peTmp  :: {-#UNPACK#-}!(Ptr Word8)
  }

newtype Put a = Put
  { unPut :: PutEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a) }

instance Functor Put where
  fmap f m = Put $ \e p -> do
    p' :!: x <- unPut m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Put where
  pure a = Put $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Put $ \e p -> do
    p' :!: f' <- unPut f e p
    p'' :!: a' <- unPut a e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Put where
  m >>= f = Put $ \e p -> do
    p' :!: x <- unPut m e p
    unPut (f x) e p'
  {-# INLINE (>>=) #-}

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
      end <- readIORef (peEnd e)
      if end `minusPtr` p >= n then
        pure $! p :!: ()
      else
        doGrow e p n
{-# INLINE grow #-}

doGrow :: PutEnv -> Ptr Word8 -> Int -> IO (Ptr Word8 :!: ())
doGrow e p n = do
  k <- newChunk n
  modifyIORef' (peChks e) $ \case
    (c:|cs) ->
      let !c' = c { chkEnd = p }
       in k :| c' : cs
  writeIORef (peEnd e) $! chkEnd k
  pure $! chkBegin k :!: ()
{-# NOINLINE doGrow #-}

chunksLength :: [Chunk] -> Int
chunksLength = foldl' (\s c -> s + chkEnd c `minusPtr` chkBegin c) 0
{-# INLINE chunksLength #-}

catChunks :: [Chunk] -> IO ByteString
catChunks chks = B.create (chunksLength chks) $ \p ->
  void $ foldlM (\q c -> do
                    let n = chkEnd c `minusPtr` chkBegin c
                    copyBytes q (chkBegin c) n
                    free $ chkBegin c
                    pure (q `plusPtr` n)) p $ reverse chks
{-# INLINE catChunks #-}

evalPutIO :: Put a -> IO (a, ByteString)
evalPutIO p = do
  k <- newChunk 0
  chks <- newIORef (k:|[])
  end <- newIORef (chkEnd k)
  p' :!: r <- allocaBytes 8 $ \t ->
    unPut p PutEnv { peChks = chks, peEnd = end, peTmp = t } (chkBegin k)
  cs <- readIORef chks
  s <- case cs of
    (x:|xs) -> catChunks $ x { chkEnd = p' } : xs
  pure (r, s)

evalPut :: Put a -> (a, ByteString)
evalPut p = unsafePerformIO $ evalPutIO p
{-# NOINLINE evalPut #-}
