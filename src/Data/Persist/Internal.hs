{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Persist.Internal (
      (:!:)(..)
    -- * The Get type
    , Get(..)
    , GetState
    , GetEnv(..)
    , GetException(..)
    , offset
    , failGet
    , stateGet
    , setStateGet
    , runGet
    , runGetIO

    -- * The Put type
    , Put(..)
    , PutState
    , PutEnv(..)
    , Chunk(..)
    , evalPut
    , evalPutIO
    , grow
    , statePut
    , setStatePut
) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word
import Foreign (ForeignPtr, Ptr, plusPtr, minusPtr,
                withForeignPtr, mallocBytes, free, allocaBytes)
import System.IO.Unsafe
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as B

#include "MachDeps.h"

data a :!: b = !a :!: !b
infixl 2 :!:

type family GetState s

data GetEnv s = GetEnv
  { geBuf   :: !(ForeignPtr Word8)
  , geBegin :: {-#UNPACK#-}!(Ptr Word8)
  , geEnd   :: {-#UNPACK#-}!(Ptr Word8)
  , geTmp   :: {-#UNPACK#-}!(Ptr Word8)
  , geState :: !(IORef (GetState s))
  }

newtype Get s a = Get
  { unGet :: GetEnv s -> Ptr Word8 -> IO ((Ptr Word8) :!: a)
  }

instance Functor (Get s) where
  fmap f m = Get $ \e p -> do
    p' :!: x <- unGet m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative (Get s) where
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

instance Monad (Get s) where
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

instance Fail.MonadFail (Get s) where
  fail msg = failGet GenericGetException ("Failed reading: " <> msg)
  {-# INLINE fail #-}

offset :: Get s Int
offset = Get $ \e p -> pure $! p :!: (p `minusPtr` (geBegin e))
{-# INLINE offset #-}

failGet :: (Int -> String -> GetException) -> String -> Get s a
failGet ctor msg = do
  off <- offset
  Get $ \_ _ -> throwIO (ctor off msg)

stateGet :: Get s (GetState s)
stateGet = Get $ \e p -> do
  s <- readIORef (geState e)
  pure $! p :!: s
{-# INLINE stateGet #-}

statePut :: Put s (PutState s)
statePut = Put $ \e p -> do
  s <- readIORef (peState e)
  pure $! p :!: s
{-# INLINE statePut #-}

setStateGet :: GetState s -> Get s ()
setStateGet s = Get $ \e p -> do
  writeIORef (geState e) s
  pure $! p :!: ()
{-# INLINE setStateGet #-}

setStatePut :: PutState s -> Put s ()
setStatePut s = Put $ \e p -> do
  writeIORef (peState e) s
  pure $! p :!: ()
{-# INLINE setStatePut #-}

runGetIO :: Get s a -> GetState s -> ByteString -> IO a
runGetIO m g s = run
  where run = withForeignPtr buf $ \p -> allocaBytes 8 $ \t -> do
          rg <- newIORef g
          let env = GetEnv { geState = rg, geBuf = buf, geBegin = p, geEnd = p `plusPtr` (pos + len), geTmp = t }
          _ :!: r <- unGet m env (p `plusPtr` pos)
          pure r
        (B.PS buf pos len) = s

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get s a -> GetState s -> ByteString -> Either String a
runGet m g s = unsafePerformIO $ catch (Right <$!> (runGetIO m g s)) handler
  where handler (x :: GetException) = pure $ Left $ displayException x
{-# NOINLINE runGet #-}

data Chunk = Chunk
  { chkBegin :: {-#UNPACK#-}!(Ptr Word8)
  , chkEnd   :: {-#UNPACK#-}!(Ptr Word8)
  }

type family PutState s

data PutEnv s = PutEnv
  { peChks :: !(IORef (NonEmpty Chunk))
  , peEnd  :: !(IORef (Ptr Word8))
  , peTmp  :: {-#UNPACK#-}!(Ptr Word8)
  , peState :: !(IORef (PutState s))
  }

newtype Put s a = Put
  { unPut :: PutEnv s -> Ptr Word8 -> IO ((Ptr Word8) :!: a) }

instance Functor (Put s) where
  fmap f m = Put $ \e p -> do
    p' :!: x <- unPut m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative (Put s) where
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

instance Monad (Put s) where
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
grow :: Int -> Put s ()
grow n
  | n < 0 = error "grow: negative length"
  | otherwise = Put $ \e p -> do
      end <- readIORef (peEnd e)
      if end `minusPtr` p >= n then
        pure $! p :!: ()
      else
        doGrow e p n
{-# INLINE grow #-}

doGrow :: PutEnv s -> Ptr Word8 -> Int -> IO ((Ptr Word8) :!: ())
doGrow e p n = do
  k <- newChunk n
  modifyIORef' (peChks e) $ \case
    (c:|cs) -> k :| c { chkEnd = p } : cs
  writeIORef (peEnd e) (chkEnd k)
  pure $! chkBegin k :!: ()
{-# NOINLINE doGrow #-}

chunksLength :: [Chunk] -> Int
chunksLength = foldr (\c s -> s + chkEnd c `minusPtr` chkBegin c) 0
{-# INLINE chunksLength #-}

catChunks :: [Chunk] -> IO ByteString
catChunks chks = B.create (chunksLength chks) $ \p ->
  void $ foldlM (\q c -> do
                    let n = chkEnd c `minusPtr` chkBegin c
                    B.memcpy q (chkBegin c) n
                    free $ chkBegin c
                    pure (q `plusPtr` n)) p $ reverse chks
{-# INLINE catChunks #-}

evalPutIO :: Put s a -> PutState s -> IO (a, ByteString)
evalPutIO p ps = do
  k <- newChunk 0
  chks <- newIORef (k:|[])
  end <- newIORef (chkEnd k)
  rps <- newIORef ps
  p' :!: r <- allocaBytes 8 $ \t ->
    unPut p PutEnv { peState = rps, peChks = chks, peEnd = end, peTmp = t } (chkBegin k)
  cs <- readIORef chks
  s <- case cs of
    (x:|xs) -> catChunks $ x { chkEnd = p' } : xs
  pure (r, s)

evalPut :: Put s a -> PutState s -> (a, ByteString)
evalPut s e = unsafePerformIO $ evalPutIO s e
{-# NOINLINE evalPut #-}
