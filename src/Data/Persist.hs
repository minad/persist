{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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

module Data.Persist (

    -- * The Persist class
      Persist(..)

    -- * Endianness
    , HostEndian
    , BigEndian(..)
    , LittleEndian(..)

    -- * Serialization
    , encode
    , decode

    -- * The Get type
    , Get
    , GetState
    , stateGet
    , setStateGet
    , modifyStateGet
    , runGet
    , ensure
    , skip
    , getBytes
    , getByteString
    , remaining
    , eof
    , getHE
    , getLE
    , getBE

    -- * The Put type
    , Put
    , PutState
    , statePut
    , setStatePut
    , modifyStatePut
    , runPut
    , evalPut
    , grow
    , putByteString
    , putHE
    , putLE
    , putBE
) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Persist.Internal
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import Foreign (Ptr, Storable(..), plusPtr, minusPtr, castPtr, withForeignPtr)
import GHC.Base (unsafeChr, ord)
import GHC.Exts (IsList(..))
import GHC.Generics
import GHC.Real (Ratio(..))
import GHC.TypeLits
import Numeric.Natural
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Short.Internal as S
import qualified Data.Monoid as M
import qualified Data.Text.Encoding as TE
import qualified Data.Tree as T

#include "MachDeps.h"

putHE :: Persist s (HostEndian a) => a -> Put s ()
getHE :: Persist s (HostEndian a) => Get s a
{-# INLINE putHE #-}
{-# INLINE getHE #-}

#ifdef WORDS_BIGENDIAN
type HostEndian = BigEndian
getHE = getBE
putHE = putBE
#else
type HostEndian = LittleEndian
getHE = getLE
putHE = putLE
#endif

poke16LE :: Ptr Word8 -> Word16 -> IO ()
poke32LE :: Ptr Word8 -> Word32 -> IO ()
poke64LE :: Ptr Word8 -> Word64 -> IO ()
{-# INLINE poke16LE #-}
{-# INLINE poke32LE #-}
{-# INLINE poke64LE #-}

poke16BE :: Ptr Word8 -> Word16 -> IO ()
poke32BE :: Ptr Word8 -> Word32 -> IO ()
poke64BE :: Ptr Word8 -> Word64 -> IO ()
{-# INLINE poke16BE #-}
{-# INLINE poke32BE #-}
{-# INLINE poke64BE #-}

peek16LE :: Ptr Word8 -> IO Word16
peek32LE :: Ptr Word8 -> IO Word32
peek64LE :: Ptr Word8 -> IO Word64
{-# INLINE peek16LE #-}
{-# INLINE peek32LE #-}
{-# INLINE peek64LE #-}

peek16BE :: Ptr Word8 -> IO Word16
peek32BE :: Ptr Word8 -> IO Word32
peek64BE :: Ptr Word8 -> IO Word64
{-# INLINE peek16BE #-}
{-# INLINE peek32BE #-}
{-# INLINE peek64BE #-}

#ifndef UNALIGNED_MEMORY
pokeByte :: (Integral a) => Ptr Word8 -> a -> IO ()
pokeByte p x = poke p (fromIntegral x)
{-# INLINE pokeByte #-}

peekByte :: (Integral a) => Ptr Word8 -> IO a
peekByte p = do
  !b <- peek p
  return $! fromIntegral b
{-# INLINE peekByte #-}

poke16LE p y = do
  pokeByte p $ y
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 8

poke16BE p y = do
  pokeByte p $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 1) $ y

poke32LE p y = do
  pokeByte p $ y
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 3) $ y `unsafeShiftR` 24

poke32BE p y = do
  pokeByte p $ y `unsafeShiftR` 24
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 3) $ y

poke64LE p y = do
  pokeByte p $ y
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 3) $ y `unsafeShiftR` 24
  pokeByte (p `plusPtr` 4) $ y `unsafeShiftR` 32
  pokeByte (p `plusPtr` 5) $ y `unsafeShiftR` 40
  pokeByte (p `plusPtr` 6) $ y `unsafeShiftR` 48
  pokeByte (p `plusPtr` 7) $ y `unsafeShiftR` 56

poke64BE p y = do
  pokeByte p $ y `unsafeShiftR` 56
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 48
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 40
  pokeByte (p `plusPtr` 3) $ y `unsafeShiftR` 32
  pokeByte (p `plusPtr` 4) $ y `unsafeShiftR` 24
  pokeByte (p `plusPtr` 5) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 6) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 7) $ y

peek16LE p = do
  !x0 <- peekByte @Word16 p
  !x1 <- peekByte @Word16 (p `plusPtr` 1)
  return $ x1 `unsafeShiftL` 8
    .|. x0

peek16BE p = do
  !x0 <- peekByte @Word16 p
  !x1 <- peekByte @Word16 (p `plusPtr` 1)
  return $ x0 `unsafeShiftL` 8
    .|. x1

peek32LE p = do
  !x0 <- peekByte @Word32 p
  !x1 <- peekByte @Word32 (p `plusPtr` 1)
  !x2 <- peekByte @Word32 (p `plusPtr` 2)
  !x3 <- peekByte @Word32 (p `plusPtr` 3)
  return $ x3 `unsafeShiftL` 24
    .|. x2 `unsafeShiftL` 16
    .|. x1 `unsafeShiftL` 8
    .|. x0

peek32BE p = do
  !x0 <- peekByte @Word32 p
  !x1 <- peekByte @Word32 (p `plusPtr` 1)
  !x2 <- peekByte @Word32 (p `plusPtr` 2)
  !x3 <- peekByte @Word32 (p `plusPtr` 3)
  return $ x0 `unsafeShiftL` 24
    .|. x1 `unsafeShiftL` 16
    .|. x2 `unsafeShiftL` 8
    .|. x3

peek64LE p = do
  !x0 <- peekByte @Word64 p
  !x1 <- peekByte @Word64 (p `plusPtr` 1)
  !x2 <- peekByte @Word64 (p `plusPtr` 2)
  !x3 <- peekByte @Word64 (p `plusPtr` 3)
  !x4 <- peekByte @Word64 (p `plusPtr` 4)
  !x5 <- peekByte @Word64 (p `plusPtr` 5)
  !x6 <- peekByte @Word64 (p `plusPtr` 6)
  !x7 <- peekByte @Word64 (p `plusPtr` 7)
  return $ x7 `unsafeShiftL` 56
    .|. x6 `unsafeShiftL` 48
    .|. x5 `unsafeShiftL` 40
    .|. x4 `unsafeShiftL` 32
    .|. x3 `unsafeShiftL` 24
    .|. x2 `unsafeShiftL` 16
    .|. x1 `unsafeShiftL` 8
    .|. x0

peek64BE p = do
  !x0 <- peekByte @Word64 p
  !x1 <- peekByte @Word64 (p `plusPtr` 1)
  !x2 <- peekByte @Word64 (p `plusPtr` 2)
  !x3 <- peekByte @Word64 (p `plusPtr` 3)
  !x4 <- peekByte @Word64 (p `plusPtr` 4)
  !x5 <- peekByte @Word64 (p `plusPtr` 5)
  !x6 <- peekByte @Word64 (p `plusPtr` 6)
  !x7 <- peekByte @Word64 (p `plusPtr` 7)
  return $ x0 `unsafeShiftL` 56
    .|. x1 `unsafeShiftL` 48
    .|. x2 `unsafeShiftL` 40
    .|. x3 `unsafeShiftL` 32
    .|. x4 `unsafeShiftL` 24
    .|. x5 `unsafeShiftL` 16
    .|. x6 `unsafeShiftL` 8
    .|. x7

#else
fromLE16 :: Word16 -> Word16
fromLE32 :: Word32 -> Word32
fromLE64 :: Word64 -> Word64
{-# INLINE fromLE16 #-}
{-# INLINE fromLE32 #-}
{-# INLINE fromLE64 #-}

fromBE16 :: Word16 -> Word16
fromBE32 :: Word32 -> Word32
fromBE64 :: Word64 -> Word64
{-# INLINE fromBE16 #-}
{-# INLINE fromBE32 #-}
{-# INLINE fromBE64 #-}

toLE16 :: Word16 -> Word16
toLE32 :: Word32 -> Word32
toLE64 :: Word64 -> Word64
{-# INLINE toLE16 #-}
{-# INLINE toLE32 #-}
{-# INLINE toLE64 #-}

toBE16 :: Word16 -> Word16
toBE32 :: Word32 -> Word32
toBE64 :: Word64 -> Word64
{-# INLINE toBE16 #-}
{-# INLINE toBE32 #-}
{-# INLINE toBE64 #-}

#ifdef WORDS_BIGENDIAN
fromBE16 = id
fromBE32 = id
fromBE64 = id
toBE16 = id
toBE32 = id
toBE64 = id
fromLE16 = byteSwap16
fromLE32 = byteSwap32
fromLE64 = byteSwap64
toLE16 = byteSwap16
toLE32 = byteSwap32
toLE64 = byteSwap64
#else
fromLE16 = id
fromLE32 = id
fromLE64 = id
toLE16 = id
toLE32 = id
toLE64 = id
fromBE16 = byteSwap16
fromBE32 = byteSwap32
fromBE64 = byteSwap64
toBE16 = byteSwap16
toBE32 = byteSwap32
toBE64 = byteSwap64
#endif

poke16LE p = poke (castPtr @_ @Word16 p) . toLE16
poke32LE p = poke (castPtr @_ @Word32 p) . toLE32
poke64LE p = poke (castPtr @_ @Word64 p) . toLE64

poke16BE p = poke (castPtr @_ @Word16 p) . toBE16
poke32BE p = poke (castPtr @_ @Word32 p) . toBE32
poke64BE p = poke (castPtr @_ @Word64 p) . toBE64

peek16LE p = fromLE16 <$!> peek (castPtr @_ @Word16 p)
peek32LE p = fromLE32 <$!> peek (castPtr @_ @Word32 p)
peek64LE p = fromLE64 <$!> peek (castPtr @_ @Word64 p)

peek16BE p = fromBE16 <$!> peek (castPtr @_ @Word16 p)
peek32BE p = fromBE32 <$!> peek (castPtr @_ @Word32 p)
peek64BE p = fromBE64 <$!> peek (castPtr @_ @Word64 p)
#endif

newtype BigEndian a = BigEndian { unBE :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

newtype LittleEndian a = LittleEndian { unLE :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

class Persist s t where
  -- | Encode a value in the Put monad.
  put :: t -> Put s ()
  -- | Decode a value in the Get monad
  get :: Get s t

  default put :: (Generic t, GPersistPut s (Rep t)) => t -> Put s ()
  put = gput . from

  default get :: (Generic t, GPersistGet s (Rep t)) => Get s t
  get = to <$!> gget

-- | Encode a value using binary serialization to a strict ByteString.
encode :: forall s a. Persist s a => PutState s -> a -> ByteString
encode s x = runPut (put @s x) s

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.
decode :: forall s a. Persist s a => GetState s -> ByteString -> Either String a
decode = runGet (get @s)

putLE :: Persist s (LittleEndian a) => a -> Put s ()
putLE = put . LittleEndian
{-# INLINE putLE #-}

putBE :: Persist s (BigEndian a) => a -> Put s ()
putBE = put . BigEndian
{-# INLINE putBE #-}

getLE :: Persist s (LittleEndian a) => Get s a
getLE = unLE <$!> get
{-# INLINE getLE #-}

getBE :: Persist s (BigEndian a) => Get s a
getBE = unBE <$!> get
{-# INLINE getBE #-}

unsafePutByte :: Integral a => a -> Put s ()
unsafePutByte x = Put $ \_ p -> do
  poke p $ fromIntegral x
  pure $! p `plusPtr` 1 :!: ()
{-# INLINE unsafePutByte #-}

unsafePut16LE :: Integral a => a -> Put s ()
unsafePut16LE x = Put $ \_ p -> do
  poke16LE p $ fromIntegral x
  pure $! p `plusPtr` 2 :!: ()
{-# INLINE unsafePut16LE #-}

unsafePut32LE :: Integral a => a -> Put s ()
unsafePut32LE x = Put $ \_ p -> do
  poke32LE p $ fromIntegral x
  pure $! p `plusPtr` 4 :!: ()
{-# INLINE unsafePut32LE #-}

unsafePut64LE :: Integral a => a -> Put s ()
unsafePut64LE x = Put $ \_ p -> do
  poke64LE p $ fromIntegral x
  pure $! p `plusPtr` 8 :!: ()
{-# INLINE unsafePut64LE #-}

unsafePut16BE :: Integral a => a -> Put s ()
unsafePut16BE x = Put $ \_ p -> do
  poke16BE p $ fromIntegral x
  pure $! p `plusPtr` 2 :!: ()
{-# INLINE unsafePut16BE #-}

unsafePut32BE :: Integral a => a -> Put s ()
unsafePut32BE x = Put $ \_ p -> do
  poke32BE p $ fromIntegral x
  pure $! p `plusPtr` 4 :!: ()
{-# INLINE unsafePut32BE #-}

unsafePut64BE :: Integral a => a -> Put s ()
unsafePut64BE x = Put $ \_ p -> do
  poke64BE p $ fromIntegral x
  pure $! p `plusPtr` 8 :!: ()
{-# INLINE unsafePut64BE #-}

unsafeGetByte :: Num a => Get s a
unsafeGetByte = Get $ \_ p -> do
  x <- peek p
  pure $! p `plusPtr` 1 :!: fromIntegral x
{-# INLINE unsafeGetByte #-}

unsafeGet16LE :: Num a => Get s a
unsafeGet16LE = Get $ \_ p -> do
  x <- peek16LE p
  pure $! p `plusPtr` 2 :!: fromIntegral x
{-# INLINE unsafeGet16LE #-}

unsafeGet32LE :: Num a => Get s a
unsafeGet32LE = Get $ \_ p -> do
  x <- peek32LE p
  pure $! p `plusPtr` 4 :!: fromIntegral x
{-# INLINE unsafeGet32LE #-}

unsafeGet64LE :: Num a => Get s a
unsafeGet64LE = Get $ \_ p -> do
  x <- peek64LE p
  pure $! p `plusPtr` 8 :!: fromIntegral x
{-# INLINE unsafeGet64LE #-}

unsafeGet16BE :: Num a => Get s a
unsafeGet16BE = Get $ \_ p -> do
  x <- peek16BE p
  pure $! p `plusPtr` 2 :!: fromIntegral x
{-# INLINE unsafeGet16BE #-}

unsafeGet32BE :: Num a => Get s a
unsafeGet32BE = Get $ \_ p -> do
  x <- peek32BE p
  pure $! p `plusPtr` 4 :!: fromIntegral x
{-# INLINE unsafeGet32BE #-}

unsafeGet64BE :: Num a => Get s a
unsafeGet64BE = Get $ \_ p -> do
  x <- peek64BE p
  pure $! p `plusPtr` 8 :!: fromIntegral x
{-# INLINE unsafeGet64BE #-}

reinterpretCast :: (Storable a, Storable b) => Ptr p -> a -> IO b
reinterpretCast p x = do
  poke (castPtr p) x
  peek (castPtr p)
{-# INLINE reinterpretCast #-}

reinterpretCastPut :: (Storable a, Storable b) => a -> Put s b
reinterpretCastPut x = Put $ \e p -> (p :!:) <$!> reinterpretCast (peTmp e) x
{-# INLINE reinterpretCastPut #-}

reinterpretCastGet :: (Storable a, Storable b) => a -> Get s b
reinterpretCastGet x = Get $ \e p -> (p :!:) <$!> reinterpretCast (geTmp e) x
{-# INLINE reinterpretCastGet #-}

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Persist s () where
  put () = pure ()
  {-# INLINE put #-}
  get = pure ()
  {-# INLINE get #-}

instance Persist s Word8 where
  put x = do
    grow 1
    unsafePutByte x
  {-# INLINE put #-}

  get = do
    ensure 1
    unsafeGetByte
  {-# INLINE get #-}

instance Persist s (LittleEndian Word16) where
  put x = do
    grow 2
    unsafePut16LE $ unLE x
  {-# INLINE put #-}

  get = do
    ensure 2
    LittleEndian <$!> unsafeGet16LE
  {-# INLINE get #-}

instance Persist s (BigEndian Word16) where
  put x = do
    grow 2
    unsafePut16BE $ unBE x
  {-# INLINE put #-}

  get = do
    ensure 2
    BigEndian <$!> unsafeGet16BE
  {-# INLINE get #-}

instance Persist s Word16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Word32) where
  put x = do
    grow 4
    unsafePut32LE $ unLE x
  {-# INLINE put #-}

  get = do
    ensure 4
    LittleEndian <$!> unsafeGet32LE
  {-# INLINE get #-}

instance Persist s (BigEndian Word32) where
  put x = do
    grow 4
    unsafePut32BE $ unBE x
  {-# INLINE put #-}

  get = do
    ensure 4
    BigEndian <$!> unsafeGet32BE
  {-# INLINE get #-}

instance Persist s Word32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Word64) where
  put x = do
    grow 8
    unsafePut64LE $ unLE x
  {-# INLINE put #-}

  get = do
    ensure 8
    LittleEndian <$!> unsafeGet64LE
  {-# INLINE get #-}

instance Persist s (BigEndian Word64) where
  put x = do
    grow 8
    unsafePut64BE $ unBE x
  {-# INLINE put #-}

  get = do
    ensure 8
    BigEndian <$!> unsafeGet64BE
  {-# INLINE get #-}

instance Persist s Word64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s Int8 where
  put = put @s @Word8 . fromIntegral
  {-# INLINE put #-}
  get = fromIntegral <$!> get @s @Word8
  {-# INLINE get #-}

instance Persist s (LittleEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$!> get
  {-# INLINE get #-}

instance Persist s (BigEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$!> get
  {-# INLINE get #-}

instance Persist s Int16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$!> get
  {-# INLINE get #-}

instance Persist s (BigEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$!> get
  {-# INLINE get #-}

instance Persist s Int32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$!> get
  {-# INLINE get #-}

instance Persist s (BigEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$!> get
  {-# INLINE get #-}

instance Persist s Int64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Double) where
  put x = reinterpretCastPut (unLE x) >>= putLE @s @Word64
  {-# INLINE put #-}
  get = getLE @s @Word64 >>= fmap LittleEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist s (BigEndian Double) where
  put x = reinterpretCastPut (unBE x) >>= putBE @s @Word64
  {-# INLINE put #-}
  get = getBE @s @Word64 >>= fmap BigEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist s Double where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Float) where
  put x = reinterpretCastPut (unLE x) >>= putLE @s @Word32
  {-# INLINE put #-}
  get = getLE @s @Word32 >>= fmap LittleEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist s (BigEndian Float) where
  put x = reinterpretCastPut (unBE x) >>= putBE @s @Word32
  {-# INLINE put #-}
  get = getBE @s @Word32 >>= fmap BigEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist s Float where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$!> get
  {-# INLINE get #-}

instance Persist s (BigEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$!> get
  {-# INLINE get #-}

instance Persist s Word where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s (LittleEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$!> get
  {-# INLINE get #-}

instance Persist s (BigEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$!> get
  {-# INLINE get #-}

instance Persist s Int where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist s Integer where
  put n = do
    put $ n < 0
    put $ unroll $ abs n

  get = do
    neg <- get
    val <- roll <$!> get
    pure $! if neg then negate val else val

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where step 0 = Nothing
        step i = Just (fromIntegral i, i `unsafeShiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where unstep b a = a `unsafeShiftL` 8 .|. fromIntegral b

instance Persist s a => Persist s (Ratio a) where
  put (n :% d) = put n *> put d
  {-# INLINE put #-}

  get = (:%) <$!> get <*> get
  {-# INLINE get #-}

instance Persist s Natural where
  put = put . unroll
  get = roll <$!> get

-- Char is serialized as UTF-8
instance Persist s Char where
  put a | c <= 0x7f     = put (fromIntegral c :: Word8)
        | c <= 0x7ff    = do put (0xc0 .|. y)
                             put (0x80 .|. z)
        | c <= 0xffff   = do put (0xe0 .|. x)
                             put (0x80 .|. y)
                             put (0x80 .|. z)
        | c <= 0x10ffff = do put (0xf0 .|. w)
                             put (0x80 .|. x)
                             put (0x80 .|. y)
                             put (0x80 .|. z)
        | otherwise = error "Not a valid Unicode code point"
    where
      c = ord a
      z, y, x, w :: Word8
      z = fromIntegral (c                 .&. 0x3f)
      y = fromIntegral (unsafeShiftR c 6  .&. 0x3f)
      x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
      w = fromIntegral (unsafeShiftR c 18 .&. 0x7)
  {-# INLINE put #-}

  get = do
    let byte = fromIntegral <$!> get @s @Word8
        shiftL6 = flip unsafeShiftL 6
    w <- byte
    r <- if | w < 0x80  -> pure w
            | w < 0xe0  -> do
                x <- xor 0x80 <$!> byte
                pure $ x .|. shiftL6 (xor 0xc0 w)
            | w < 0xf0  -> do
                x <- xor 0x80 <$!> byte
                y <- xor 0x80 <$!> byte
                pure $ y .|. shiftL6 (x .|. shiftL6
                                       (xor 0xe0 w))
            | otherwise -> do
                x <- xor 0x80 <$!> byte
                y <- xor 0x80 <$!> byte
                z <- xor 0x80 <$!> byte
                pure $ z .|. shiftL6 (y .|. shiftL6
                                       (x .|. shiftL6 (xor 0xf0 w)))
    if r <= 0x10FFFF then
      pure $ unsafeChr r
    else
      failGet CharException "Invalid character"
  {-# INLINE get #-}

instance Persist s Text where
  put = put . TE.encodeUtf8
  {-# INLINE put #-}
  get = do
    n <- get
    TE.decodeUtf8 <$!> getBytes n
  {-# INLINE get #-}

instance Persist s Bool
instance Persist s Ordering
instance Persist s a => Persist s (Maybe a)
instance Persist s e => Persist s (T.Tree e)
instance (Persist s a, Persist s b) => Persist s (Either a b)
instance (Persist s a, Persist s b) => Persist s (a,b)
instance (Persist s a, Persist s b, Persist s c) => Persist s (a,b,c)
instance (Persist s a, Persist s b, Persist s c, Persist s d)
        => Persist s (a,b,c,d)
instance (Persist s a, Persist s b, Persist s c, Persist s d, Persist s e)
        => Persist s (a,b,c,d,e)
instance (Persist s a, Persist s b, Persist s c, Persist s d, Persist s e
         , Persist s f)
        => Persist s (a,b,c,d,e,f)
instance (Persist s a, Persist s b, Persist s c, Persist s d, Persist s e
         , Persist s f, Persist s h)
        => Persist s (a,b,c,d,e,f,h)
instance Persist s a => Persist s (M.Dual a)
instance Persist s M.All
instance Persist s M.Any
instance Persist s a => Persist s (M.Sum a)
instance Persist s a => Persist s (M.Product a)
instance Persist s a => Persist s (M.First a)
instance Persist s a => Persist s (M.Last a)

-- | Persist a list in the following format:
--   Word64 (little endian format)
--   element 1
--   ...
--   element n
instance Persist s a => Persist s [a] where
    put l = do
      put $ length l
      mapM_ put l
    {-# INLINE put #-}

    get = go [] =<< get @s @Word64
      where go as 0 = pure $! reverse as
            go as i = do x <- get
                         x `seq` go (x:as) (i - 1)
    {-# INLINE get #-}

instance Persist s ByteString where
  put s = do
    put $ B.length s
    putByteString s
  get = get >>= getByteString

instance Persist s L.ByteString where
  put = put . L.toStrict
  get = L.fromStrict <$!> get

instance Persist s S.ShortByteString where
  put s = do
    let n = S.length s
    put n
    grow n
    Put $ \_ p -> do
      S.copyToPtr s 0 p n
      pure $! p `plusPtr` n :!: ()

  get = S.toShort <$!> get

instance (Ord a, Persist s a) => Persist s (Set a) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance (Ord k, Persist s k, Persist s e) => Persist s (Map k e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance Persist s IntSet where
  put = put . toList
  get = fromList <$!> get

instance Persist s e => Persist s (NonEmpty e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance Persist s e => Persist s (IntMap e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance Persist s e => Persist s (Seq e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

type family SumArity (a :: * -> *) :: Nat where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

class GPersistPut s f where
  gput :: f a -> Put s ()

class GPersistGet s f where
  gget :: Get s (f a)

instance GPersistPut s f => GPersistPut s (M1 i c f) where
  gput = gput . unM1
  {-# INLINE gput #-}

instance GPersistGet s f => GPersistGet s (M1 i c f) where
  gget = fmap M1 gget
  {-# INLINE gget #-}

instance Persist s a => GPersistPut s (K1 i a) where
  gput = put . unK1
  {-# INLINE gput #-}

instance Persist s a => GPersistGet s (K1 i a) where
  gget = fmap K1 get
  {-# INLINE gget #-}

instance GPersistPut s U1 where
  gput _ = pure ()
  {-# INLINE gput #-}

instance GPersistGet s U1 where
  gget = pure U1
  {-# INLINE gget #-}

instance GPersistPut s V1 where
  gput x = case x of {}
  {-# INLINE gput #-}

instance GPersistGet s V1 where
  gget = undefined
  {-# INLINE gget #-}

instance (GPersistPut s a, GPersistPut s b) => GPersistPut s (a :*: b) where
  gput (a :*: b) = gput a *> gput b
  {-# INLINE gput #-}

instance (GPersistGet s a, GPersistGet s b) => GPersistGet s (a :*: b) where
  gget = (:*:) <$!> gget <*> gget
  {-# INLINE gget #-}

instance (SumArity (a :+: b) <= 255, GPersistPutSum s 0 (a :+: b)) => GPersistPut s (a :+: b) where
  gput x = gputSum x (Proxy :: Proxy 0)
  {-# INLINE gput #-}

instance (SumArity (a :+: b) <= 255, GPersistGetSum s 0 (a :+: b)) => GPersistGet s (a :+: b) where
  gget = do
    tag <- get
    ggetSum tag (Proxy :: Proxy 0)
  {-# INLINE gget #-}

class KnownNat n => GPersistPutSum s (n :: Nat) (f :: * -> *) where
  gputSum :: f p -> Proxy n -> Put s ()

class KnownNat n => GPersistGetSum s (n :: Nat) (f :: * -> *) where
  ggetSum :: Word8 -> Proxy n -> Get s (f p)

instance (GPersistPutSum s n a, GPersistPutSum s (n + SumArity a) b, KnownNat n)
         => GPersistPutSum s n (a :+: b) where
  gputSum (L1 l) _ = gputSum l (Proxy :: Proxy n)
  gputSum (R1 r) _ = gputSum r (Proxy :: Proxy (n + SumArity a))
  {-# INLINE gputSum #-}

instance (GPersistGetSum s n a, GPersistGetSum s (n + SumArity a) b, KnownNat n)
         => GPersistGetSum s n (a :+: b) where
  ggetSum tag proxyL
    | tag < sizeL = L1 <$!> ggetSum tag proxyL
    | otherwise = R1 <$!> ggetSum tag (Proxy :: Proxy (n + SumArity a))
    where
      sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
  {-# INLINE ggetSum #-}

instance (GPersistPut s a, KnownNat n) => GPersistPutSum s n (C1 c a) where
  gputSum x _ = do
    put (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
    gput x
  {-# INLINE gputSum #-}

instance (GPersistGet s a, KnownNat n) => GPersistGetSum s n (C1 c a) where
  ggetSum tag _
    | tag == cur = gget
    | tag > cur = fail "Sum tag invalid"
    | otherwise = fail "Implementation error"
    where
      cur = fromInteger (natVal (Proxy :: Proxy n))
  {-# INLINE ggetSum #-}

-- | Ensure that @n@ bytes are available. Fails if fewer than @n@ bytes are available.
ensure :: Int -> Get s ()
ensure n
  | n < 0 = failGet LengthException "ensure: negative length"
  | otherwise = do
      m <- remaining
      when (m < n) $ failGet LengthException "Not enough bytes available"
{-# INLINE ensure #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get s ()
skip n = do
  ensure n
  Get $ \_ p -> pure $! p `plusPtr` n :!: ()
{-# INLINE skip #-}

-- | Get the number of remaining unparsed bytes.  Useful for checking whether
-- all input has been consumed.
remaining :: Get s Int
remaining = Get $ \e p -> pure $! p :!: geEnd e `minusPtr` p
{-# INLINE remaining #-}

-- -- | Succeed if end of input reached.
eof :: Get s ()
eof = do
  n <- remaining
  when (n /= 0) $ failGet EOFException "Expected end of file"
{-# INLINE eof #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get s ByteString
getBytes n = do
  ensure n
  Get $ \e p -> pure $! p `plusPtr` n :!: B.PS (geBuf e) (p `minusPtr` geBegin e) n
{-# INLINE getBytes #-}

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getByteString :: Int -> Get s ByteString
getByteString n = B.copy <$!> getBytes n
{-# INLINE getByteString #-}

runPut :: Put s a -> PutState s -> ByteString
runPut s = snd . evalPut s
{-# INLINE runPut #-}

putByteString :: ByteString -> Put s ()
putByteString (B.PS b o n) = do
  grow n
  Put $ \_ p -> do
    withForeignPtr b $ \q -> B.memcpy p (q `plusPtr` o) n
    pure $! p `plusPtr` n :!: ()
{-# INLINE putByteString #-}

modifyStateGet :: (GetState s -> GetState s) -> Get s ()
modifyStateGet f = do
  s <- stateGet
  setStateGet (f s)
{-# INLINE modifyStateGet #-}

modifyStatePut :: (PutState s -> PutState s) -> Put s ()
modifyStatePut f = do
  s <- statePut
  setStatePut (f s)
{-# INLINE modifyStatePut #-}
