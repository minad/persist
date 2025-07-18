{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Persist
  ( -- * The Persist class
    Persist (..)

    -- * Endianness
  , HostEndian
  , BigEndian (..)
  , LittleEndian (..)

    -- * Helpers for writing Persist instances
  , HasEndianness (..)
  , ReinterpretAs (..)
  , SerializeAs (..)
  , ViaReinterpretAs (..)
  , ViaSerializeAs (..)

    -- * Serialization
  , encode
  , encodeLazy
  , decode

    -- * The Get type
  , Get
  , runGet
  , ensure
  , skip
  , getBytes
  , getByteString
  , remaining
  , eof
  , getPrefix
  , getHE
  , getLE
  , getBE

    -- * The Put type
  , Put
  , runPut
  , runPutLazy
  , evalPut
  , evalPutLazy
  , grow
  , putByteString
  , putHE
  , putLE
  , putBE

    -- * Size Reserve/Resolve
  , reserveSize
  , resolveSizeExclusiveBE
  , resolveSizeExclusiveLE
  , resolveSizeInclusiveBE
  , resolveSizeInclusiveLE
  , resolveSizeLE
  , resolveSizeBE
  ) where

import Control.Exception (throw)
import Control.Monad (forM_, when, (<$!>))
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind (Constraint, Type)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Persist.Internal
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
#ifdef UNALIGNED_MEMORY
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
#else
import Data.Word (Word8, Word16, Word32, Word64)
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Short.Internal as S
import qualified Data.Monoid as M
import qualified Data.Text.Encoding as TE
import qualified Data.Tree as T
import Foreign (Ptr, Storable (..), castPtr, minusPtr, plusPtr, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import GHC.Base (ord, unsafeChr)
import GHC.Exts (IsList (..))
import GHC.Generics
import GHC.Real (Ratio (..))
import GHC.TypeLits
  ( ErrorMessage (..)
  , KnownNat
  , Nat
  , Natural
  , Symbol
  , TypeError
  , natVal
  , type (+)
  , type (<=?)
  )

#include "MachDeps.h"

putHE :: (Persist (HostEndian a)) => a -> Put ()
getHE :: (Persist (HostEndian a)) => Get a
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
  pokeByte p y
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 8

poke16BE p y = do
  pokeByte p $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 1) y

poke32LE p y = do
  pokeByte p y
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 3) $ y `unsafeShiftR` 24

poke32BE p y = do
  pokeByte p $ y `unsafeShiftR` 24
  pokeByte (p `plusPtr` 1) $ y `unsafeShiftR` 16
  pokeByte (p `plusPtr` 2) $ y `unsafeShiftR` 8
  pokeByte (p `plusPtr` 3) y

poke64LE p y = do
  pokeByte p y
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
  pokeByte (p `plusPtr` 7) y

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

newtype BigEndian a = BigEndian {unBE :: a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

newtype LittleEndian a = LittleEndian {unLE :: a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

class Persist t where
  -- | Encode a value in the Put monad.
  put :: t -> Put ()

  -- | Decode a value in the Get monad
  get :: Get t

  -- | Encode a value without checking for sufficient size.
  unsafePut :: t -> Put ()
  unsafePut = put

  -- | Decode a value without checking for sufficient size.
  unsafeGet :: Get t
  unsafeGet = get

  default put :: (Generic t, GPersistPut (Rep t)) => t -> Put ()
  put = gput . from

  default get :: (Generic t, GPersistGet (Rep t)) => Get t
  get = to <$!> gget

-- | Encode a value using binary serialization to a strict ByteString.
encode :: (Persist a) => a -> ByteString
encode = runPut . put

-- | Encode a value using binary serialization to a lazy ByteString.
encodeLazy :: (Persist a) => a -> L.ByteString
encodeLazy = runPutLazy . put


{- | Decode a value from a strict ByteString, reconstructing the original
structure.
-}
decode :: (Persist a) => ByteString -> Either String a
decode = runGet get

putLE :: (Persist (LittleEndian a)) => a -> Put ()
putLE = put . LittleEndian
{-# INLINE putLE #-}

putBE :: (Persist (BigEndian a)) => a -> Put ()
putBE = put . BigEndian
{-# INLINE putBE #-}

getLE :: forall a. (Persist (LittleEndian a)) => Get a
getLE = (.unLE) <$!> get @(LittleEndian a)
{-# INLINE getLE #-}

getBE :: forall a. (Persist (BigEndian a)) => Get a
getBE = (.unBE) <$!> get @(BigEndian a)
{-# INLINE getBE #-}

unsafePutByte :: (Integral a) => a -> Put ()
unsafePutByte x = Put $ \_ p -> do
  poke p $ fromIntegral x
  pure $! p `plusPtr` 1 :!: ()
{-# INLINE unsafePutByte #-}

unsafePut16LE :: (Integral a) => a -> Put ()
unsafePut16LE x = Put $ \_ p -> do
  poke16LE p $ fromIntegral x
  pure $! p `plusPtr` 2 :!: ()
{-# INLINE unsafePut16LE #-}

unsafePut32LE :: (Integral a) => a -> Put ()
unsafePut32LE x = Put $ \_ p -> do
  poke32LE p $ fromIntegral x
  pure $! p `plusPtr` 4 :!: ()
{-# INLINE unsafePut32LE #-}

unsafePut64LE :: (Integral a) => a -> Put ()
unsafePut64LE x = Put $ \_ p -> do
  poke64LE p $ fromIntegral x
  pure $! p `plusPtr` 8 :!: ()
{-# INLINE unsafePut64LE #-}

unsafePut16BE :: (Integral a) => a -> Put ()
unsafePut16BE x = Put $ \_ p -> do
  poke16BE p $ fromIntegral x
  pure $! p `plusPtr` 2 :!: ()
{-# INLINE unsafePut16BE #-}

unsafePut32BE :: (Integral a) => a -> Put ()
unsafePut32BE x = Put $ \_ p -> do
  poke32BE p $ fromIntegral x
  pure $! p `plusPtr` 4 :!: ()
{-# INLINE unsafePut32BE #-}

unsafePut64BE :: (Integral a) => a -> Put ()
unsafePut64BE x = Put $ \_ p -> do
  poke64BE p $ fromIntegral x
  pure $! p `plusPtr` 8 :!: ()
{-# INLINE unsafePut64BE #-}

unsafeGetByte :: (Num a) => Get a
unsafeGetByte = Get $ \_ p -> do
  x <- peek p
  pure $! p `plusPtr` 1 :!: fromIntegral x
{-# INLINE unsafeGetByte #-}

unsafeGet16LE :: (Num a) => Get a
unsafeGet16LE = Get $ \_ p -> do
  x <- peek16LE p
  pure $! p `plusPtr` 2 :!: fromIntegral x
{-# INLINE unsafeGet16LE #-}

unsafeGet32LE :: (Num a) => Get a
unsafeGet32LE = Get $ \_ p -> do
  x <- peek32LE p
  pure $! p `plusPtr` 4 :!: fromIntegral x
{-# INLINE unsafeGet32LE #-}

unsafeGet64LE :: (Num a) => Get a
unsafeGet64LE = Get $ \_ p -> do
  x <- peek64LE p
  pure $! p `plusPtr` 8 :!: fromIntegral x
{-# INLINE unsafeGet64LE #-}

unsafeGet16BE :: (Num a) => Get a
unsafeGet16BE = Get $ \_ p -> do
  x <- peek16BE p
  pure $! p `plusPtr` 2 :!: fromIntegral x
{-# INLINE unsafeGet16BE #-}

unsafeGet32BE :: (Num a) => Get a
unsafeGet32BE = Get $ \_ p -> do
  x <- peek32BE p
  pure $! p `plusPtr` 4 :!: fromIntegral x
{-# INLINE unsafeGet32BE #-}

unsafeGet64BE :: (Num a) => Get a
unsafeGet64BE = Get $ \_ p -> do
  x <- peek64BE p
  pure $! p `plusPtr` 8 :!: fromIntegral x
{-# INLINE unsafeGet64BE #-}

reinterpretCast :: (Storable a, Storable b) => Ptr p -> a -> IO b
reinterpretCast p x = do
  poke (castPtr p) x
  peek (castPtr p)
{-# INLINE reinterpretCast #-}

reinterpretCastPut :: (Storable a, Storable b) => a -> Put b
reinterpretCastPut x = Put $ \e p -> (p :!:) <$!> reinterpretCast e.tmp x
{-# INLINE reinterpretCastPut #-}

reinterpretCastGet :: (Storable a, Storable b) => a -> Get b
reinterpretCastGet x = Get $ \e p -> (p :!:) <$!> reinterpretCast e.tmp x
{-# INLINE reinterpretCastGet #-}

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Persist () where
  put () = pure ()
  {-# INLINE put #-}
  get = pure ()
  {-# INLINE get #-}

class HasEndianness a where
  unsafeGetLE :: Get a
  unsafePutLE :: a -> Put ()
  unsafeGetBE :: Get a
  unsafePutBE :: a -> Put ()
  endiannessSize :: Int

instance HasEndianness Word8 where
  unsafeGetLE = unsafeGetByte
  unsafeGetBE = unsafeGetByte
  unsafePutLE = unsafePutByte
  unsafePutBE = unsafePutByte
  endiannessSize = 1

instance HasEndianness Word16 where
  unsafeGetLE = unsafeGet16LE
  {-# INLINE unsafeGetLE #-}
  unsafeGetBE = unsafeGet16BE
  {-# INLINE unsafeGetBE #-}
  unsafePutLE = unsafePut16LE
  {-# INLINE unsafePutLE #-}
  unsafePutBE = unsafePut16BE
  {-# INLINE unsafePutBE #-}
  endiannessSize = 2
  {-# INLINE endiannessSize #-}

instance HasEndianness Word32 where
  unsafeGetLE = unsafeGet32LE
  {-# INLINE unsafeGetLE #-}
  unsafeGetBE = unsafeGet32BE
  {-# INLINE unsafeGetBE #-}
  unsafePutLE = unsafePut32LE
  {-# INLINE unsafePutLE #-}
  unsafePutBE = unsafePut32BE
  {-# INLINE unsafePutBE #-}
  endiannessSize = 4
  {-# INLINE endiannessSize #-}

instance HasEndianness Word64 where
  unsafeGetLE = unsafeGet64LE
  {-# INLINE unsafeGetLE #-}
  unsafeGetBE = unsafeGet64BE
  {-# INLINE unsafeGetBE #-}
  unsafePutLE = unsafePut64LE
  {-# INLINE unsafePutLE #-}
  unsafePutBE = unsafePut64BE
  {-# INLINE unsafePutBE #-}
  endiannessSize = 8
  {-# INLINE endiannessSize #-}

instance (HasEndianness a) => Persist (LittleEndian a) where
  put x = do
    grow (endiannessSize @a)
    unsafePut x
  {-# INLINE put #-}

  unsafePut = unsafePutLE . (.unLE)
  {-# INLINE unsafePut #-}

  get = do
    ensure (endiannessSize @a)
    unsafeGet
  {-# INLINE get #-}

  unsafeGet = LittleEndian <$!> unsafeGetLE
  {-# INLINE unsafeGet #-}

instance (HasEndianness a) => Persist (BigEndian a) where
  put x = do
    grow (endiannessSize @a)
    unsafePut x
  {-# INLINE put #-}

  unsafePut = unsafePutBE . (.unBE)
  {-# INLINE unsafePut #-}

  get = do
    ensure (endiannessSize @a)
    unsafeGet
  {-# INLINE get #-}

  unsafeGet = BigEndian <$!> unsafeGetBE
  {-# INLINE unsafeGet #-}

deriving via (LittleEndian Word8) instance Persist Word8
deriving via (LittleEndian Word16) instance Persist Word16
deriving via (LittleEndian Word32) instance Persist Word32
deriving via (LittleEndian Word64) instance Persist Word64

class SerializeAs a where
  type SerializeTarget a
  castPut :: a -> SerializeTarget a
  castGet :: SerializeTarget a -> a

instance SerializeAs Int8 where
  type SerializeTarget Int8 = Word8
  castPut = fromIntegral
  castGet = fromIntegral

instance SerializeAs Int16 where
  type SerializeTarget Int16 = Word16
  castPut = fromIntegral
  castGet = fromIntegral

instance SerializeAs Int32 where
  type SerializeTarget Int32 = Word32
  castPut = fromIntegral
  castGet = fromIntegral

instance SerializeAs Int64 where
  type SerializeTarget Int64 = Word64
  castPut = fromIntegral
  castGet = fromIntegral

newtype ViaSerializeAs a = MkViaSerializeAs a
  deriving (Eq, Ord, Show)

instance (SerializeAs a, HasEndianness (SerializeTarget a)) => HasEndianness (ViaSerializeAs a) where
  endiannessSize = endiannessSize @(SerializeTarget a)
  {-# INLINE endiannessSize #-}
  unsafeGetBE = MkViaSerializeAs . castGet <$!> unsafeGetBE
  {-# INLINE unsafeGetBE #-}
  unsafeGetLE = MkViaSerializeAs . castGet <$!> unsafeGetLE
  {-# INLINE unsafeGetLE #-}
  unsafePutBE (MkViaSerializeAs x) = unsafePutBE . castPut $! x
  {-# INLINE unsafePutBE #-}
  unsafePutLE (MkViaSerializeAs x) = unsafePutLE . castPut $! x
  {-# INLINE unsafePutLE #-}

deriving via (ViaSerializeAs Int8) instance HasEndianness Int8
deriving via (ViaSerializeAs Int16) instance HasEndianness Int16
deriving via (ViaSerializeAs Int32) instance HasEndianness Int32
deriving via (ViaSerializeAs Int64) instance HasEndianness Int64

deriving via (LittleEndian Int8) instance Persist Int8
deriving via (LittleEndian Int16) instance Persist Int16
deriving via (LittleEndian Int32) instance Persist Int32
deriving via (LittleEndian Int64) instance Persist Int64

instance SerializeAs Word where
  type SerializeTarget Word = Word64
  castPut = fromIntegral
  castGet = fromIntegral

instance SerializeAs Int where
  type SerializeTarget Int = Int64
  castPut = fromIntegral
  castGet = fromIntegral

instance (SerializeAs a, Persist (SerializeTarget a)) => Persist (ViaSerializeAs a) where
  put (MkViaSerializeAs x) = put (castPut x)
  {-# INLINE put #-}
  unsafePut (MkViaSerializeAs x) = unsafePut (castPut x)
  {-# INLINE unsafePut #-}
  get = MkViaSerializeAs . castGet <$> get
  {-# INLINE get #-}
  unsafeGet = MkViaSerializeAs . castGet <$> unsafeGet
  {-# INLINE unsafeGet #-}

deriving via (ViaSerializeAs Word) instance HasEndianness Word
deriving via (ViaSerializeAs Int) instance HasEndianness Int
deriving via (ViaSerializeAs Word) instance Persist Word
deriving via (ViaSerializeAs Int) instance Persist Int

class ReinterpretAs a where
  type ReinterpretTarget a

newtype ViaReinterpretAs a = MkViaReinterpretAs a
  deriving (Eq, Ord, Show)

instance
  forall a b.
  (ReinterpretAs a, Storable a, Storable b, HasEndianness b, b ~ ReinterpretTarget a) =>
  HasEndianness (ViaReinterpretAs a)
  where
  unsafePutLE (MkViaReinterpretAs x) = reinterpretCastPut x >>= unsafePutLE @b
  {-# INLINE unsafePutLE #-}
  unsafePutBE (MkViaReinterpretAs x) = reinterpretCastPut x >>= unsafePutBE @b
  {-# INLINE unsafePutBE #-}
  unsafeGetLE = MkViaReinterpretAs <$> (unsafeGetLE @(ReinterpretTarget a) >>= reinterpretCastGet)
  {-# INLINE unsafeGetLE #-}
  unsafeGetBE = MkViaReinterpretAs <$> (unsafeGetBE @(ReinterpretTarget a) >>= reinterpretCastGet)
  {-# INLINE unsafeGetBE #-}
  endiannessSize = endiannessSize @b

instance ReinterpretAs Double where
  type ReinterpretTarget Double = Word64

instance ReinterpretAs Float where
  type ReinterpretTarget Float = Word32

deriving via (ViaReinterpretAs Double) instance HasEndianness Double
deriving via (ViaReinterpretAs Float) instance HasEndianness Float

deriving via (LittleEndian Double) instance Persist Double
deriving via (LittleEndian Float) instance Persist Float

instance Persist Integer where
  put n = do
    put $ n < 0
    put $ unroll $ abs n

  get = do
    neg <- get
    val <- roll <$!> get
    pure $! if neg then negate val else val

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
 where
  step 0 = Nothing
  step i = Just (fromIntegral i, i `unsafeShiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
 where
  unstep b a = a `unsafeShiftL` 8 .|. fromIntegral b

instance (Persist a) => Persist (Ratio a) where
  put (n :% d) = put n *> put d
  {-# INLINE put #-}

  get = (:%) <$!> get <*> get
  {-# INLINE get #-}

instance Persist Natural where
  put = put . unroll
  get = roll <$!> get

-- Char is serialized as UTF-8
instance Persist Char where
  put a
    | c <= 0x7f = grow 1 >> unsafePut a
    | c <= 0x7ff = grow 2 >> unsafePut a
    | c <= 0xffff = grow 3 >> unsafePut a
    | c <= 0x10ffff = grow 4 >> unsafePut a
    | otherwise = error "Not a valid Unicode code point"
   where
    c = ord a
  {-# INLINE put #-}

  unsafePut a
    | c <= 0x7f = unsafePut (fromIntegral c :: Word8)
    | c <= 0x7ff = do
        unsafePut (0xc0 .|. y)
        unsafePut (0x80 .|. z)
    | c <= 0xffff = do
        unsafePut (0xe0 .|. x)
        unsafePut (0x80 .|. y)
        unsafePut (0x80 .|. z)
    | c <= 0x10ffff = do
        unsafePut (0xf0 .|. w)
        unsafePut (0x80 .|. x)
        unsafePut (0x80 .|. y)
        unsafePut (0x80 .|. z)
    | otherwise = error "Not a valid Unicode code point"
   where
    c = ord a
    z, y, x, w :: Word8
    z = fromIntegral (c .&. 0x3f)
    y = fromIntegral (unsafeShiftR c 6 .&. 0x3f)
    x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
    w = fromIntegral (unsafeShiftR c 18 .&. 0x7)
  {-# INLINE unsafePut #-}

  get = do
    let byte = fromIntegral <$!> get @Word8
        shiftL6 = flip unsafeShiftL 6
    w <- byte
    r <-
      if
        | w < 0x80 -> pure w
        | w < 0xe0 -> do
            x <- xor 0x80 <$!> byte
            pure $ x .|. shiftL6 (xor 0xc0 w)
        | w < 0xf0 -> do
            x <- xor 0x80 <$!> byte
            y <- xor 0x80 <$!> byte
            pure $
              y
                .|. shiftL6
                  ( x
                      .|. shiftL6
                        (xor 0xe0 w)
                  )
        | otherwise -> do
            x <- xor 0x80 <$!> byte
            y <- xor 0x80 <$!> byte
            z <- xor 0x80 <$!> byte
            pure $
              z
                .|. shiftL6
                  ( y
                      .|. shiftL6
                        (x .|. shiftL6 (xor 0xf0 w))
                  )
    if r <= 0x10FFFF
      then
        pure $ unsafeChr r
      else
        failGet CharException "Invalid character"
  {-# INLINE get #-}

instance Persist Text where
  put = put . TE.encodeUtf8
  {-# INLINE put #-}
  get = do
    n <- get
    TE.decodeUtf8 <$!> getBytes n
  {-# INLINE get #-}

instance Persist Bool
instance Persist Ordering
instance (Persist a) => Persist (Maybe a)
instance (Persist e) => Persist (T.Tree e)
instance (Persist a, Persist b) => Persist (Either a b)
instance (Persist a, Persist b) => Persist (a, b)
instance (Persist a, Persist b, Persist c) => Persist (a, b, c)
instance
  (Persist a, Persist b, Persist c, Persist d) =>
  Persist (a, b, c, d)
instance
  (Persist a, Persist b, Persist c, Persist d, Persist e) =>
  Persist (a, b, c, d, e)
instance
  ( Persist a
  , Persist b
  , Persist c
  , Persist d
  , Persist e
  , Persist f
  ) =>
  Persist (a, b, c, d, e, f)
instance
  ( Persist a
  , Persist b
  , Persist c
  , Persist d
  , Persist e
  , Persist f
  , Persist g
  ) =>
  Persist (a, b, c, d, e, f, g)
instance (Persist a) => Persist (M.Dual a)
instance Persist M.All
instance Persist M.Any
instance (Persist a) => Persist (M.Sum a)
instance (Persist a) => Persist (M.Product a)
instance (Persist a) => Persist (M.First a)
instance (Persist a) => Persist (M.Last a)

{- | Persist a list in the following format:
  Word64 (little endian format)
  element 1
  ...
  element n
-}
instance (Persist a) => Persist [a] where
  put l = do
    sizeHandle <- reserveSize @Word64
    go sizeHandle 0 l
   where
    go sizeHandle !n [] = resolveSizeLE sizeHandle n
    go sizeHandle !n (x : rest) = put x >> go sizeHandle (n + 1) rest
  {-# INLINE put #-}

  get = go [] =<< get @Word64
   where
    go as 0 = pure $! reverse as
    go as i = do
      x <- get
      x `seq` go (x : as) (i - 1)
  {-# INLINE get #-}

instance Persist ByteString where
  put s = do
    let lengthSize = fromIntegral (endiannessSize @Int)
    grow (B.length s + lengthSize)
    unsafePut s
  {-# INLINE put #-}

  unsafePut s = do
    unsafePut $ B.length s
    unsafePutByteString s
  {-# INLINE unsafePut #-}

  get = get >>= getByteString
  {-# INLINE get #-}

instance Persist L.ByteString where
  put s = do
    let lengthSize = fromIntegral (endiannessSize @Int64)
    grow (fromIntegral $ L.length s + lengthSize)
    unsafePut s
  {-# INLINE put #-}

  unsafePut s = do
    unsafePut $ L.length s
    forM_ (L.toChunks s) unsafePutByteString
  {-# INLINE unsafePut #-}

  get = L.fromStrict <$!> get
  {-# INLINE get #-}

instance Persist S.ShortByteString where
  put s = do
    let lengthSize = fromIntegral (endiannessSize @Int)
    grow (S.length s + lengthSize)
    unsafePut s

  unsafePut s = do
    let n = S.length s
    unsafePut n
    Put $ \_ p -> do
      S.copyToPtr s 0 p n
      pure $! p `plusPtr` n :!: ()

  get = S.toShort <$!> get

instance (Ord a, Persist a) => Persist (Set a) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance (Ord k, Persist k, Persist e) => Persist (Map k e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance Persist IntSet where
  put = put . toList
  get = fromList <$!> get

instance (Persist e) => Persist (NonEmpty e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance (Persist e) => Persist (IntMap e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

instance (Persist e) => Persist (Seq e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$!> get
  {-# INLINE get #-}

type family SumArity (a :: Type -> Type) :: Nat where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

class GPersistPut f where
  gput :: f a -> Put ()

class GPersistGet f where
  gget :: Get (f a)

instance (GPersistPut f) => GPersistPut (M1 i c f) where
  gput = gput . unM1
  {-# INLINE gput #-}

instance (GPersistGet f) => GPersistGet (M1 i c f) where
  gget = fmap M1 gget
  {-# INLINE gget #-}

instance (Persist a) => GPersistPut (K1 i a) where
  gput = put . unK1
  {-# INLINE gput #-}

instance (Persist a) => GPersistGet (K1 i a) where
  gget = fmap K1 get
  {-# INLINE gget #-}

instance GPersistPut U1 where
  gput _ = pure ()
  {-# INLINE gput #-}

instance GPersistGet U1 where
  gget = pure U1
  {-# INLINE gget #-}

instance GPersistPut V1 where
  gput x = case x of {}
  {-# INLINE gput #-}

instance GPersistGet V1 where
  gget = undefined
  {-# INLINE gget #-}

instance (GPersistPut a, GPersistPut b) => GPersistPut (a :*: b) where
  gput (a :*: b) = gput a *> gput b
  {-# INLINE gput #-}

instance (GPersistGet a, GPersistGet b) => GPersistGet (a :*: b) where
  gget = (:*:) <$!> gget <*> gget
  {-# INLINE gget #-}

instance (FitsInByte (SumArity (a :+: b)), GPersistPutSum 0 (a :+: b)) => GPersistPut (a :+: b) where
  gput x = gputSum x (Proxy :: Proxy 0)
  {-# INLINE gput #-}

instance (FitsInByte (SumArity (a :+: b)), GPersistGetSum 0 (a :+: b)) => GPersistGet (a :+: b) where
  gget = do
    tag <- get
    ggetSum tag (Proxy :: Proxy 0)
  {-# INLINE gget #-}

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
  FitsInByteResult 'True = ()
  FitsInByteResult 'False =
    TypeErrorMessage
      "Generic deriving of Persist instances can only be used on datatypes with fewer than 256 constructors."

type family TypeErrorMessage (a :: Symbol) :: Constraint where
  TypeErrorMessage a = TypeError ('Text a)

class (KnownNat n) => GPersistPutSum (n :: Nat) (f :: Type -> Type) where
  gputSum :: f p -> Proxy n -> Put ()

class (KnownNat n) => GPersistGetSum (n :: Nat) (f :: Type -> Type) where
  ggetSum :: Word8 -> Proxy n -> Get (f p)

instance
  (GPersistPutSum n a, GPersistPutSum (n + SumArity a) b, KnownNat n) =>
  GPersistPutSum n (a :+: b)
  where
  gputSum (L1 l) _ = gputSum l (Proxy :: Proxy n)
  gputSum (R1 r) _ = gputSum r (Proxy :: Proxy (n + SumArity a))
  {-# INLINE gputSum #-}

instance
  (GPersistGetSum n a, GPersistGetSum (n + SumArity a) b, KnownNat n) =>
  GPersistGetSum n (a :+: b)
  where
  ggetSum tag proxyL
    | tag < sizeL = L1 <$!> ggetSum tag proxyL
    | otherwise = R1 <$!> ggetSum tag (Proxy :: Proxy (n + SumArity a))
   where
    sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
  {-# INLINE ggetSum #-}

instance (GPersistPut a, KnownNat n) => GPersistPutSum n (C1 c a) where
  gputSum x _ = do
    put (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
    gput x
  {-# INLINE gputSum #-}

instance (GPersistGet a, KnownNat n) => GPersistGetSum n (C1 c a) where
  ggetSum tag _
    | tag == cur = gget
    | tag > cur = fail "Sum tag invalid"
    | otherwise = fail "Implementation error"
   where
    cur = fromInteger (natVal (Proxy :: Proxy n))
  {-# INLINE ggetSum #-}

-- | Ensure that @n@ bytes are available. Fails if fewer than @n@ bytes are available.
ensure :: Int -> Get ()
ensure n
  | n < 0 = failGet LengthException "ensure: negative length"
  | otherwise = do
      m <- remaining
      when (m < n) $ failGet LengthException "Not enough bytes available"
{-# INLINE ensure #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = do
  ensure n
  Get $ \_ p -> pure $! p `plusPtr` n :!: ()
{-# INLINE skip #-}

{- | Get the number of remaining unparsed bytes.  Useful for checking whether
all input has been consumed.
-}
remaining :: Get Int
remaining = Get $ \e p -> pure $! p :!: e.end `minusPtr` p
{-# INLINE remaining #-}

-- -- | Succeed if end of input reached.
eof :: Get ()
eof = do
  n <- remaining
  when (n /= 0) $ failGet EOFException "Expected end of file"
{-# INLINE eof #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get ByteString
getBytes n = do
  ensure n
  Get $ \e p -> pure $! p `plusPtr` n :!: B.PS e.buf (p `minusPtr` e.begin) n
{-# INLINE getBytes #-}

{- | An efficient 'get' method for strict ByteStrings. Fails if fewer
than @n@ bytes are left in the input. This function creates a fresh
copy of the underlying bytes.
-}
getByteString :: Int -> Get ByteString
getByteString n = B.copy <$!> getBytes n
{-# INLINE getByteString #-}

{- | An efficient 'get' method to apply a recursive 'get' to a substring of
known length.
-}
getPrefix :: Int -> Get a -> Get a
getPrefix prefixLength baseGet = do
  ensure prefixLength
  unsafeGetPrefix prefixLength baseGet

runPut :: Put a -> ByteString
runPut = snd . evalPut
{-# INLINE runPut #-}

runPutLazy :: Put a -> L.ByteString
runPutLazy = snd . evalPutLazy
{-# INLINE runPutLazy #-}

putByteString :: ByteString -> Put ()
putByteString bs = do
  grow (B.length bs)
  unsafePutByteString bs

unsafePutByteString :: ByteString -> Put ()
unsafePutByteString (B.PS b o n) = do
  Put $ \_ p -> do
    withForeignPtr b $ \q -> copyBytes p (q `plusPtr` o) n
    pure $! p `plusPtr` n :!: ()
{-# INLINE putByteString #-}

{- | Reserve a length value that can be filled in later. The length
  value itself must have a fixed size.
-}
reserveSize :: forall a. (HasEndianness a) => Put (PutSize a)
reserveSize = do
  grow sizeSize
  doWrite
 where
  sizeSize = fromIntegral $ endiannessSize @a
  doWrite = Put $ \e p -> do
    let p' = p `plusPtr` sizeSize
    (c :| _) <- readIORef e.chunks
    pure $!
      p'
        :!: PutSize
          { sizePtr = p
          , sizeStart = p'
          , chunkStart = c.begin
          }

{- | Backpatch a computed length value, excluding the bytes for the length
  itself.
-}
resolveSizeExclusive :: forall a. (Integral a, HasEndianness a) => (a -> Put ()) -> PutSize a -> Put ()
resolveSizeExclusive putter PutSize {..} = Put $ \e p -> do
  writeSize <- computeSize e chunkStart sizeStart p
  _ <- (putter writeSize).unPut e sizePtr
  pure $ p :!: ()

resolveSizeInclusive :: forall a. (Integral a, HasEndianness a) => (a -> Put ()) -> PutSize a -> Put ()
resolveSizeInclusive putter PutSize {..} = Put $ \e p -> do
  writeSize <- computeSize e chunkStart sizePtr p
  _ <- (putter writeSize).unPut e sizePtr
  pure $ p :!: ()

resolveSize :: forall a. (Integral a, HasEndianness a) => (a -> Put ()) -> PutSize a -> a -> Put ()
resolveSize putter PutSize {..} manualSize = Put $ \e p -> do
  _ <- (putter manualSize).unPut e sizePtr
  pure $ p :!: ()

computeSize ::
  forall a.
  (Integral a, HasEndianness a) =>
  PutEnv ->
  Ptr Word8 ->
  Ptr Word8 ->
  Ptr Word8 ->
  IO a
computeSize env chunkStartPtr basePtr finalPtr = do
  (chunk :| chunks) <- readIORef env.chunks
  if chunkStartPtr == chunk.begin
    then pure . fromIntegral $! finalPtr `minusPtr` basePtr
    else pure $ loop chunks (finalPtr `minusPtr` chunk.begin)
 where
  loop :: [Chunk] -> Int -> a
  loop [] !_acc = throw PutSizeMissingStartChunk
  loop (chunk : _) !acc
    | chunk.begin == chunkStartPtr =
        fromIntegral $ acc + (chunk.end `minusPtr` basePtr)
  loop (chunk : rest) !acc =
    loop rest (acc + (chunk.end `minusPtr` chunk.begin))

resolveSizeExclusiveBE :: (Integral a, HasEndianness a) => PutSize a -> Put ()
resolveSizeExclusiveBE = resolveSizeExclusive unsafePutBE

resolveSizeExclusiveLE :: (Integral a, HasEndianness a) => PutSize a -> Put ()
resolveSizeExclusiveLE = resolveSizeExclusive unsafePutLE

resolveSizeInclusiveBE :: (Integral a, HasEndianness a) => PutSize a -> Put ()
resolveSizeInclusiveBE = resolveSizeInclusive unsafePutBE

resolveSizeInclusiveLE :: (Integral a, HasEndianness a) => PutSize a -> Put ()
resolveSizeInclusiveLE = resolveSizeInclusive unsafePutLE

resolveSizeLE :: (Integral a, HasEndianness a) => PutSize a -> a -> Put()
resolveSizeLE = resolveSize unsafePutLE

resolveSizeBE :: (Integral a, HasEndianness a) => PutSize a -> a -> Put()
resolveSizeBE = resolveSize unsafePutBE
