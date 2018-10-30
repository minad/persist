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
    , runPut
    , evalPut
    , grow
    , putByteString
    , putHE
    , putLE
    , putBE
) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Data.IORef
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import Foreign (ForeignPtr, Ptr, Storable(..), plusPtr, minusPtr,
                withForeignPtr, mallocBytes, free)
import GHC.Base (unsafeChr, ord)
import GHC.Exts (IsList(..))
import GHC.Float (castWord32ToFloat, castWord64ToDouble, castDoubleToWord64, castFloatToWord32)
import GHC.Generics
import GHC.Real (Ratio(..))
import GHC.TypeLits
import Numeric.Natural
import System.IO.Unsafe
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Short.Internal as S
import qualified Data.Monoid as M
import qualified Data.Text.Encoding as TE
import qualified Data.Tree as T

#include "MachDeps.h"

data a :!: b = !a :!: !b
infixl 2 :!:

putHE :: Persist (HostEndian a) => a -> Put ()
getHE :: Persist (HostEndian a) => Get a
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

newtype BigEndian a = BigEndian { unBigEndian :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

newtype LittleEndian a = LittleEndian { unLittleEndian :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

class Persist t where
  -- | Encode a value in the Put monad.
  put :: t -> Put ()
  -- | Decode a value in the Get monad
  get :: Get t

  default put :: (Generic t, GPersistPut (Rep t)) => t -> Put ()
  put = gput . from

  default get :: (Generic t, GPersistGet (Rep t)) => Get t
  get = to <$> gget

-- | Encode a value using binary serialization to a strict ByteString.
encode :: Persist a => a -> ByteString
encode = runPut . put

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.
decode :: Persist a => ByteString -> Either String a
decode = runGet get

putLE :: Persist (LittleEndian a) => a -> Put ()
putLE = put . LittleEndian
{-# INLINE putLE #-}

putBE :: Persist (BigEndian a) => a -> Put ()
putBE = put . BigEndian
{-# INLINE putBE #-}

getLE :: Persist (LittleEndian a) => Get a
getLE = unLittleEndian <$> get
{-# INLINE getLE #-}

getBE :: Persist (BigEndian a) => Get a
getBE = unBigEndian <$> get
{-# INLINE getBE #-}

unsafePutByte :: Integral a => a -> Put ()
unsafePutByte x = Put $ \_ p -> do
  poke p $ fromIntegral x
  pure $! p `plusPtr` 1 :!: ()
{-# INLINE unsafePutByte #-}

unsafeGetByte :: Num a => Get a
unsafeGetByte = Get $ \_ p -> do
  x <- peek p
  pure $! p `plusPtr` 1 :!: fromIntegral x
{-# INLINE unsafeGetByte #-}

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Persist () where
  put () = pure ()
  {-# INLINE put #-}
  get = pure ()
  {-# INLINE get #-}

instance Persist Word8 where
  put x = do
    grow 1
    unsafePutByte x
  {-# INLINE put #-}

  get = do
    ensure 1
    unsafeGetByte
  {-# INLINE get #-}

instance Persist (LittleEndian Word16) where
  put x = do
    grow 2
    let y = unLittleEndian x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8
  {-# INLINE put #-}

  get = do
    ensure 2
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    pure $ LittleEndian $
      x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word16) where
  put x = do
    grow 2
    let y = unBigEndian x
    unsafePutByte $ y `unsafeShiftR` 8
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 2
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word32) where
  put x = do
    grow 4
    let y = unLittleEndian x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24
  {-# INLINE put #-}

  get = do
    ensure 4
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    x2 <- unsafeGetByte
    x3 <- unsafeGetByte
    pure $ LittleEndian $
      x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word32) where
  put x = do
    grow 4
    let y = unBigEndian x
    unsafePutByte $ y `unsafeShiftR` 24
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 4
    x3 <- unsafeGetByte
    x2 <- unsafeGetByte
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word64) where
  put x = do
    grow 8
    let y = unLittleEndian x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 32 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 40 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 48 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 56
  {-# INLINE put #-}

  get = do
    ensure 8
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    x2 <- unsafeGetByte
    x3 <- unsafeGetByte
    x4 <- unsafeGetByte
    x5 <- unsafeGetByte
    x6 <- unsafeGetByte
    x7 <- unsafeGetByte
    pure $ LittleEndian $
      x7 `unsafeShiftL` 56
      .|. x6 `unsafeShiftL` 48
      .|. x5 `unsafeShiftL` 40
      .|. x4 `unsafeShiftL` 32
      .|. x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word64) where
  put x = do
    grow 8
    let y = unBigEndian x
    unsafePutByte $ y `unsafeShiftR` 56
    unsafePutByte $ y `unsafeShiftR` 48 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 40 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 32 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 8
    x7 <- unsafeGetByte
    x6 <- unsafeGetByte
    x5 <- unsafeGetByte
    x4 <- unsafeGetByte
    x3 <- unsafeGetByte
    x2 <- unsafeGetByte
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x7 `unsafeShiftL` 56
      .|. x6 `unsafeShiftL` 48
      .|. x5 `unsafeShiftL` 40
      .|. x4 `unsafeShiftL` 32
      .|. x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist Int8 where
  put = put @Word8 . fromIntegral
  {-# INLINE put #-}
  get = fromIntegral <$> get @Word8
  {-# INLINE get #-}

instance Persist (LittleEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$> get
  {-# INLINE get #-}

instance Persist Int16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$> get
  {-# INLINE get #-}

instance Persist Int32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist Int64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Double) where
  put = put . fmap castDoubleToWord64
  {-# INLINE put #-}
  get = fmap castWord64ToDouble <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Double) where
  put = put . fmap castDoubleToWord64
  {-# INLINE put #-}
  get = fmap castWord64ToDouble <$> get
  {-# INLINE get #-}

instance Persist Double where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Float) where
  put = put . fmap castFloatToWord32
  {-# INLINE put #-}
  get = fmap castWord32ToFloat <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Float) where
  put = put . fmap castFloatToWord32
  {-# INLINE put #-}
  get = fmap castWord32ToFloat <$> get
  {-# INLINE get #-}

instance Persist Float where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist Word where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$> get
  {-# INLINE get #-}

instance Persist Int where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist Integer where
  put n = do
    put $ n < 0
    put $ unroll $ abs n

  get = do
    neg <- get
    val <- roll <$> get
    pure $! if neg then negate val else val

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where step 0 = Nothing
        step i = Just (fromIntegral i, i `unsafeShiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where unstep b a = a `unsafeShiftL` 8 .|. fromIntegral b

instance Persist a => Persist (Ratio a) where
  put (n :% d) = put n *> put d
  {-# INLINE put #-}

  get = (:%) <$> get <*> get
  {-# INLINE get #-}

instance Persist Natural where
  put = put . unroll
  get = roll <$> get

-- Char is serialized as UTF-8
instance Persist Char where
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
    let byte = fromIntegral <$> get @Word8
        shiftL6 = flip unsafeShiftL 6
    w <- byte
    r <- if | w < 0x80  -> pure w
            | w < 0xe0  -> do
                x <- xor 0x80 <$> byte
                pure $ x .|. shiftL6 (xor 0xc0 w)
            | w < 0xf0  -> do
                x <- xor 0x80 <$> byte
                y <- xor 0x80 <$> byte
                pure $ y .|. shiftL6 (x .|. shiftL6
                                       (xor 0xe0 w))
            | otherwise -> do
                x <- xor 0x80 <$> byte
                y <- xor 0x80 <$> byte
                z <- xor 0x80 <$> byte
                pure $ z .|. shiftL6 (y .|. shiftL6
                                       (x .|. shiftL6 (xor 0xf0 w)))
    if r < 0x10FFFF then
      pure $ unsafeChr r
    else
      fail "Invalid character"
  {-# INLINE get #-}

instance Persist Text where
  put = put . TE.encodeUtf8
  {-# INLINE put #-}
  get = do
    n <- get
    TE.decodeUtf8 <$> getBytes n
  {-# INLINE get #-}

instance Persist Bool
instance Persist Ordering
instance (Persist a) => Persist (Maybe a)
instance Persist e => Persist (T.Tree e)
instance (Persist a, Persist b) => Persist (Either a b)
instance (Persist a, Persist b) => Persist (a,b)
instance (Persist a, Persist b, Persist c) => Persist (a,b,c)
instance (Persist a, Persist b, Persist c, Persist d)
        => Persist (a,b,c,d)
instance (Persist a, Persist b, Persist c, Persist d, Persist e)
        => Persist (a,b,c,d,e)
instance (Persist a, Persist b, Persist c, Persist d, Persist e
         , Persist f)
        => Persist (a,b,c,d,e,f)
instance (Persist a, Persist b, Persist c, Persist d, Persist e
         , Persist f, Persist g)
        => Persist (a,b,c,d,e,f,g)
instance Persist a => Persist (M.Dual a)
instance Persist M.All
instance Persist M.Any
instance Persist a => Persist (M.Sum a)
instance Persist a => Persist (M.Product a)
instance Persist a => Persist (M.First a)
instance Persist a => Persist (M.Last a)

-- | Persist a list in the following format:
--   Word64 (little endian format)
--   element 1
--   ...
--   element n
instance Persist a => Persist [a] where
    put l = do
      put $ length l
      mapM_ put l
    {-# INLINE put #-}

    get = go [] =<< get @Word64
      where go as 0 = pure $! reverse as
            go as i = do x <- get
                         x `seq` go (x:as) (i - 1)
    {-# INLINE get #-}

instance Persist ByteString where
  put s = do
    put $ B.length s
    putByteString s
  get = get >>= getByteString

instance Persist L.ByteString where
  put = put . L.toStrict
  get = L.fromStrict <$!> get

instance Persist S.ShortByteString where
  put s = do
    let n = S.length s
    put n
    grow n
    Put $ \_ p -> do
      S.copyToPtr s 0 p n
      pure $! p `plusPtr` n :!: ()

  get = S.toShort <$!> get

instance (Ord a, Persist a) => Persist (Set a) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

instance (Ord k, Persist k, Persist e) => Persist (Map k e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

instance Persist IntSet where
  put = put . toList
  get = fromList <$> get

instance Persist e => Persist (NonEmpty e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

instance Persist e => Persist (IntMap e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

instance Persist e => Persist (Seq e) where
  put = put . toList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

type family SumArity (a :: * -> *) :: Nat where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

class GPersistPut f where
  gput :: f a -> Put ()

class GPersistGet f where
  gget :: Get (f a)

instance GPersistPut f => GPersistPut (M1 i c f) where
  gput = gput . unM1
  {-# INLINE gput #-}

instance GPersistGet f => GPersistGet (M1 i c f) where
  gget = fmap M1 gget
  {-# INLINE gget #-}

instance Persist a => GPersistPut (K1 i a) where
  gput = put . unK1
  {-# INLINE gput #-}

instance Persist a => GPersistGet (K1 i a) where
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
  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

instance (SumArity (a :+: b) <= 255, GPersistPutSum 0 (a :+: b)) => GPersistPut (a :+: b) where
  gput x = gputSum x (Proxy :: Proxy 0)
  {-# INLINE gput #-}

instance (SumArity (a :+: b) <= 255, GPersistGetSum 0 (a :+: b)) => GPersistGet (a :+: b) where
  gget = do
    tag <- get
    ggetSum tag (Proxy :: Proxy 0)
  {-# INLINE gget #-}

class KnownNat n => GPersistPutSum (n :: Nat) (f :: * -> *) where
  gputSum :: f p -> Proxy n -> Put ()

class KnownNat n => GPersistGetSum (n :: Nat) (f :: * -> *) where
  ggetSum :: Word8 -> Proxy n -> Get (f p)

instance (GPersistPutSum n a, GPersistPutSum (n + SumArity a) b, KnownNat n)
         => GPersistPutSum n (a :+: b) where
  gputSum (L1 l) _ = gputSum l (Proxy :: Proxy n)
  gputSum (R1 r) _ = gputSum r (Proxy :: Proxy (n + SumArity a))
  {-# INLINE gputSum #-}

instance (GPersistGetSum n a, GPersistGetSum (n + SumArity a) b, KnownNat n)
         => GPersistGetSum n (a :+: b) where
  ggetSum tag proxyL
    | tag < sizeL = L1 <$> ggetSum tag proxyL
    | otherwise = R1 <$> ggetSum tag (Proxy :: Proxy (n + SumArity a))
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

data GetEnv = GetEnv
  { geBuf   :: !(ForeignPtr Word8)
  , geBegin :: !(Ptr Word8)
  , geEnd   :: !(Ptr Word8)
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

  fail = Fail.fail
  {-# INLINE fail #-}

instance Fail.MonadFail Get where
  fail msg = Get $ \_ _ -> fail $ "Failed reading: " <> msg
  {-# INLINE fail #-}

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> ByteString -> Either String a
runGet m s = unsafePerformIO $ catch run handler
  where run = withForeignPtr buf $ \p -> do
          let env = GetEnv { geBuf = buf, geBegin = p, geEnd = p `plusPtr` (pos + len) }
          _ :!: r <- unGet m env (p `plusPtr` pos)
          pure $ Right r
        handler (e :: IOException) = pure $ Left $ displayException e
        (B.PS buf pos len) = s
{-# NOINLINE runGet #-}

-- | Ensure that @n@ bytes are available. Fails if fewer than @n@ bytes are available.
ensure :: Int -> Get ()
ensure n
  | n < 0 = fail "ensure: negative length"
  | otherwise = do
      m <- remaining
      when (m < n) $ fail "Not enough bytes available"
{-# INLINE ensure #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = do
  ensure n
  Get $ \_ p -> pure $! p `plusPtr` n :!: ()
{-# INLINE skip #-}

-- | Get the number of remaining unparsed bytes.  Useful for checking whether
-- all input has been consumed.
remaining :: Get Int
remaining = Get $ \e p -> pure $! p :!: geEnd e `minusPtr` p
{-# INLINE remaining #-}

-- -- | Succeed if end of input reached.
eof :: Get ()
eof = do
  n <- remaining
  when (n /= 0) $ fail "Expected end of file"
{-# INLINE eof #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get ByteString
getBytes n = do
  ensure n
  Get $ \e p -> pure $! p `plusPtr` n :!: B.PS (geBuf e) (p `minusPtr` geBegin e) n
{-# INLINE getBytes #-}

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getByteString :: Int -> Get ByteString
getByteString n = B.copy <$!> getBytes n
{-# INLINE getByteString #-}

data Chunk = Chunk
  { chkBegin :: !(Ptr Word8)
  , chkEnd   :: !(Ptr Word8)
  }

data PutEnv = PutEnv
  { peChks :: !(IORef (NonEmpty Chunk))
  , peEnd  :: !(IORef (Ptr Word8))
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

doGrow :: PutEnv -> Ptr Word8 -> Int -> IO (Ptr Word8 :!: ())
doGrow e p n = do
  k <- newChunk n
  modifyIORef' (peChks e) $ \case
    (c:|cs) -> k :| c { chkEnd = p } : cs
  writeIORef (peEnd e) (chkEnd k)
  pure $! chkBegin k :!: ()
{-# NOINLINE doGrow #-}

-- | Ensure that @n@ bytes can be written.
grow :: Int -> Put ()
grow n
  | n < 0 = fail "grow: negative length"
  | otherwise = Put $ \e p -> do
      end <- readIORef (peEnd e)
      if end `minusPtr` p >= n then
        pure $! p :!: ()
      else
        doGrow e p n
{-# INLINE grow #-}

runPut :: Put a -> ByteString
runPut = snd . evalPut
{-# INLINE runPut #-}

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

evalPut :: Put a -> (a, ByteString)
evalPut p = unsafePerformIO $ do
  k <- newChunk 0
  chks <- newIORef (k:|[])
  end <- newIORef (chkEnd k)
  p' :!: r <- unPut p (PutEnv chks end) (chkBegin k)
  cs <- readIORef chks
  s <- case cs of
    (x:|xs) -> catChunks $ x { chkEnd = p' } : xs
  pure (r, s)
{-# NOINLINE evalPut #-}

putByteString :: ByteString -> Put ()
putByteString (B.PS b o n) = do
  grow n
  Put $ \_ p -> do
    withForeignPtr b $ \q -> B.memcpy p (q `plusPtr` o) n
    pure $! p `plusPtr` n :!: ()
{-# INLINE putByteString #-}
