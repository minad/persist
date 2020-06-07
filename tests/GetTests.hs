{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GetTests (tests) where

import           Control.Applicative
import           Control.Monad
import           Data.Word
import           Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.Persist
import           Test.Framework (Test(),testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck as QC


-- Data to express Get parser to generate
data GetD
  = Get8
  | Eof
  | Get16be
  | Get32be
  | Get64be
  | Get16le
  | Get32le
  | Get64le
  | GetD  :*>   GetD
  | Skip Int
  deriving Show

-- Get parser generator
buildGet :: forall s. GetD -> Get s ()
buildGet = d  where
  d Get8           =  get @s @Word8    *> pure ()
  d Eof            =  eof
  d Get16be        =  getBE @s @Word16 *> pure ()
  d Get32be        =  getBE @s @Word32 *> pure ()
  d Get64be        =  getBE @s @Word64 *> pure ()
  d Get16le        =  getLE @s @Word16 *> pure ()
  d Get32le        =  getLE @s @Word32 *> pure ()
  d Get64le        =  getLE @s @Word64 *> pure ()
  d (x :*>  y)     =  d x *>  d y
  d (Skip i)       =  skip i

-- Randomly generate parser
genGetD :: Gen GetD
genGetD =
    oneof $
    [ pure g
    | g <- [ Get8, Eof
           , Get16be, Get32be, Get64be
           , Get16le, Get32le, Get64le
           ]
    ] <>
    [ (:*>)     <$> genGetD <*> genGetD
    , Skip      <$> choose (0, 10)
    ]

instance Arbitrary GetD where
  arbitrary = genGetD

instance Arbitrary (Get s ()) where
  arbitrary = buildGet <$> genGetD

newtype R a =
  R { unR :: Either String a }
  deriving Show


-- Ignore equality of error message string
instance Eq a => Eq (R a) where
  (==)  =  (==) `on` either (const Nothing) Just . unR

data Chunks = Chunks [[Word8]] deriving (Eq, Show)

mkChunks :: Word -> Chunks
mkChunks n = Chunks . take (fromIntegral n) $ cycle [ [x] | x <- [0 .. 255] ]

instance Arbitrary Chunks where
  arbitrary = mkChunks <$> choose (0, 512)


testLength :: Word
testLength = 255

type instance GetState () = ()

(==!) :: Eq a => Get () a -> Get () a -> Property
p1 ==! p2 =
  conjoin
  [ counterexample (show s) $ R (runGet p1 () s) == R (runGet p2 () s)
  | n <- [0 .. testLength]
  , let Chunks in0 = mkChunks n
        s = BS.pack $ concat in0
  ]

infix 2 ==!

monadIdL' :: GetD -> Property
monadIdL' getD =
    (return () >>= const x) ==! x
  where
    x = buildGet getD

monadIdR' :: GetD -> Property
monadIdR' getD =
    (x >>= return) ==! x
  where
    x = buildGet getD

monadAssoc' :: GetD -> GetD -> GetD -> Property
monadAssoc' p1 p2 p3 =
    (x >> (y >> z)) ==! (x >> y >> z)
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

tests :: Test
tests  = testGroup "GetTests"
  [ testProperty "monad left id"          monadIdL'
  , testProperty "monad right id"         monadIdR'
  , testProperty "monad assoc"            monadAssoc'
  ]
