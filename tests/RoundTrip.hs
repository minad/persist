{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------
-- |
-- Module      :
-- Copyright   : (c) Galois, Inc, 2009
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
module RoundTrip where

import Numeric.Natural
import Data.Persist
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.QuickCheck as QC

import Test.Framework (Test(),testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

roundTrip :: (Persist a, Eq a) => (a -> Put ()) -> Get a -> a -> Bool
roundTrip p g a = res == Right a
  where res = runGet (g <* eof) (runPut (p a))

-- | Did a call to 'quickCheckResult' succeed?
isSuccess :: QC.Result -> Bool
isSuccess Success{} = True
isSuccess _ = False

tests :: Test
tests  = testGroup "Round Trip"
  [ testProperty "Word8        Round Trip" $ roundTrip put (get @Word8)
  , testProperty "Word16       Round Trip" $ roundTrip put (get @Word16)
  , testProperty "Word16be     Round Trip" $ roundTrip putBE (getBE @Word16)
  , testProperty "Word16le     Round Trip" $ roundTrip putLE (getLE @Word16)
  , testProperty "Word16host   Round Trip" $ roundTrip putHE (getHE @Word16)
  , testProperty "Word32       Round Trip" $ roundTrip put (get @Word32)
  , testProperty "Word32be     Round Trip" $ roundTrip putBE (getBE @Word32)
  , testProperty "Word32le     Round Trip" $ roundTrip putLE (getLE @Word32)
  , testProperty "Word32host   Round Trip" $ roundTrip putHE (getHE @Word32)
  , testProperty "Word64       Round Trip" $ roundTrip put (get @Word64)
  , testProperty "Word64be     Round Trip" $ roundTrip putBE (getBE @Word64)
  , testProperty "Word64le     Round Trip" $ roundTrip putLE (getLE @Word64)
  , testProperty "Word64host   Round Trip" $ roundTrip putHE (getHE @Word64)

  , testProperty "Int8         Round Trip" $ roundTrip put (get @Int8)
  , testProperty "Int16        Round Trip" $ roundTrip put (get @Int16)
  , testProperty "Int16be      Round Trip" $ roundTrip putBE (getBE @Int16)
  , testProperty "Int16le      Round Trip" $ roundTrip putLE (getLE @Int16)
  , testProperty "Int16host    Round Trip" $ roundTrip putHE (getHE @Int16)
  , testProperty "Int32        Round Trip" $ roundTrip put (get @Int32)
  , testProperty "Int32be      Round Trip" $ roundTrip putBE (getBE @Int32)
  , testProperty "Int32le      Round Trip" $ roundTrip putLE (getLE @Int32)
  , testProperty "Int32host    Round Trip" $ roundTrip putHE (getHE @Int32)
  , testProperty "Int64        Round Trip" $ roundTrip put (get @Int64)
  , testProperty "Int64be      Round Trip" $ roundTrip putBE (getBE @Int64)
  , testProperty "Int64le      Round Trip" $ roundTrip putLE (getLE @Int64)
  , testProperty "Int64host    Round Trip" $ roundTrip putHE (getHE @Int64)

  , testProperty "Float        Round Trip" $ roundTrip put (get @Float)
  , testProperty "Floatbe      Round Trip" $ roundTrip putBE (getBE @Float)
  , testProperty "Floatle      Round Trip" $ roundTrip putLE (getLE @Float)
  , testProperty "Floathost    Round Trip" $ roundTrip putHE (getHE @Float)
  , testProperty "Double       Round Trip" $ roundTrip put (get @Double)
  , testProperty "Doublebe     Round Trip" $ roundTrip putBE (getBE @Double)
  , testProperty "Doublele     Round Trip" $ roundTrip putLE (getLE @Double)
  , testProperty "Doublehost   Round Trip" $ roundTrip putHE (getHE @Double)

  , testProperty "Char Round Trip"
    $ roundTrip put (get :: Get Char)
  , testProperty "String Round Trip"
    $ roundTrip put (get :: Get String)
  , testProperty "Text Round Trip"
    $ roundTrip put get . T.pack
  , testProperty "Integer Round Trip"
    $ roundTrip put (get :: Get Integer)
  , testProperty "Natural Round Trip"
    $ roundTrip put get . (fromInteger :: Integer -> Natural) . abs
  , testProperty "(Word8,Word8) Round Trip"
    $ roundTrip put (get :: Get (Word8, Word8))
  , testProperty "(Word8,Word16,Word32,Word64) Round Trip"
    $ roundTrip put (get :: Get (Word8, Word16, Word32, Word64))
  , testProperty "Complex Round Trip"
    $ roundTrip put (get :: Get (Either (Word8, Word8) (Word16, Either Int32 [String], Word64)))
  , testProperty "[Word8] Round Trip"
    $ roundTrip put (get :: Get [Word8])
  , testProperty "Bool Round Trip"
    $ roundTrip put (get :: Get Bool)
  , testProperty "Ordering Round Trip"
    $ roundTrip put (get :: Get Ordering)
  , testProperty "Maybe Word8 Round Trip"
    $ roundTrip put (get :: Get (Maybe Word8))
  , testProperty "Either Word8 Word16 Round Trip"
    $ roundTrip put (get :: Get (Either Word8 Word16))
  ]
