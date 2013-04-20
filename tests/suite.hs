module Main (main) where

import Data.Binary (encode, decode)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

import Data.Base58Address

prop_read_show_bitcoin :: BitcoinAddress -> Bool
prop_read_show_bitcoin adr = (read $! str) == adr
	where
	str = show $! adr
	{-# NOINLINE str #-}

prop_read_show_ripple :: RippleAddress -> Bool
prop_read_show_ripple adr = (read $! str) == adr
	where
	str = show $! adr
	{-# NOINLINE str #-}

prop_encode_decode_ripple :: RippleAddress0 -> Bool
prop_encode_decode_ripple (RippleAddress0 adr) = (decode $! bytes) == adr
	where
	bytes = encode $! adr
	{-# NOINLINE bytes #-}

tests :: [Test]
tests =
	[
		testGroup "read/show loop" [
			testProperty "BitcoinAddress" prop_read_show_bitcoin,
			testProperty "RippleAddress" prop_read_show_ripple
		],
		testGroup "encode/decode loop" [
			testProperty "RippleAddress" prop_encode_decode_ripple
		]
	]

main :: IO ()
main = defaultMain tests
