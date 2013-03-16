module Main (main) where

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

tests :: [Test]
tests =
	[
		testGroup "Encode/decode loop" [
			testProperty "BitcoinAddress" prop_read_show_bitcoin,
			testProperty "RippleAddress" prop_read_show_ripple
		]
	]

main :: IO ()
main = defaultMain tests
