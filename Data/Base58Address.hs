{-# LANGUAGE CPP #-}
#ifdef TESTS
module Data.Base58Address (BitcoinAddress, RippleAddress, RippleAddress0(..)) where
#else
module Data.Base58Address (BitcoinAddress, RippleAddress) where
#endif

import Control.Monad (when)
import Control.Arrow ((***))
import Data.Word
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Binary.Get (getByteString)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS

import Data.Base58Address.BaseConvert
import Data.Base58Address.Alphabet

#ifdef TESTS
import Test.QuickCheck

instance Arbitrary Base58Address where
	arbitrary = do
		ver <- arbitrary
		adr <- arbitrary `suchThat` (>=0)
		return $ Base58Address ver adr

instance Arbitrary BitcoinAddress where
	arbitrary = fmap BitcoinAddress arbitrary

instance Arbitrary RippleAddress where
	arbitrary = fmap RippleAddress arbitrary

newtype RippleAddress0 = RippleAddress0 RippleAddress
	deriving (Show)

instance Arbitrary RippleAddress0 where
	arbitrary = do
		adr <- arbitrary `suchThat` (>=0)
		return $ RippleAddress0 $ RippleAddress $ Base58Address 0 adr
#endif

newtype BitcoinAddress = BitcoinAddress Base58Address
	deriving (Ord, Eq)

bitcoinAlphabet :: Alphabet
bitcoinAlphabet = read "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

instance Show BitcoinAddress where
	show (BitcoinAddress adr) = showB58 bitcoinAlphabet adr

instance Read BitcoinAddress where
	readsPrec _ s = case decodeB58 bitcoinAlphabet s of
		Just x -> [(BitcoinAddress x,"")]
		Nothing -> []

newtype RippleAddress = RippleAddress Base58Address
	deriving (Ord, Eq)

rippleAlphabet :: Alphabet
rippleAlphabet = read "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

instance Show RippleAddress where
	show (RippleAddress adr) = showB58 rippleAlphabet adr

instance Read RippleAddress where
	readsPrec _ s = case decodeB58 rippleAlphabet s of
		Just x -> [(RippleAddress x,"")]
		Nothing -> []

instance Binary RippleAddress where
	get = do
		len <- getWord8
		when (len /= 20) $
			fail $ "RippleAddress is 160 bit encoding, len is " ++ show len
		value <- (fromBase 256 . BS.unpack) `fmap` getByteString 20
		return $ RippleAddress (Base58Address 0 value)

	put (RippleAddress (Base58Address 0 value)) = do
		putWord8 20
		let bytes = toBase 256 value
		mapM_ putWord8 (replicate (20 - length bytes) 0 ++ bytes)
	put _ = fail "RippleAddress version is always 0"

data Base58Address = Base58Address !Word8 !Integer
	deriving (Ord, Eq)

showB58 :: Alphabet -> Base58Address -> String
showB58 alphabet (Base58Address version addr) = prefix ++
	toString alphabet 58 (fromBase 256 (bytes' ++ mkChk bytes') :: Integer)
	where
	prefix | version == 0 = toString alphabet 58 0
	       | otherwise = ""
	bytes' = version : replicate (20 - length bytes) 0 ++ bytes
	bytes = toBase 256 addr

decodeB58 :: Alphabet -> String -> Maybe Base58Address
decodeB58 alphabet s = do
	(chk,bytes) <- fmap (splitChk . toBase 256)
		(toIntegral alphabet 58 s :: Maybe Integer)
	let bytes' = replicate (21 - length bytes) 0 ++ bytes
	if mkChk bytes' /= chk then Nothing else
		Just $! Base58Address (head bytes') (fromBase 256 (tail bytes'))

splitChk :: [a] -> ([a], [a])
splitChk = (reverse *** reverse) . splitAt 4 . reverse

mkChk :: [Word8] -> [Word8]
mkChk = BS.unpack . BS.take 4 . SHA256.hash . SHA256.hash . BS.pack
