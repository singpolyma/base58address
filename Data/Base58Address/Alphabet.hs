module Data.Base58Address.Alphabet where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Base58Address.BaseConvert

data Alphabet = Alphabet !(IntMap Char) !(Map Char Int)
	deriving (Eq)

instance Show Alphabet where
	show (Alphabet a _) = map snd $ IntMap.toAscList a

instance Read Alphabet where
	readsPrec _ alphabet = [(
			Alphabet
			(IntMap.fromAscList $ zip [0..] alphabet)
			(Map.fromList $ zip alphabet [0..]),
		"")]

toAlphaDigit :: Alphabet -> Int -> Maybe Char
toAlphaDigit (Alphabet letters _) i = IntMap.lookup i letters

toString :: (Integral a) => Alphabet -> a -> a -> String
toString a b v = let Just x = mapM (toAlphaDigit a) (toBase b v) in x

fromAlphaDigit :: Alphabet -> Char -> Maybe Int
fromAlphaDigit (Alphabet _ indices) v = Map.lookup v indices

toIntegral :: (Integral a) => Alphabet -> a -> String -> Maybe a
toIntegral a b = fmap (fromBase b) .
	mapM (fmap toInteger . fromAlphaDigit a)
