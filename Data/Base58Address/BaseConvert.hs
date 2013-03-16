module Data.Base58Address.BaseConvert (toBase, fromBase) where

import Data.Sequence (unfoldl)
import Data.Foldable (toList)

toBase :: (Integral a, Integral b) => a -> a -> [b]
toBase _ 0 = [0]
toBase b v
	| v < 0 = error "Data.Base58Address.BaseConvert.toBase v < 0"
	| otherwise = map fromIntegral $ toList $
		unfoldl (\n -> if n == 0 then Nothing else Just $! (n `divMod` b)) v

fromBase :: (Integral a, Integral b) => b -> [a] -> b
fromBase b = foldl (\n k -> n * b + fromIntegral k) 0
