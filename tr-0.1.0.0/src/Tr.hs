-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- makeMap
makeMap :: CharSet -> CharSet -> [(Char, Char)]
makeMap [] (v : []) = []
makeMap (k : ks) (v : []) = (k, v) : (makeMap ks (v : []))
makeMap (k : ks) (v0 : v1 : vs) = (k, v0) : (makeMap ks (v1 : vs))

-- replaceMode
replaceMode :: [(Char, Char)] -> CharSet -> CharSet
replaceMode lookupTable [] = []
replaceMode lookupTable (k : ks) = (case lookup k lookupTable of
                                      Just v -> v
                                      Nothing -> k
                                   ) : (replaceMode lookupTable ks)

-- deleteMode
deleteMode :: [Char] -> CharSet -> CharSet
deleteMode dict [] = []
deleteMode dict (k : ks) | elem k dict = deleteMode dict ks
                         | otherwise = k : (deleteMode dict ks)

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
tr inset (Just outset) = replaceMode (makeMap inset outset)
tr inset Nothing = deleteMode inset


