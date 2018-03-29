{-# LANGUAGE ScopedTypeVariables #-}
-- |

import Data.Char
import Data.Ix
import Data.List
import Data.Monoid
import Test.QuickCheck

fieldCount = 10
rowCount = 1000

newtype S = S String

instance Arbitrary S where
  arbitrary = do
    s <- arbitrary
    pure (S (map clean s))
    where
      clean c =
        if inRange ('a', 'z') c ||
           inRange ('0', '9') c || inRange ('A', 'Z') c || elem c "\r\n,\" \t"
          then c
          else 'a'

main :: IO ()
main = do
  rows <-
    generate
      (vectorOf
         1000
         ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary))
  putStr
    (unlines
       (map
          (intercalate "," .
           map printField .
           (\(S v, S x, S y, S z) -> take fieldCount (cycle [v, x, y, z])))
          rows))

printField :: String -> String
printField cs =
  if any invalid cs
    then escape cs
    else cs
  where
    invalid c = c == ',' || c == '\r' || c == '\n' || c == '"'
    escape =
      ("\"" <>) .
      (<> "\"") .
      concatMap
        (\c ->
           if c == '"'
             then "\"\""
             else [c])
