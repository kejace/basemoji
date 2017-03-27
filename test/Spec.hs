import Test.QuickCheck
import Data.Encoding.Basemoji

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

instance Arbitrary Alphabet where
   arbitrary = undefined

prop_decodeEncode :: Integer -> Bool
prop_decodeEncode x = (decode hands $ encode hands x) == Just x

main :: IO ()
main = verboseCheck prop_decodeEncode
