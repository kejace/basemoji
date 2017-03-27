module Main where

import qualified Crypto.Hash as C
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Maybe

import           Data.Encoding.Basemoji 

stringToHashInt :: String -> Integer
stringToHashInt s = fromJust x
    where
        x = decode hexAlphabet $ (\x -> Emoji (x:[])) <$> show r
        r = C.hashlazy $ BSL8.pack s :: C.Digest C.RIPEMD160

main :: IO ()
main = do
    s <- getLine
    print .  encode allEmoji . stringToHashInt $ s
