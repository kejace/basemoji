{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Encoding.Basemoji where

import           Control.Applicative
import           Data.List
import           Data.Monoid
import           Data.String

newtype Emoji = Emoji String deriving (Eq, Read, IsString, Monoid)

instance Show Emoji where
    show (Emoji x) = x

newtype Basemoji = Basemoji [Emoji]

data Alphabet = Hands [Emoji] | Ages [Emoji]

instance Show Basemoji where
    show (Basemoji x) = concat $ show <$> x

hexAlphabet :: [Emoji]
hexAlphabet = ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"]

charAlphabet = ['a','b','c','d','e','f']
charMods = [-32]

hands :: [Emoji]
hands = [ "\128072", "\128073"
        , "\9757", "\128070"
        , "\128405", "\128071"
        , "\9996", "\129310"
        , "\128406", "\129304"
        , "\129305", "\128400"
        , "\9995", "\128076"
        , "\128077", "\128078"
        , "\9994", "\128074"
        , "\129307", "\129308"
        ]

ages :: [Emoji]
ages = [ "\128118" -- baby
    -- , "\129490" -- child
       , "\128102" -- boy
       , "\128103" -- girl
    -- , "\129489" -- adult
       , "\128104" -- man
       , "\128105" -- woman
    -- , "\129491" -- older adult
       , "\128116" -- older man
       , "\128117" -- older woman
       ]

allEmoji = mappend <$> (hands ++ ages) <*> fitzPatrick

fitzPatrick :: [Emoji]
fitzPatrick = ["\127995", "\127996", "\127997", "\127998", "\127999"]

decode :: [Emoji] -> [Emoji] -> Maybe Integer
decode alphabet = foldl (\v d -> (+) <$> ((len *) <$> v) <*> (fromIntegral <$> elemIndex d alphabet )) $ Just 0
  where len = fromIntegral $ length alphabet

encode :: [Emoji] -> Integer -> [Emoji]
encode alphabet x = f $ y x
   where f [] = [genericIndex alphabet 0]
         f a  = a
         y s  = reverse $ unfoldr (\b -> if b == 0 
                                         then Nothing 
                                         else Just ((genericIndex alphabet) $ b `mod` len, b `div` len)                                           
                                  ) s 
         len  = fromIntegral . length $ alphabet 
