module Data.Tweet where

data Tweet = Tweet String

content :: Tweet -> String
content (Tweet x) = x




