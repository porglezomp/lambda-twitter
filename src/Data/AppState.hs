module Data.AppState where

import Data.Tweet

data AppState = AppState Int [Tweet]

tweets :: AppState -> [Tweet]
tweets (AppState _ tws) = tws

offset :: AppState -> Int
offset (AppState x _) = x
