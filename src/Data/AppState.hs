module Data.AppState where

import Data.Tweet
import Data.List.Zipper
import UI.TweetsWidget

data Origin = Top | Bottom
data Frame = Frame { frameTop :: Int, frameBottom :: Int, frameCursor :: Int, frameOrigin :: Origin }
data AppState = AppState { appTweets :: Zipper Tweet, appFrame :: Frame }

junkFrame :: Frame
junkFrame = Frame 0 0 0 Top

scrollUp :: AppState -> AppState
scrollUp state = AppState { appTweets = left . appTweets $ state, appFrame = appFrame state }
                                    
scrollDown :: AppState -> AppState
scrollDown state = AppState { appTweets = right . appTweets $ state, appFrame = appFrame state }

tweetsWidget :: AppState -> TweetsWidget
tweetsWidget = TweetsWidget . appTweets
