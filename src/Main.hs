module Main where

import Web.Authenticate.OAuth ()
import UI.HSCurses.CursesHelper (start, end, drawLine)
import UI.HSCurses.Curses (Key(..), update, wAddStr, werase
                          ,wRefresh, getYX, move, getCh, stdScr, scrSize)
import UI.HSCurses.Widgets (Widget(..), DrawingHint(..))
import Data.Tweet
import Data.AppState

_tweets :: [Tweet]
_tweets = map Tweet ["Hello, World!", "Another Tweet!"
                    ,"A third tweet!", "There are so many of these tweets!"]

drawTweets :: [Tweet] -> IO ()
drawTweets twts = do (y, _) <- getYX stdScr
                     drawTweets' twts y
  where
    drawTweets' [] _ = return ()
    drawTweets' (t:ts) y =
      do (w, _) <- scrSize
         draw (y, 0) (w, 3) DHNormal $ TweetWidget { tweet = t }
         drawTweets' ts $ y + 4

_mapM :: (Monad m) => [a] -> (a -> m b) -> m ()
_mapM a b = mapM_ b a

drawAll :: AppState -> IO ()
drawAll state = do werase stdScr
                   move (offset state) 0
                   drawTweets $ tweets state
                   wRefresh stdScr
                   update

data TweetWidget = TweetWidget {
  tweet :: Tweet
  }

instance Widget TweetWidget where
  minSize tweetWidget = (length . content . tweet $ tweetWidget, 3)
  draw (y, x) (w, h) hint tweetWidget =
    do move y x
       drawLine w borderChar
       move (y + 1) (x + 1)
       wAddStr stdScr $ content . tweet $ tweetWidget
       move (y + h) x
       drawLine w borderChar
       return ()
    where
      borderChar = case hint of
        DHNormal -> repeat '-'
        _        -> repeat '='


respondToKey :: AppState -> IO ()
respondToKey state = getCh >>= \key -> case key of
  KeyChar 'q' -> return ()
  KeyUp       -> do let newState = AppState (max 0 $ offset state - 1)  $ tweets state
                    drawAll newState
                    respondToKey newState
  KeyDown     -> do let newState = AppState (offset state + 1) $ tweets state
                    drawAll newState
                    respondToKey newState
  _           -> respondToKey state

main :: IO ()
main = do start -- init curses
          let state = AppState 0 _tweets
          drawAll state
          respondToKey state
          end   -- clean up curses
  

