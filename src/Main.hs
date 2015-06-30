module Main where

import Web.Authenticate.OAuth ()
import UI.HSCurses.CursesHelper (start, end)
import UI.HSCurses.Curses (Window, Key(..), update, wAddStr, werase
                          ,wRefresh, getYX, move, getCh, stdScr)

data Tweet = Tweet String

tweets :: [Tweet]
tweets = map Tweet ["Hello, World!", "Another Tweet!"
                   ,"A third tweet!", "There are so many of these tweets!"]

content :: Tweet -> String
content (Tweet x) = x

drawTweet :: Tweet -> Window -> IO ()
drawTweet tweet window = do wAddStr window $ content tweet
                            wAddStr window "\n "

data AppState = AppState Int

offset :: AppState -> Int
offset (AppState x) = x

_mapM :: (Monad m) => [a] -> (a -> m b) -> m ()
_mapM a b = mapM_ b a

drawAll :: AppState -> IO ()
drawAll state = do werase stdScr
                   move (offset state) 0
                   _mapM tweets $ \tweet -> do drawTweet tweet stdScr
                                               (y, _) <- getYX stdScr
                                               move (y + 1) 0
                   wRefresh stdScr
                   update

respondToKey :: AppState -> IO AppState
respondToKey state = getCh >>= \key -> case key of
  KeyChar 'q' -> return state
  KeyUp       -> do let newState = AppState $ max 0 $ offset state - 1
                    drawAll newState
                    respondToKey newState
  KeyDown     -> do let newState = AppState $ offset state + 1
                    drawAll newState
                    respondToKey newState
  _           -> respondToKey state

main :: IO ()
main = do start -- init curses
          let state = AppState 0
          drawAll state
          respondToKey state
          end   -- clean up curses
  

