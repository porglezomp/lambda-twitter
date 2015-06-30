module Main where

import Web.Authenticate.OAuth ()
import UI.HSCurses.CursesHelper (start, end)
import UI.HSCurses.Curses (Key(..), CursorVisibility(CursorInvisible), update, werase, wRefresh, getCh, stdScr, scrSize, cursSet)
import UI.HSCurses.Widgets (Widget(..), DrawingHint(..))

import Data.Tweet
import Data.AppState
import UI.TweetsWidget

_tweets :: [Tweet]
_tweets = map Tweet ["Hello, World!", "Another Tweet!"
                    ,"A third tweet!", "There are so many of these tweets!"]

_mapM :: (Monad m) => [a] -> (a -> m b) -> m ()
_mapM a b = mapM_ b a

drawAll :: AppState -> IO ()
drawAll state = do
  werase stdScr
  (w, h) <- scrSize
  draw (offset state, 0) (w, h) DHNormal $ TweetsWidget . Data.AppState.tweets $ state
  wRefresh stdScr
  update

respondToKey :: AppState -> IO ()
respondToKey state = getCh >>= \key -> case key of
  KeyChar 'q' -> return ()
  KeyUp       -> do let state' = AppState (max 0 $ offset state - 1)  $ Data.AppState.tweets state
                    drawAll state'
                    respondToKey state'
  KeyDown     -> do let state' = AppState (offset state + 1) $ Data.AppState.tweets state
                    drawAll state'
                    respondToKey state'
  _           -> respondToKey state

main :: IO ()
main = do start -- init curses
          cursSet CursorInvisible
          let state = AppState 0 _tweets
          drawAll state
          respondToKey state
          end   -- clean up curses
