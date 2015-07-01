module Main where

import Web.Authenticate.OAuth ()
import UI.HSCurses.CursesHelper (start, end)
import UI.HSCurses.Curses (Key(..), CursorVisibility(CursorInvisible), update,
                           werase, wRefresh, getCh, stdScr, scrSize, cursSet)
import UI.HSCurses.Widgets (Widget(..), DrawingHint(..))
import Data.List.Zipper (fromList)

import Data.Tweet
import Data.AppState

createTweets :: IO [Tweet]
createTweets = return $ map Tweet [
  "Hello, World!", "Another Tweet!", "A third tweet!",
  "There are so many of these tweets!", "You have to love tweets",
  "So many tweets", "Wow look at all of them"
  ]

drawAll :: AppState -> IO ()
drawAll state = do
  werase stdScr
  (w, h) <- scrSize
  draw (0, 0) (w, h) DHNormal $ tweetsWidget state
  wRefresh stdScr
  update

respondToKey :: AppState -> IO ()
respondToKey state = getCh >>= \key -> case key of
  KeyChar 'q' -> return ()
  KeyUp       -> do let state' = scrollUp state
                    drawAll state'
                    respondToKey state'
  KeyDown     -> do let state' = scrollDown state
                    drawAll state'
                    respondToKey state'
  _           -> respondToKey state

main :: IO ()
main = do start -- init curses
          cursSet CursorInvisible
          tweets <- createTweets
          let state = AppState (fromList tweets) junkFrame
          drawAll state
          respondToKey state
          end   -- clean up curses
