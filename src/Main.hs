module Main where

import Web.Authenticate.OAuth ()
import UI.HSCurses.Curses (initScr, update, wAddStr, wRefresh, endWin)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do window <- initScr
          wAddStr window "Hello, World!"
          wRefresh window
          update
          threadDelay 1000000 -- in µsec
          endWin
  

