module UI.TweetsWidget where

import UI.HSCurses.CursesHelper (drawLine)
import UI.HSCurses.Curses (stdScr, move, wAddStr)
import UI.HSCurses.Widgets (Widget(..), DrawingHint(..))
import Data.Tweet

data TweetWidget = TweetWidget { tweet :: Tweet }

instance Widget TweetWidget where
  minSize tweetWidget = (length . content . tweet $ tweetWidget, 3)
  draw (y, x) (w, h) hint tweetWidget =
    do move y x
       drawLine w borderChar
       move (y + 1) (x + 1)
       wAddStr stdScr $ content . tweet $ tweetWidget
       move (y + h - 1) x
       drawLine w borderChar
       return ()
    where
      borderChar = case hint of
        DHNormal -> repeat '-'
        _        -> repeat '='

data TweetsWidget = TweetsWidget { tweets :: [Tweet] }

instance Widget TweetsWidget where
  minSize tweetsWidget = foldl1 (\(w1, h1) (w2, h2) -> (max w1 w2, h1 + h2)) sizes
    where sizes = map minSize $ map TweetWidget $ UI.TweetsWidget.tweets tweetsWidget

  draw (y, x) (w, h) _ tweetsWidget = drawTweets (UI.TweetsWidget.tweets tweetsWidget) y
    where
      drawTweets [] _ = return ()
      drawTweets (t:ts) y' = do draw (y', x) (w, 3) DHNormal $ TweetWidget t
                                drawTweets ts $ y' + 3
