module UI.TweetsWidget where

import UI.HSCurses.CursesHelper (drawLine)
import UI.HSCurses.Curses (stdScr, move, wAddStr)
import UI.HSCurses.Widgets (Widget(..), DrawingHint(..))
import Data.List.Zipper

import Data.Tweet

data TweetWidget = TweetWidget { tweetWidgetTweet :: Tweet }

instance Widget TweetWidget where
  minSize tweetWidget = (length . content . tweetWidgetTweet $ tweetWidget, 3)

  draw (y, x) (w, h) hint tweetWidget =
    do move y x
       drawLine w borderChar
       move (y + 1) (x + 1)
       wAddStr stdScr $ content . tweetWidgetTweet $ tweetWidget
       move (y + h - 1) x
       drawLine w borderChar
       return ()
    where
      borderChar = case hint of
        DHNormal -> repeat '-'
        _        -> repeat '='

data TweetsWidget = TweetsWidget { tweetsWidgetTweets :: Zipper Tweet }

instance Widget TweetsWidget where
  minSize tweetsWidget = foldl1 (\(w1, h1) (w2, h2) -> (max w1 w2, h1 + h2)) sizes
    where sizes = map minSize $ map TweetWidget $ toList $ tweetsWidgetTweets tweetsWidget

  draw (y, x) (w, h) _ tweetsWidget = drawTweets (tweetsWidgetTweets tweetsWidget) DHFocus y
    where
      drawTweets tweets hint y'
        | endp tweets = return ()
        | otherwise   = do let tw = TweetWidget $ cursor tweets
                               (_, h') = minSize tw
                           drawTweets (right tweets) DHNormal $ y' + h' - 1
                           draw (y', x) (w, h') hint tw
                           
