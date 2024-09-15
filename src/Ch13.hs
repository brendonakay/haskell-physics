module Ch13 where

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

axes :: Picture
axes =
  Pictures
    [ Color red $ Line [(0, 0), (100, 0)],
      Color green $ Line [(0, 0), (0, 100)]
    ]

mainCh13 :: IO ()
mainCh13 = display displayMode black axes
