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

blueCircle :: Picture
blueCircle = Color blue (Circle 100)

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 100)

wholePicture :: Picture
wholePicture =
  Pictures
    [ Translate (-120) 0 blueCircle,
      Translate 120 0 redDisk
    ]

mainWholePicture :: IO ()
mainWholePicture = display displayMode white wholePicture

mainCh13 :: IO ()
mainCh13 = display displayMode white axes
