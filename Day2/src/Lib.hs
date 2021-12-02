module Lib
    (
        Command(Forward,Down,Up),
        units,
        parseLineToCommand,
        delta,
        finalPosition,
        deltaAim,
        aimwiseApply,
        finalAimPosition
    ) where

import Data.List.Extra
import Data.Tuple.Extra

data Command = Forward Int | Down Int | Up Int deriving (Eq, Show)

parseCommand :: String -> Int -> Command
parseCommand "forward" u = Forward u
parseCommand "down" u = Down u
parseCommand "up" u = Up u

parseLineToCommand :: String -> Command
parseLineToCommand x = uncurry parseCommand $ second read $ word1 x

units :: Command -> Int
units (Forward x) = x
units (Down x) = x
units (Up x) = x

delta :: Command -> (Int, Int)  -- (horiz pos, depth)
delta (Forward x) = (x, 0)
delta (Down x) = (0, x)
delta (Up x) = (0, -x)

pairAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pairAdd (a, b) (c, d) = (a + c, b + d)

deltaAim :: Command -> (Int, Int, Int)  -- (horiz pos, depth multiplier, aim)
deltaAim (Forward x) = (x, x, 0)
deltaAim (Up x) = (0, 0, -x)
deltaAim (Down x) = (0, 0, x)

aimwiseApply :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
aimwiseApply (a, b, c) (d, e, f) = (a + d, b + (c * e), c + f)

finalPosition :: [Command] -> (Int, Int)
finalPosition commands = foldl1 pairAdd $ map delta commands

positionOnly :: (Int, Int, Int) -> (Int, Int)
positionOnly (x, y, _) = (x, y)

finalAimPosition :: [Command] -> (Int, Int)
finalAimPosition commands = positionOnly $ foldl aimwiseApply (0,0,0) $ map deltaAim commands
