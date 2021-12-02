module Lib
    (
        Command(Forward,Down,Up),
        units,
        parseLineToCommand,
        delta,
        finalPosition
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

finalPosition :: [Command] -> (Int, Int)
finalPosition commands = foldr1 pairAdd $ map delta commands
