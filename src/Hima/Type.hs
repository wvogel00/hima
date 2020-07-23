module Hima.Type where

import Graphics.Gloss

data Status = Status{
    score :: Int,
    himas :: [HimaObj], 
    nextLanes :: [(Lane, Kind)],
    doll :: (Power,Pose),
    imgs :: [Image],
    miss :: Int,
    frameCnt :: Int
}

initStatus = Status {
    score = 0,
    himas = [(Hima, top, Z)],
    nextLanes = [],
    doll = (P100,A),
    imgs = [],
    miss = 0,
    frameCnt = 0
}

width = 640 :: Int
height = 480 :: Int

left = -260 :: Float
right = 260 :: Float
top = 200 :: Float
bottom = -200 :: Float

data Image = Doll Power Pose Picture | Character Kind Picture | NoImage deriving Eq

type HimaObj = (Kind, Height, Lane)
type Doll = (Power,Pose)

data Lane = Z | X | C | V | Empty deriving (Eq, Show, Enum)

type Height = Float

data Kind = Hima | Ashi | Kizu | EbiA | EbiB deriving (Eq,Show, Enum)

data Power = P100 | P50 | P20 deriving (Eq,Show)

data Pose = A | B deriving (Eq, Show)