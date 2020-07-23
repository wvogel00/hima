module Hima
    ( playHima
    ) where

import Hima.Type
import Hima.Image
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Char (toUpper)
import Data.List (unfoldr)
import Data.Maybe
import Debug.Trace (trace)

framerate = 24

playHima :: IO ()
playHima = do
    -- 文字が出現するレーン列を生成
    let himaRs = map setKind . randomRs (1,5) $ mkStdGen 21
    let laneRs = map setLane . randomRs (1,10) $ mkStdGen 42
    xs <- getImages
    play window white framerate
        initStatus{imgs = xs, nextLanes = zip laneRs himaRs}
            drawStatus dealEvent updateStatus

window :: Display
window = InWindow "hima" (width, height) (300, 0)

hitBorder = 0.02

drawStatus :: Status -> Picture
drawStatus status = Pictures [gameFrame, himaPics, dollPic] where
    himaPics = Pictures $ map (drawHima (imgs status)) (himas status) 
    dollPic = drawText "doll"

drawHima imgs h@(_,y,Empty) = Blank
drawHima imgs h@(_,y,l) = Translate (laneX l) y $ atatchImageHima h imgs

gameFrame = Pictures $ [vertBars, horiBars, chars, scoreboard, hitLine] where
    hitLine = Color red $ Line [(left,toY 0.02),(right,toY hitBorder)]
    vertBars = toLines $ zip (map toPos $ zip [0..4] [0,0..]) (map toPos $ zip [0..4] [1,1..])
    horiBars = Pictures [Line [(left,top),(right,top)],Line [(left,bottom),(right,bottom)] ]
    chars = Pictures [drawZ,drawX,drawC,drawV]
    drawZ = Translate (laneX Z) (toY (-0.04)) $ drawText "z"
    drawX = Translate (laneX X) (toY (-0.04)) $ drawText "x"
    drawC = Translate (laneX C) (toY (-0.04)) $ drawText "c"
    drawV = Translate (laneX V) (toY (-0.04)) $ drawText "v"
    scoreboard = Translate (toX 5.2) (toY 0.5) $ drawText "Score:"

dealEvent (EventKey key Down _ _) status = case key of
    Char c -> updateStatusByEvent c status
    _ -> status
dealEvent _ status = status

-- フレーム毎の更新
updateStatus :: Float -> Status -> Status
updateStatus t status = status{
        himas = hima'++freefall (himas status)
        , doll = doll'
        , miss = miss'
        , nextLanes = nextLanes'
        , frameCnt = frameCnt'
        } where
    -- 1秒に1回，次の文字を足すかどうか判定
    frameCnt' = (1 + frameCnt status) `mod` framerate
    hima' = if frameCnt' == 0 then [toObj.head $ nextLanes status] else []
    nextLanes' = if frameCnt' == 0 then tail (nextLanes status) else nextLanes status
    -- ボーダー以下の文字を削除
    freefall = filter ((>= toY hitBorder).getPos) . map (addPos (-2))
    unhit (k,y,l) = k == Hima && y <= toY (-hitBorder)
    miss' = if any unhit (himas status) then miss status + 1 else miss status
    doll' = if miss' < 10 then (P100, nextPose $ doll status)
                    else if miss' < 50 then (P50, nextPose $ doll status)
                            else (P20, nextPose $ doll status)
    toObj (lane, kind) = (kind, top, lane)

-- 当たり判定による状態更新
updateStatusByEvent c status = status{himas = newHimas, score = newScore} where
    hit c (k,y,l) = k == Hima && show l == [toUpper c] && y <= toY (hitBorder*3) && toY (-hitBorder) <= y
    newHimas = filter (not.hit c) $ himas status
    newScore = score status + (length $ himas status) - length newHimas


-- 乱数に文字情報・レーン情報を付与
setKind :: Int -> Kind
setKind v = fromJust $ lookup v $ zip [1..] [Hima ..]

setLane :: Int -> Lane
setLane v = fromJust $ lookup v $ zip [1..] $ [Z ..] ++ repeat Empty

-- レーンの型情報からX座標を取得
laneX Z = toX 0.5
laneX X = toX 1.5
laneX C = toX 2.5
laneX V = toX 3.5
laneX Empty = undefined

-- ポーズの更新
nextPose (_,A) = B
nextPose (_,B) = A

toX x = x*(right-left)/4 + left
toY y = y*(top-bottom)+bottom
toPos (x,y) = (toX x, toY y)

getPos :: HimaObj -> Float
getPos (_,y,_) = y
addPos :: Float -> HimaObj -> HimaObj
addPos v (k,y,l) = (k,y+v,l)

toLines poses = Pictures $ map (\(p1,p2) -> Line [p1,p2]) poses

drawText = Translate (-150) (-10).Scale 0.25 0.25 . Text