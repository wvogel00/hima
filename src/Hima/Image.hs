module Hima.Image where

import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.List
import Hima.Type
import Debug.Trace (trace)

getImages = do
    files <- filter (isSuffixOf ".png") <$> getDirectoryContents "img"
    putStrLn "image files : "
    mapM_ putStrLn files
    xs <- fmap assignType . zip files <$> mapM loadJuicy (map ("img/" ++) files)
    return xs

atatchImageHima :: HimaObj -> [Image] -> Picture
atatchImageHima _ [] = Blank
atatchImageHima h@(k,_,_) (Character k' pic:imgs) = if k == k' then pic else atatchImageHima h imgs
atatchImageHima h (_:imgs) = atatchImageHima h imgs

atatchImageDoll :: Doll -> [Image] -> Picture
atatchImageDoll _ [] = trace "obj" Blank
atatchImageDoll d (Doll p a pic:imgs) = if fst d == p && snd d == a then pic else atatchImageDoll d imgs
atatchImageDoll d (_:imgs) = atatchImageDoll d imgs

assignType :: (FilePath, Maybe Picture) -> Image
assignType (file, Just pic)
    | "Hima" `isInfixOf` file =  Character Hima pic
    | "Kizu" `isInfixOf` file = Character Kizu pic
    | "EbiA" `isInfixOf` file = Character EbiA pic
    | "EbiB" `isInfixOf` file = Character EbiB pic
    | "Ashi" `isInfixOf` file = Character Ashi pic
    | "P100A" `isInfixOf` file = Doll P100 A pic
    | "P100B" `isInfixOf` file = Doll P100 B pic
    | "P50A" `isInfixOf` file = Doll P50 A pic
    | "P50B" `isInfixOf` file = Doll P50 B pic
    | "P20A" `isInfixOf` file = Doll P20 A pic
    | "P20B" `isInfixOf` file = Doll P20 B pic
    | otherwise = NoImage
assignType (file, Nothing) = trace (file ++ " is not exist") NoImage

getPicture :: Image -> Picture
getPicture (Doll _ _ pic) = pic
getPicture (Character _ pic) = pic
getPicture NoImage = Blank