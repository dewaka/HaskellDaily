-- Novel Compression problem
-- http://www.reddit.com/r/dailyprogrammer/comments/25clki/5122014_challenge_162_easy_novel_compression_pt_1/

import Data.Char

data Word = Lower Int
           | Upper Int
           | Capcase Int deriving (Show)

data Punctuation = Hyphen | Stop | Question | Bang | SemiColon | Colon

data ControlChar = Dash | NewLine

instance Show ControlChar where
  show Dash = "-"
  show NewLine = "E"

instance Show Punctuation where
  show Hyphen = "!"
  show Stop = "."
  show Question = "?"
  show Bang = "!"
  show SemiColon = ";"
  show Colon = ":"

data CData = WData Word | PData Punctuation | CltData ControlChar
           deriving (Show)

upCaseWord :: String -> String
upCaseWord = map toUpper

lowCaseWord :: String -> String
lowCaseWord = map toLower

capCase :: String -> String
capCase "" = ""
capCase (c:cs) = toUpper c: cs

decompress :: [String] -> [CData] -> String
decompress _ [] = ""
decompress words (WData (Lower n):CltData Dash:xs)
  =  (lowCaseWord $ words !! n) ++ "-" ++ decompress words xs
decompress words (WData (Upper n):CltData Dash:xs)
  = (upCaseWord $ words !! n) ++ "-" ++ decompress words xs
decompress words (WData (Capcase n):CltData Dash:xs)
  = (capCase $ words !! n) ++ "-" ++ decompress words xs
decompress words (WData (Lower n):xs)
  = (lowCaseWord $ words !! n) ++ " " ++ decompress words xs
decompress words (WData (Upper n):xs)
  = (upCaseWord $ words !! n) ++ " " ++ decompress words xs
decompress words (WData (Capcase n):xs)
  = (capCase $ words !! n) ++ " " ++ decompress words xs
decompress words (PData p:xs)
  = show p ++ " " ++ decompress words xs


readCData :: String -> [CData]
readCData str = readCData' str [] False
  where
    toInt s = read s :: Int

    readCData' [] nstr b = if nstr==[]
                           then []
                           else [WData $ Lower (toInt nstr)]
    readCData' ('^':xs) nstr b = WData (Capcase (toInt nstr)) : readCData' xs [] False
    readCData' ('!':xs) nstr b = if b
                                 then PData Bang : readCData' xs [] False
                                 else WData (Upper (toInt nstr)) : readCData' xs [] False
    readCData' (' ':xs) nstr b = WData (Lower $ toInt nstr) : readCData' xs [] True

    readCData' ('.':xs) nstr b = PData Stop : readCData' xs nstr False
    readCData' (';':xs) nstr b = PData SemiColon : readCData' xs nstr False
    readCData' (':':xs) nstr b = PData Colon : readCData' xs nstr False
    readCData' (s:xs) nstr b = readCData' xs (nstr ++ [s]) False

testWords = ["is","my","hello","name","stan"]


main = do
    putStrLn "hello there"
