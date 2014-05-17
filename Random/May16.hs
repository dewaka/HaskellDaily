-- Novel Compression problem
-- http://www.reddit.com/r/dailyprogrammer/comments/25clki/5122014_challenge_162_easy_novel_compression_pt_1/

data Chunk = Lower Int
           | Upper Int
           | Capcase Int deriving (Show)

data ControlChar = Hyphen | Stop | Question | Bang | SemiColon | Colon

instance Show ControlChar where
  show Hyphen = "!"
  show Stop = "."
  show Question = "?"
  show Bang = "!"
  show SemiColon = ";"
  show Colon = ":"

main = do
    putStrLn "hello there"
