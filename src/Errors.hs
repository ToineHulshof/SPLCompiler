module Errors where

import Data.Array

type Positioned a = ((Int, Int), a)
type P = Positioned String

z :: a -> Positioned a
z a = ((0, 0), a)

-- Code is the list of chars in a program including its position, where the integers are the line and column respectively
type Code = [Positioned Char]

-- Error is a datatype to store an error message as a String with its position, where the integers are the line and column respectively
data Error = Error ErrorKind String P deriving (Eq, Show)

data Errors = Errors FilePath (Array Int String) [Error]

data ErrorKind = ParseError | TypeError deriving Eq

instance Show ErrorKind where
  show ParseError = "Parse error: "
  show TypeError = "Type error: "

-- Joins a list of strings with a given seperator
join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x : xs) = x ++ s ++ join s xs

removePath :: FilePath -> FilePath
removePath f = reverse $ takeWhile (/= '/') (reverse f)

instance Show Errors where
  show (Errors file lines errors) = join "\n\n" $ map showError errors where
    showError (Error k e ((li, co), c)) =
      "\x1b[1m" ++ removePath file ++ ":" ++ show li ++ ":" ++ show co ++ ": \x1b[31merror:\x1b[0m\x1b[1m " ++ show k ++ e ++ "\n" ++
      replicate leftLength ' ' ++ "\x1b[34m|\n" ++ show li ++ " |\x1b[0m " ++
      let (l, r) = splitAt (co - 1) (lines ! li) in (l ++ "\x1b[31m\x1b[1m" ++ c ++ "\x1b[0m" ++ drop (length c) r) ++ "\n" ++
      replicate leftLength ' ' ++ "\x1b[34m\x1b[1m|\x1b[0m" ++ replicate co ' ' ++ "\x1b[31m\x1b[1m" ++ replicate (length c) '^' ++ "\x1b[0m" where
      leftLength = length (show li) + 1
