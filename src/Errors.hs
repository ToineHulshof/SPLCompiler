module Errors where

import Data.Array
import Data.List.NonEmpty ( NonEmpty ((:|)) )
import Data.List ( sort )

type Position = (Int, Int)
type Positioned a = (Position, a)
type P = Positioned String

-- Code is the list of chars in a program including its position, where the integers are the line and column respectively
type Code = [Positioned Char]

-- Error is a datatype to store an error message as a String with its position, where the integers are the line and column respectively
data Error = Error ErrorKind (NonEmpty String) (Maybe P) deriving (Eq, Show)

data Errors = Errors FilePath (Array Int String) [Error]

data ErrorKind = ParseError | TypeError | CodegenError deriving Eq

instance Ord Error where
  (Error _ _ (Just ((l1, c1), _))) <= (Error _ _ (Just ((l2, c2), _))) = if l1 == l2 then c1 <= c2 else l1 <= l2
  (Error _ _ (Just _)) <= (Error _ _ Nothing) = False
  (Error _ _ Nothing) <= (Error _ _ (Just _)) = True
  (Error _ _ Nothing) <= (Error _ _ Nothing) = True

instance Show ErrorKind where
  show ParseError = "Parse error: "
  show TypeError = "Type error: "
  show CodegenError = "Codegen error: "

-- Joins a list of strings with a given seperator
join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x : xs) = x ++ s ++ join s xs

nes :: a -> NonEmpty a
nes a = a :| []

removePath :: FilePath -> FilePath
removePath f = reverse $ takeWhile (/= '/') (reverse f)

removeSpace :: Char -> Char
removeSpace '\n' = ' '
removeSpace '\t' = ' '
removeSpace c = c

showErrorStrings :: NonEmpty String -> String
showErrorStrings (s :| ss) = s ++ concatMap ("\n  • " ++) ss

instance Show Errors where
  show (Errors file lines errors) = join "\n\n" $ map showError (sort errors) where
    showError (Error k e Nothing) = "\x1b[1m" ++ removePath file ++ ": \x1b[31merror:\x1b[0m\x1b[1m " ++ show k ++ showErrorStrings e
    showError (Error k e (Just ((li, co), c'))) = let c = map removeSpace c' in
      "\x1b[1m" ++ removePath file ++ ":" ++ show li ++ ":" ++ show co ++ ": \x1b[31merror:\x1b[0m\x1b[1m " ++ show k ++ showErrorStrings e ++ "\n" ++
      replicate leftLength ' ' ++ "\x1b[34m|\n" ++ show li ++ " |\x1b[0m " ++
      let (l, r) = splitAt (co - 1) (lines ! li) in (l ++ "\x1b[31m\x1b[1m" ++ c ++ "\x1b[0m" ++ drop (length c) r) ++ "\n" ++
      replicate leftLength ' ' ++ "\x1b[34m\x1b[1m|\x1b[0m" ++ replicate co ' ' ++ "\x1b[31m\x1b[1m" ++ replicate (length c) '^' ++ "\x1b[0m" where
      leftLength = length (show li) + 1
