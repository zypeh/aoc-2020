{-# language OverloadedStrings #-}
module Main where

import Z.Data.Text (Text)
import Z.Data.Vector.Base (Bytes, countBytes)
import qualified Z.Data.Parser as Parser
import GHC.Word (Word8)
import Z.Data.ASCII
import Control.Applicative (many)
import Data.Either (fromRight)

input :: Bytes
input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

main :: IO ()
main = case parsePassword input of
  Left err -> print err
  Right results -> print $ foldr (\isValid -> if isValid then (+1) else id) 0 (checkPassword <$> results)

checkPassword :: Res -> Bool
checkPassword res = let n = countBytes (_chr res) (password res) in n >= _min res && n <= _max res

data Res = Res
  { _chr     :: Word8
  , _min     :: Int
  , _max     :: Int
  , password :: Bytes
  } deriving (Show)

ws :: Word8 -> Bool
ws w = w == SPACE || w == NEWLINE
{-# inline ws #-}

skipSpaces :: Parser.Parser ()
skipSpaces = Parser.skipWhile ws
{-# INLINABLE skipSpaces #-}

parsePassword :: Bytes -> Either Parser.ParseError [Res]
parsePassword = Parser.parse' $ many (skipSpaces *> limits <* skipSpaces) <* Parser.endOfInput
  where
    limits :: Parser.Parser Res
    limits = do
      min' <- Parser.int
      Parser.skipWord8 -- '-'
      max' <- Parser.int
      skipSpaces
      chr' <- Parser.anyWord8
      Parser.skipWord8 -- ':'
      skipSpaces
      password' <- Parser.takeWhile (not . ws)
      pure (Res chr' min' max' password')
