{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}
module Main (main) where

import Text.HTML.DOM as H
import Text.Read (reads)
import Text.Parsec
import Text.Parsec.String
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH

import Control.Monad (mapM_)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.Conduit
import Data.Conduit.Combinators as Combinators (stdin)
import Data.Either
import Data.Maybe
import Data.Text (Text, strip, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time

import GHC.Generics

data Transaction = Transaction { trxnDate :: Day, trxnCode :: Int, trxnCodeStr :: String, trxnAmount :: Float, trxnBalance :: Float, trxnDesc :: Maybe String } deriving (Generic, Show)
instance ToJSON Transaction

code2string :: Int -> String
code2string x
  | x == 109  = "VISA Refund"
  | x == 110  = "Interest Credit"
  | x == 151  = "VISA Payment"
  | x == 160  = "ATM Withdraw"
  | x == 163  = "Internal Transfer"
  | x == 242  = "Direct Credit"
  | x == 244  = "Osko Receive"
  | x == 284  = "Osko Payment"
  | x == 292  = "BPay"
  | x == 890  = "ATM Withdraw (Foreign)"
  | otherwise = "Unknown " ++ (show x)

dollarParser :: Parser (Char, Char, String)
dollarParser = (,,) <$> (option ' ' (char '-')) <*> (char '$') <*> (many anyChar)

str2float str = read str :: Float
str2int   str = read str :: Int

cursInnerText2str curs = unpack $ strip $ toStrict $ innerText curs

fromRightOnly :: Either a b -> b
fromRightOnly (Right b) = b
fromRightOnly _ = error "Left in fromRightO"

parseDollars :: String -> Float
parseDollars "" = error "Empty String!"
parseDollars str = 
  let parse_result = parse dollarParser "" str in
    let (sgn, _, floatstr) = fromRightOnly parse_result in
      str2float $ [sgn] ++ filter (/= ',') floatstr

parseDateCol :: Cursor -> Day
parseDateCol col =
  let dateCurs = head $ queryT [jq| div.createdate |] col in 
    let dateStr = cursInnerText2str dateCurs in
      fromJust (parseTimeM True defaultTimeLocale "%d/%m/%Y" dateStr :: Maybe Day)

cols2tuple a [b,c] = (a,b,c)

parseCols :: [Cursor] -> (Day, Float, Float)
parseCols [] = error "Empty cols"
parseCols (col1:cols) = 
  cols2tuple (parseDateCol col1) [ parseDollars $ cursInnerText2str c | c <- take 2 cols ]

parsePartialTransaction :: Cursor -> Transaction
parsePartialTransaction curs = 
  let (day, amount, balance) = parseCols (queryT [jq| div.trxn-summary > div.columns |] curs) in
    Transaction day (-1) "" amount balance Nothing

parseTransaction :: Cursor -> Transaction
parseTransaction curs = 
  let desc  =           unpack  $ head $ attribute "data-tran-desc" curs in 
  let code  = str2int $ unpack  $ head $ attribute "data-tran-code" curs in 
    (parsePartialTransaction curs) {trxnCode = code, trxnCodeStr = code2string code, trxnDesc = Just desc}

main :: IO ()
main = do
  -- root <- fmap fromDocument (H.readFile "Transaction History.html") -- Read from file
  root <- fmap fromDocument (runConduit $ Combinators.stdin .| H.sinkDoc) -- Pipe from stdin

  let cs = queryT [jq| div#trxns li[data-tran-desc] |] root
  let dats = [ parseTransaction c | c <- cs ]
  putStrLn $ BS.unpack $ encode dats