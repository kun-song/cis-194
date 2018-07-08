{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- my imports
import qualified Data.List as L

-- Exercise 1 -----------------------------------------
-- result: "Haskell Is Great!"
getSecret :: FilePath -> FilePath -> IO ByteString
getSecret o_path m_path =
  do
    o_str <- BS.readFile o_path
    m_str <- BS.readFile m_path
    return $ decrypt o_str m_str

decrypt :: ByteString -> ByteString -> ByteString
decrypt key encrypted = BS.pack $ filter (/= 0) $ BS.zipWith xor key encrypted

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key dest_path =
  do
    encrypted <- BS.readFile $ dest_path ++ ".enc"
    let decrypted = decrypt (BS.cycle key) encrypted
    BS.writeFile dest_path decrypted

-- Exercise 3 -----------------------------------------
-- parse both JSON and String
parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path =
  do
    json_str <- BS.readFile path
    let jsons = Parser.decode json_str
    return $ jsons

-- Exercise 4 -----------------------------------------
-- getBadTs "./clues/victims.json" "./clues/transactions.json"
getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims_path trs_path =
  do
    victims <- parseFile victims_path :: IO (Maybe [TId])
    trs     <- parseFile trs_path :: IO (Maybe [Transaction])
    return $ filter (\t -> p t victims) <$> trs
    where p _ Nothing     = False
          p t (Just tids) = tid t `elem` tids

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow []       = Map.empty
getFlow (t : ts) =
  Map.insert (to t) (amount t) $ Map.insert (from t) (negate $ amount t) (getFlow ts)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flow = let max_amout = maximum $ map (\(_, v) -> v) $ Map.toList flow
                   in
                     head $ Map.keys $ Map.filter (== max_amout) flow

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow tids = let (payersRaw, payeesRaw) = Map.partition (> 0) flow
                       payersSorted = L.sortBy (\(_, v1) (_, v2) -> v1 `compare` v2) $ Map.toList payersRaw
                       payeesSorted = L.sortBy (\(_, v1) (_, v2) -> v1 `compare` v2) $ Map.toList payeesRaw
                   in
                     aux payersSorted payeesSorted tids
                   where aux [] _  _              = []
                         aux _ [] _               = []
                         aux _ _ []               = []
                         aux ((_, 0) : ps) qs ids = aux ps qs ids
                         aux ps ((_, 0) : qs) ids = aux ps qs ids
                         aux ((p, m1) : ps) ((q, m2) : qs) (id : ids) =
                           let min_amount = m1 `min` (abs m2)
                               tr = Transaction {from = q, to = p, amount = min_amount, tid = id}
                           in
                             tr : (aux ((p, m1 - min_amount) : ps) ((q, m2 + min_amount) : qs) ids)

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path trs = BS.writeFile path $ encode trs

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
