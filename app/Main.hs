{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import GHC.Generics (Generic)

import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Csv
import Data.Time
import Data.Time.Format.ISO8601
import Text.Read

data Person
  = Name String
  | PNumber String
  deriving Show

data WAHeader
  = Datetime Day TimeOfDay
  deriving Show

data Entry
  = Message WAHeader Person String
  | Meta WAHeader String
  deriving Show

data RawEntry
  = RawMessage WAHeader Person String
  | RawMeta WAHeader String
  | RawContinuation String
  deriving Show

data Row = Row
  { rowType:: !String
  , day::  !String
  , time:: !String
  , sender:: !String
  , message:: !String
  } deriving (Show, Eq, Generic)

instance DefaultOrdered Row
instance FromNamedRecord Row
instance ToNamedRecord Row

parseWAHeader :: String -> Maybe (WAHeader, String)
parseWAHeader line = do
    (_, msg) <- strs
    d <- date
    t <- time
    pure (Datetime d t, msg)
    where
        strs :: Maybe (String, String)
        strs = case break (== '-') line of
            (s, '-' : ' ' : msg) | length s == 18 -> Just (s, msg)
            _ -> Nothing

        dateTime :: Maybe (String, String)
        dateTime = do
            (h, _) <- strs
            case break (== ',') h of
                (dayStr, ',': ' ' : timeStr) -> Just (dayStr, timeStr)
                _ -> Nothing

        date_ints :: Maybe (Int, Int, Integer)
        date_ints = do
            (dateStr, _) <- dateTime
            (dd, rest) <- case break (== '/') dateStr of
                (dd@[_, _], '/':rest) | all isDigit dd -> (, rest) <$> readMaybe dd
                _ -> Nothing

            (mm, rest') <- case break (== '/') rest of
                (mm@[_,_], '/':rest') | all isDigit mm -> (, rest') <$> readMaybe mm
                _ -> Nothing

            case rest' of
              yyyy@[_,_,_,_] | all isDigit yyyy -> (dd, mm, ) <$> readMaybe yyyy
              _ -> Nothing

        time_ints :: Maybe (Int, Int)
        time_ints = do
            (_, timeStr) <- dateTime
            case break (== ':') timeStr of
                (hh@[_,_], ':':m1:m2:" ") | all isDigit (hh<>(m1 : [m2])) -> do
                    hour <- readMaybe hh
                    minutes <- readMaybe (m1 : [m2])
                    pure (hour, minutes)
                _ -> Nothing

        date :: Maybe Day
        date = do
          (dd, mm, yyyy) <- date_ints
          Just (fromGregorian yyyy mm dd)

        time :: Maybe TimeOfDay
        time = do
          (hh, mm) <- time_ints
          makeTimeOfDayValid hh mm 0

matchNumber :: String -> Bool
matchNumber (x:xs) = x `elem` " +()-" <> ['0'..'9'] && matchNumber xs
matchNumber [] = True

parseSender :: String -> Maybe (Person, String)
parseSender s = case break (== ':') s of
    (number, ':':' ':message) | matchNumber number -> Just (PNumber number, message)
    (name, ':' : ' ' :message) -> Just (Name name, message)
    _ -> Nothing

parseLine :: String -> RawEntry
parseLine  s = case parseWAHeader s of
  Just (header, message) -> case parseSender message of
      Just (sender, message2) -> RawMessage header sender message2
      _ -> RawMeta header message
  _ -> RawContinuation s


mergeContinues :: [RawEntry] -> [Entry]
mergeContinues (x@RawMessage {} : RawContinuation s : RawContinuation s2 : xs) = mergeContinues $ x : RawContinuation (s <> s2) : xs
mergeContinues (RawMessage h p s : RawContinuation s2 : xs) = Message h p (s <> s2) : mergeContinues xs
mergeContinues (RawMessage h p s : xs) = Message h p s : mergeContinues xs
mergeContinues (RawMeta h s : xs) = Meta h s : mergeContinues xs
mergeContinues [] = []
mergeContinues (x : y : xs) = error ("Unexpected sequence of lines" ++ show x ++ show y)


personToName :: Person -> String
personToName (Name s) = s
personToName (PNumber s) = s

toRows :: [Entry] -> [Row]
toRows (Message (Datetime d t) p s : xs) = Row "message" (showGregorian d) (iso8601Show t) (personToName p) s : toRows xs
toRows (Meta (Datetime d t) s: xs) = Row "notice" (showGregorian d) (iso8601Show t) "" s : toRows xs
toRows [] = []


main :: IO ()
main = do
  alltext <- getContents
  BS.putStr (encodeDefaultOrderedByName  $ toRows $  mergeContinues (fmap parseLine (lines alltext)))
  pure ()
