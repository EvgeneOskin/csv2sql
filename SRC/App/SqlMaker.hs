module SRC.App.SqlMaker( convertOneStringScvToSql
                       , changeDateFormat
                       , dataToStr
                       , csvFileNameToSqlFileName
) where

import Data.Time
import Data.Time.Format
import System.Locale
import Text.Regex.Posix
import Data.List.Split

convertOneStringScvToSql :: String -> String
convertOneStringScvToSql x = 
    "INSERT INTO prices (ticker, date, open, high, low, close, volume) VALUES " ++ x ++";"

changeDateFormat :: String -> String -> String -> String
changeDateFormat x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
    where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime

dataToStr :: [String] -> String
dataToStr [a0,a1,a2,a3,a4,a5,a6] = "(\'" ++ a0 ++ "\'," ++
                                   "\'" ++ (changeDateFormat a1 "%d-%b-%Y" "%Y-%m-%d %H:%M:%S") ++"\'," ++
                                   "\'" ++ a2 ++ "\'," ++
                                   "\'" ++ a3 ++ "\'," ++
                                   "\'" ++ a4 ++ "\'" ++
                                   "\'" ++ a5 ++ "\'," ++
                                   "\'" ++ a6 ++ "\')"

csvFileNameToSqlFileName :: String -> String
csvFileNameToSqlFileName x = head (endBy "." x)  ++ ".sql"
