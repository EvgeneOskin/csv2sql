module SRC.App.MyIO( mainConvertor
                   , convertOneFileCsvToSql
                   , mainConvertLoop
                   , dirSearchForCsvFiles
                   , isCsvFile
                   , processArgs
                   , run
) where

import System.Environment (getArgs)
import SRC.App.SqlMaker
import System.IO
import Control.Monad (forM)
import System.Directory
import System.FilePath ((</>))
import System.Console.GetOpt
import Data.List.Split
import Text.Regex.Posix
import Data.List

processArgs :: [String] -> IO (Options, [String])
processArgs argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: csv2sql [OPTION...]"

data Options = Options
 { optIn   :: String
 , optOut  :: String
 , optHelp :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions  = Options
 { optIn   = ""
 , optOut  = ""
 , optHelp = False
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['I'] ["input"]
              (ReqArg (\ f opts -> opts { optIn     = f })
                          "STR") "input path"
 , Option ['O'] ["output"]
              (ReqArg (\ f opts -> opts { optOut     = f })
                          "STR") "output path"
 , Option ['H'] ["help"]
               (NoArg (\   opts -> opts { optHelp        = True }))
               "help"
  ]

run :: (Options, [String]) -> IO Bool
run (Options _ _ True, _)  = do
  let header = "Usage: csv2sql [OPTION...]"
  putStrLn $ usageInfo header options
  return True
run (Options inFileName outFileName _, _) = mainConvertor inFileName outFileName

mainConvertor :: String -> String -> IO Bool
mainConvertor inPath outPath = do --choice what user type as "input path" dir or file
  isDir <- System.Directory.doesDirectoryExist inPath
  if isDir
    then do
      dirSearchForCsvFiles inPath outPath
      return True
    else do
      isCSV <- isCsvFile inPath
      if isCSV
        then convertOneFileCsvToSql inPath outPath
        else return False

convertOneFileCsvToSql :: String -> String -> IO Bool
convertOneFileCsvToSql input output = do
  inh <- openFile input ReadMode
  oth <- openFile output WriteMode
  headString <- hGetLine inh
  if any (\ a-> a=~"[0-9]+.[0-9]*" :: Bool) $ endBy "," headString
    then convertAndPutOneStringToSql headString inh oth
    else mainConvertLoop inh oth
  hClose inh
  hClose oth
  return True

mainConvertLoop :: Handle -> Handle -> IO ()
mainConvertLoop inh oth = do 
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inpStr <- hGetLine inh
      convertAndPutOneStringToSql inpStr inh oth
                                  
convertAndPutOneStringToSql inpStr inh oth= do
  let sqlrow = SRC.App.SqlMaker.convertOneStringScvToSql $ SRC.App.SqlMaker.dataToStr splitedStr
      splitedStr = endBy "," $ filter (`notElem` "\r\n") inpStr
  hPutStrLn oth sqlrow
  mainConvertLoop inh oth
                      
dirSearchForCsvFiles :: String -> String -> IO [Bool]
dirSearchForCsvFiles topDirI topDirO = do
  topDirContents <- System.Directory.getDirectoryContents topDirI
  let properNames = filter (`notElem` [".",".."]) topDirContents
  forM properNames $ \name -> do
                              let pathI = topDirI </> name
                                  pathO = topDirO </> name
                              isDir <- System.Directory.doesDirectoryExist pathI
                              if isDir
                                then do
                                  dirSearchForCsvFiles pathI pathO
                                  return True
                                else do
                                  isCSV <- isCsvFile pathI
                                  if isCSV
                                    then do
                                      System.Directory.createDirectoryIfMissing True topDirO
                                      convertOneFileCsvToSql pathI (csvFileNameToSqlFileName pathO)
                                    else return False

isCsvFile :: String -> IO Bool
isCsvFile x = return $ ".csv" `isSuffixOf` xStr
    where xStr = x :: String

