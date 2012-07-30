module SRC.App.MyIO
    (
      input_greeting,
      convertChoice,
      convertSingle,
      mainloop,
      dirSearch,
      isCSVFile,
      nameCSVtoSQL
    ) where

    import System.Environment (getArgs)
    import SRC.App.SqlMaker
    import System.IO
    import Text.Regex.Posix
    import Control.Monad (forM)
    import System.Directory
    import System.FilePath ((</>))
    import System.Console.GetOpt

        
    input_greeting = do
      args <- getArgs
      case getOpt RequireOrder options args of
        (flags, nonOpts, msgs) ->do
                       print flags
                       getFileNames flags

    data Flag = IN String
              | OUT String
              deriving Show
                
    options = [ Option ['I'] [] (ReqArg IN "FILE") "input",
              Option ['O'] [] (ReqArg OUT "FILE") "output"]

    getFileNames [(IN inFileName), (OUT outFileName)] = convertChoice inFileName outFileName
    getFileNames [(OUT outFileName), (IN inFileName)] = convertChoice inFileName outFileName

    convertChoice inPath outPath = do
      isDir <- System.Directory.doesDirectoryExist inPath
      if isDir
        then do
          dirSearch inPath outPath
          return True
        else do
          isCSV <- isCSVFile inPath
          if isCSV
            then convertSingle inPath outPath
            else return (False)
             
    convertSingle input output = do
      inh <- openFile input ReadMode
      oth <- openFile output WriteMode
      headString <- hGetLine inh
      mainloop inh oth
      hClose inh
      hClose oth
      return True
                      
    mainloop inh oth = do 
      ineof <- hIsEOF inh
      if ineof
        then return ()
        else do
          inpStr <- hGetLine inh
          let
              sqlrow = SRC.App.SqlMaker.connectorOnce (SRC.App.SqlMaker.dataToStr (SRC.App.SqlMaker.separateStringMulti (==',') inpStr))
          hPutStrLn oth sqlrow
          mainloop inh oth
        
    dirSearch topDirI topDirO = do
      topDirContents <- System.Directory.getDirectoryContents topDirI
      let properNames = filter (`notElem` [".",".."]) topDirContents
      forM properNames $ \name -> do
            let pathI = topDirI </> name
                pathO = topDirO </> name
            isDir <- System.Directory.doesDirectoryExist pathI
            if isDir
              then do
                System.Directory.createDirectoryIfMissing True pathO
                dirSearch pathI pathO
                return True
              else do
                isCSV <- isCSVFile pathI
                if isCSV
                  then convertSingle pathI (nameCSVtoSQL pathO)
                  else return False
                  
    isCSVFile x = return (xStr =~ "(.)+[.]csv" :: Bool)
        where xStr = x :: String

    nameCSVtoSQL x =  take (length x - 3) x  ++ "sql"
