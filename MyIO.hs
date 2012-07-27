module MyIO
    (
      input_greeting,
      ioData,
      interactWith,
      mainloop
    ) where

    import Database.HDBC
    import Database.HDBC.Sqlite3
    import System.Environment (getArgs)
    import DB_connector
    import System.IO
        
    input_greeting = do
      args <- getArgs
      case args of
        [input,output] -> interactWith input output
        _ -> putStrLn "error type in cmd line 2 file name"

    interactWith input output = do
      inh <- openFile input ReadMode
      conn <- Database.HDBC.Sqlite3.connectSqlite3 output
      headString <- hGetLine inh
      let
          head = DB_connector.makeTableHead (DB_connector.separateStringMulti (==',') headString)
      DB_connector.connectorCreater conn "t1" head
      mainloop inh conn "t1"
      hClose inh
      Database.HDBC.disconnect conn
                      
    mainloop inh conn table_name = do 
      ineof <- hIsEOF inh
      if ineof
      then return ()
      else do
        inpStr <- hGetLine inh
        let
            sqlrow = DB_connector.stringsToSql (DB_connector.separateStringMulti (==',') inpStr)
        connectorOnce conn table_name  sqlrow
        mainloop inh conn table_name
        
    ioData name_db headString contentString = do
      let 
          sqlContentMaker = map (DB_connector.stringsToSql)
          sqlContent = sqlContentMaker (DB_connector.makeContent contentString)
          head = DB_connector.makeTableHead (DB_connector.separateStringMulti (==',') headString)
      DB_connector.connectorAll name_db "table_1" head sqlContent
