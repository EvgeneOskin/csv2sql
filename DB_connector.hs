module DB_connector
    (
     connectorAll,
     connectorCreater,
     connectorOnce,
     makeTableHead,
     separateStringFirst,
     separateStringMulti,
     makeContent,
     turpleToSql,
     stringsToTurple,
     stringsToSql
    ) where

    import Database.HDBC

    connectorAll conn table_name table_head xs = do
      Database.HDBC.run conn ("CREATE TABLE " ++ table_name ++  " (" ++ table_head ++ ")") []
      stmt <- Database.HDBC.prepare conn  ("INSERT INTO " ++ table_name ++ " VALUES (?, ?, ?, ?, ?, ?, ?)")
      Database.HDBC.executeMany stmt xs
      Database.HDBC.commit conn

    connectorCreater conn table_name table_head = do
      Database.HDBC.run conn ("CREATE TABLE " ++ table_name ++  " (" ++ table_head ++ ")") []
      Database.HDBC.commit conn

    connectorOnce conn table_name x = do
      stmt <- Database.HDBC.prepare conn  ("INSERT INTO " ++ table_name ++ " VALUES (?, ?, ?, ?, ?, ?, ?)")
      Database.HDBC.execute stmt x
      Database.HDBC.commit conn

              
    makeTableHead [a0,a1,a2,a3,a4,a5,a6] = a0 ++ " TEXT, " ++ a1 ++ " TEXT, "
                                            ++ a2 ++ " REAL, " ++ a3 ++ " REAL, "
                                            ++ a4 ++ " REAL, " ++ a5 ++ " REAL, " ++ a6 ++ " INT"

    separateStringFirst _ [] = []                                            
    separateStringFirst f (x:xs)
        | f x = []
        | otherwise = x:separateStringFirst f xs
                   
    separateStringMulti _ [] = []                         
    separateStringMulti f xs = lx : separateStringMulti f lxs                  
        where
          lx = separateStringFirst f xs
          lxs = drop (length lx + 1) xs

    makeContent xs = map (separateStringMulti (==',')) (separateStringMulti (=='\n') xs)
                              
    turpleToSql (a0,a1,a2,a3,a4,a5,a6) = [Database.HDBC.toSql a0,
                                          Database.HDBC.toSql a1,
                                          Database.HDBC.toSql a2,
                                          Database.HDBC.toSql a3,
                                          Database.HDBC.toSql a4,
                                          Database.HDBC.toSql a5,
                                          Database.HDBC.toSql a6
                                         ]

    stringsToTurple [a0,a1,a2,a3,a4,a5,a6] = (read ("\"" ++ a0 ++"\"") :: String,
                                              read ("\"" ++ a1 ++"\"") :: String,
                                              read a2 :: Double,
                                              read a3 :: Double,
                                              read a4 :: Double,
                                              read a5 :: Double,
                                              read a6 :: Integer
                                             )
                                             
    stringsToSql xs = turpleToSql (stringsToTurple xs)