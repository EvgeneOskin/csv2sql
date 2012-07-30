module SRC.App.SqlMaker
    (
     connectorOnce,
     separateStringFirst,
     separateStringMulti,
     dataToStr
    ) where

    import Data.Time
    import Data.Time.Format
    import System.Locale
    
    connectorOnce x = 
        "INSERT INTO prices (ticker, date, open, high, low, clone, volume) VALUES " ++ x ++";"
              
    separateStringFirst _ [] = []                                            
    separateStringFirst f (x:xs)
        | f x = []
        | otherwise = x:separateStringFirst f xs
                   
    separateStringMulti _ [] = []                         
    separateStringMulti f xs = lx : separateStringMulti f lxs                  
        where
          lx = separateStringFirst f xs
          lxs = drop (length lx + 1) xs

    makeGoodDate x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
        where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime
                                   
    dataToStr [a0,a1,a2,a3,a4,a5,a6] = "(\'" ++ a0 ++ "\'," ++
                                       "\'" ++ (makeGoodDate a1 "%d-%b-%Y" "%Y-%m-%d %H:%M:%S") ++"\'," ++
                                       "\'" ++ a2 ++ "\'," ++
                                       "\'" ++ a3 ++ "\'," ++
                                        "\'" ++ a4 ++ "\'" ++
                                       "\'" ++ a5 ++ "\'," ++
                                        "\'" ++ a6 ++ "\')"
