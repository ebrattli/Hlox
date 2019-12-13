module ErrorHandler
    ( report
    )
where

report :: Integer -> String -> String -> IO ()
report line err message =
    putStrLn $ "[line " ++ show line ++ "] Error " ++ err ++ ": " ++ message

err :: Integer -> String -> IO ()
err = flip report ""
