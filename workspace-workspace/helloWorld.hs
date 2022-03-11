main = do
    putStr "What is your name? "
    name <- getLine
    putStrLn $ "Welcome " ++ name
