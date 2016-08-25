wrap :: Int -> Int -> Int -> [String]
wrap width height depth = upper depth ++ bottom width height depth

bottom :: Int -> Int -> Int -> [String]
bottom width height depth = [replicate width '#' ++ replicate x '+' | x <- replicate (height - depth - 1) depth ++ [depth, (depth - 1)..0]]

upper :: Int -> [String]
upper depth = [replicate x ' ' ++ replicate 19 ':' ++ '/':[] ++ replicate (depth - x) '+' | x <- [depth, (depth - 1)..1]]

printResult :: [String] -> IO ()
printResult [] = putStrLn("Empty list")
printResult str_list = mapM_ putStrLn str_list
