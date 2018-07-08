module MyModule where

jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 $ lines wocky
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines lines = do
  let first_line = extractFirstLines lines
  putStrLn $ unlines first_line
  return $ length first_line

extractFirstLines :: [String] -> [String]
extractFirstLines [] = []
extractFirstLines [_] = []
extractFirstLines ("" : first : rest) = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest

