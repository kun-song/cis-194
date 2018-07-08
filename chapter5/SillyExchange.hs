module MyModule where

main :: IO ()
main = do
  putStrLn "Hello, user!"
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Hi " ++ name ++ "!"
