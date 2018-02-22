-- module wikibook

{-
main = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you")
-}

{-
doGuessing num = do
  putStrLn "Enter your guess: "
  guess <- getLine
  if (read guess) < num
    then do putStrLn "Too Low"
            doGuessing num
    else if (read guess) > num
         then do putStrLn "Too High"
                 doGuessing num
         else do putStrLn "You Win!"
-}
doGuessing num = do
  putStrLn "Enter your guess: "
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too Low"
             doGuessing num
    GT -> do putStrLn "Tow High"
             doGuessing num
    EQ -> do putStrLn "You Win!"

main = do
  x <- getX
  putStrLn x

getX = do
  return "Hello"
  return "aren't"
  return "there"
  return "returns"
  return "rather"
  return "pointless?"
