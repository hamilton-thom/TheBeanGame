
import Game

main :: IO ()
main =
  do
    x <- printWelcome
    putStrLn x
    return ()

welcomeMessage :: String
welcomeMessage = "Welcome to the Bean Game. To begin please \
                 \enter the number of players which playing today."

printWelcome :: IO String
printWelcome =
  do
    putStrLn welcomeMessage
    x <- distinctNewString initialList
    return x

initialList :: IO [String]
initialList = pure ["This", "is", "an", "example", "list"]

{-
getPlayerNames :: (Int, Int) -> IO [String]
getPlayerNames (_, 0) = return []
getPlayerNames (n, m) =
  do
    putStrLn "Please enter player number " + (n - m + 1) + "'s name: "
    name <- distinctNewString getPlayerNames (n, m - 1)
    return

-}

distinctNewString :: IO [String] -> IO String
distinctNewString ioList =
  do
    xs <- ioList
    attempt <- getLine
    case attempt `elem` xs of
      True -> do
                putStrLn ("\"" ++ attempt ++ "\" has already been entered, please try again.")
                output <- distinctNewString ioList
                return output
      False -> return attempt
