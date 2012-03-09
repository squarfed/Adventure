> module Input (listen,yes) where

> import Data.Char (toLower)
> import System.IO(hFlush,stdout)


> yes :: String -> String -> String -> IO (Bool)
> yes q y n = loop
>     where
>       loop = do putStr "\n** "
>                 hFlush stdout
>                 l <- map toLower `fmap` getLine
>                 case l of
>                   "yes" -> putStrLn y >> return True
>                   "no" ->  putStrLn n >> return False
>                   _ -> putStrLn " Please answer Yes or No." >> loop

> listen :: IO (String,String)
> listen = loop
>     where
>       loop = do
>         putStr "* "
>         hFlush stdout
>         ws <- words `fmap` getLine
>         case ws of
>           []      -> putStrLn " Tell me to do something." >> loop
>           [w1]    -> return (w1,[])
>           [w1,w2] -> return (w1,w2)
>           _       -> putStrLn " Please stick to 1- and 2-word commands.">> loop
