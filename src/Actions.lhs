Actions
-------

> module Actions (Action(..),actions) where

> import Control.Monad.State
> import Data.Maybe (fromJust)


> data Action = Abstain | Take | Drop | Open | Close
>             | On | Off | Wave | Calm | Go | Relax
>             | Pour | Eat | Drink | Rub | Toss | Wake | Feed
>             | Fill | Break | Blast | Kill
>             | Say | Read | Feefie | Brief | Find
>             | Inventory | Score | Quit
>             deriving (Eq,Ord,Show)


> type ActionDictionary = [(String,Action)]


> type MessageMap = [(Action,String)]


> type ActionM = State (ActionDictionary,MessageMap)


> runActionM ::ActionM a -> (ActionDictionary,MessageMap)
> runActionM = flip execState ([],[])


> newWord word act = modify $ \(m1,m2) -> ((word,act):m1,m2)


> ditto :: String -> ActionM ()
> ditto word = do ((_,act):_,_) <- get
>                 newWord word act


> setMsg :: String -> ActionM ()
> setMsg msg = do (m1,m2) <- get
>                 let (_,act) = head m1
>                 put (m1,(act,msg):m2)


> getMsg :: Action -> ActionM String
> getMsg act = get >>= return . (fromJust . lookup act) . snd


> actions :: (ActionDictionary,MessageMap)
> actions = runActionM $ do
>    newWord "take" Take >> ditto "carry" >> ditto "keep"
>    ditto "catch" >> ditto "captu" >> ditto "steal"
>    ditto "get" >> ditto "tote"
>    setMsg "You are already carrying it!"

>    newWord "drop" Drop >> ditto "relea" >> ditto "free"
>    ditto "disca" >> ditto "dump"
>    setMsg "You aren't carrying it!"

>    newWord "open" Open >> ditto "unloc"
>    setMsg "I don't know how to lock or unlock such a thing."

>    newWord "close" Close >> ditto "lock"
>    setMsg =<< getMsg Open

>    newWord "light" On >> ditto "on"
>    setMsg "You have no source of light."

>    newWord "extin" Off >> ditto "off"
>    setMsg =<< getMsg On

>    newWord "wave" Wave >> ditto "shake" >> ditto "swing"
>    setMsg "Nothing happens."

>    newWord "calm" Calm >> ditto "placa" >> ditto "tame"
>    setMsg "I'm game. Would you care to explain how?"

>    newWord "walk" Go >> ditto "run" >> ditto "trave"
>    ditto "go" >> ditto "proce" >> ditto "explo"
>    ditto "goto" >> ditto "follo" >> ditto "turn"
>    setMsg "Where?"

>    newWord "nothi" Relax
>    setMsg "OK."

>    newWord "pour" Pour
>    setMsg =<< getMsg Drop

>    newWord "eat" Eat >> ditto "devou"
>    setMsg "Don't be ridiculous!"

>    newWord "drink" Drink
>    setMsg
>         "You have taken a drink from the stream. The water tastes strongly of\n\
>         \minerals, but is not unpleasant. It is extremely cold."

>    newWord "rub" Rub
>    setMsg "Rubbing the electric lamp is not particularly rewarding. Anyway\n\
>               \nothing exciting happens."

>    newWord "throw" Toss >> ditto "toss"
>    setMsg "Peculiar. Nothing unexpected happens."

>    newWord "wake" Wake >> ditto "distu"
>    setMsg =<< getMsg Eat

>    newWord "feed" Feed
>    setMsg "There is nothing here to eat."

>    newWord "fill" Fill
>    setMsg "You can't fill that."

>    newWord "break" Break >> ditto "smash" >> ditto "shatt"
>    setMsg "It is beyond your power to do that."
