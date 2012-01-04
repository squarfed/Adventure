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

> runActionM = flip execState ([],[])

> newWord word act = modify $ \(m1,m2) -> ((word,act):m1,m2)

> setMsg act msg = modify $ \(m1,m2) -> (m1,(act,msg):m2)

> getMsg act = get >>= return . (fromJust . lookup act) . snd

> actions :: (ActionDictionary,MessageMap)
> actions = runActionM $ do
>    newWord "take" Take >> newWord "carry" Take >> newWord "keep" Take
>    newWord "catch" Take >> newWord "captu" Take >> newWord "steal" Take
>    newWord "get" Take >> newWord "tote" Take
>    setMsg Take "You are already carrying it!"
>    newWord "drop" Drop >> newWord "relea" Drop >> newWord "free" Drop
>    newWord "disca" Drop >> newWord "dump" Drop
>    setMsg Drop "You aren't carrying it!"
>    newWord "open" Open >> newWord "unloc" Open
>    setMsg Open "I don't know how to lock or unlock such a thing."
>    newWord "close" Close >> newWord "lock" Close
>    setMsg Close =<< getMsg Open
>    newWord "light" On >> newWord "on" On
>    setMsg On "You have no source of light."
>    newWord "extin" Off >> newWord "off" Off
>    setMsg Off =<< getMsg On
>    newWord "wave" Wave >> newWord "shake" Wave >> newWord "swing" Wave
>    setMsg Wave "Nothing happens."
>    newWord "calm" Calm >> newWord "placa" Calm >> newWord "tame" Calm
>    setMsg Calm "I'm game. Would you care to explain how?"
>    newWord "walk" Go >> newWord "run" Go >> newWord "trave" Go
>    newWord "go" Go >> newWord "proce" Go >> newWord "explo" Go
>    newWord "goto" Go >> newWord "follo" Go >> newWord "turn" Go
>    setMsg Go "Where?"
>    newWord "nothi" Relax
>    setMsg Relax "OK."
>    newWord "pour" Pour
>    setMsg Pour =<< getMsg Drop
>    newWord "eat" Eat >> newWord "devou" Eat
>    setMsg Eat "Don't be ridiculous!"
>    newWord "drink" Drink
>    setMsg Drink
>         "You have taken a drink from the stream. The water tastes strongly of\n\
>         \minerals, but is not unpleasant. It is extremely cold."
>    newWord "rub" Rub
>    setMsg Rub "Rubbing the electric lamp is not particularly rewarding. Anyway\n\
>               \nothing exciting happens."
>    newWord "throw" Toss >> newWord "toss" Toss
>    setMsg Toss "Peculiar. Nothing unexpected happens."
>    newWord "wake" Wake >> newWord "distu" Wake
>    setMsg Wake =<< getMsg Eat
>    newWord "feed" Feed
>    setMsg Feed "There is nothing here to eat."
>    newWord "fill" Fill
>    setMsg Fill "You can't fill that."
>    newWord "break" Break >> newWord "smash" Break >> newWord "shatt" Break
>    setMsg Break "It is beyond your power to do that."
