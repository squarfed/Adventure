Actions
-------

> module Actions (Action(..),actions) where

> import qualified Data.Map as  Map
> import           Data.Map (Map)

> data Action = Abstain | Take | Drop | Open | Close
>             | On | Off | Wave | Calm | Go | Relax
>             | Pour | Eat | Drink | Rub | Toss | Wake | Feed
>             | Fill | Break | Blast | Kill
>             | Say | Read | Feefie | Brief | Find
>             | Inventory | Score | Quit
>             deriving (Eq,Ord,Show)


> type ActionDictionary = [(String,Action)]

> actions :: [(String, Action)]
> actions = [("take", Take),("carry", Take),("keep", Take),
>            ("catch", Take),("captu", Take),("steal", Take),
>            ("get", Take),("tote", Take),
>            ("drop", Drop),("relea", Drop),("free", Drop),
>            ("disca", Drop),("dump", Drop),
>            ("open", Open),("unloc", Open),
>            ("close", Close),("lock", Close),
>            ("light", On),("on", On),
>            ("extin", Off),("off", Off),
>            ("wave", Wave),("shake", Wave),("swing", Wave),
>            ("calm", Calm),("placa", Calm),("tame", Calm),
>            ("walk", Go),("run", Go),("trave", Go),("go", Go),
>            ("proce", Go),("explo", Go),("goto", Go),("follo", Go),
>            ("turn", Go),
>            ("nothi", Relax),
>            ("pour", Pour),
>            ("eat", Eat),("devou", Eat),
>            ("drink", Drink),
>            ("rub", Rub),
>            ("throw", Toss),("toss", Toss),
>            ("wake", Wake),("distu", Wake),
>            ("feed", Feed),
>            ("fill", Fill),
>            ("break", Break),("smash", Break),("shatt", Break),
>            ("blast", Blast),("deton", Blast),("ignit", Blast),("blowu", Blast),
>            ("attac", Kill),("kill", Kill),("fight", Kill),
>            ("hit", Kill),("strik", Kill),("slay", Kill),
>            ("say", Say),("chant", Say),("sing", Say),("utter", Say),
>            ("mumbl", Say),
>            ("read", Read),("perus", Read),
>            ("fee", Feefie),("fie", Feefie),("foe", Feefie),
>            ("foo", Feefie),("fum", Feefie),
>            ("brief", Brief),
>            ("find", Find),("where", Find),
>            ("inven", Inventory),
>            ("score", Score),
>            ("quit", Quit)]


> defaultMsg :: Map Action String
> defaultMsg = Map.fromList
>      [(Take, "You are already carrying it!"),
>       (Drop, "You aren't carrying it!"),
>       (Open, "I don't know how to lock or unlock such a thing."),
>       (Close, sameAs Open),
>       (On, "You have no source of light."),
>       (Off, sameAs On),
>       (Wave, "Nothing happens."),
>       (Calm, "I'm game.  Would you care to explain how?"),
>       (Go, "Where?"),
>       (Relax, "OK."),
>       (Pour, sameAs Drop),
>       (Eat, "Don't be ridiculous!"),
>       (Drink,
>         "You have taken a drink from the stream.  The water tastes strongly of\n\
>         \minerals, but is not unpleasant. It is extremely cold."),
>       (Rub,
>         "Rubbing the electric lamp is not particularly rewarding.  Anyway,\n\
>         \nothing exciting happens."),
>       (Toss, "Peculiar. Nothing unexpected happens."),
>       (Wake, sameAs Eat),
>       (Feed, "There is nothing here to eat."),
>       (Fill, "You can't fill that."),
>       (Break, "It is beyond your power to do that."),
>       (Blast,"Blasting requires dynamite."),
>       (Kill,sameAs Eat),
>       (Read,"I’m afraid I don’t understand."),
>       (Brief,"On what?"),
>       (Feefie,"I don’t know how."),
>       (Find,
>          "I can only tell you what you see as you move about and manipulate\n\
>          \things.  I cannot tell you where remote things are."),
>       (Inventory,sameAs Find),
>       (Score,"Eh?"),
>       (Quit,sameAs Score)]
>     where
>        sameAs = (defaultMsg Map.!)
