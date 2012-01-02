Miscellaneous messages
----------------------

> module Messages where

> import Control.Monad.State
> import qualified Data.Map as M
> import           Data.Map (Map)

> type Message = String

> type MessageM = State (Message,Map String Message)

> runMessageM :: MessageM a -> Map String Message
> runMessageM = snd . flip execState ("",M.empty)

> newMess :: String -> Message -> MessageM ()
> newMess word mess = modify $ \(_,dict) ->  (mess,M.insert word mess dict)

> ditto :: String ->MessageM ()
> ditto word = do (mess,dict) <- get
>                 put $ (mess,M.insert word mess dict)


> messages :: Map String Message
> messages = runMessageM $ do
>    newMess "abra" "Good try, but that is an old worn-out magic word."
>    ditto "abrac"
>    ditto "opens"
>    ditto "sesam"
>    ditto "shaze"
>    ditto "hocus"
>    ditto "pocus"
>    newMess "help"
>       "I know of places, actions, and things.  Most of my vocabulary\n\
>       \describes places and is used to move you there.  To move, try words\n\
>       \like forest, building, downstream, enter, east, west, north, south,\n\
>       \up, or down.  I know about a few special objects, like a black rod\n\
>       \hidden in the cave.  These objects can be manipulated using some of\n\
>       \the action words that I know.  Usually you will need to give both the\n\
>       \object and action words (in either order), but sometimes I can infer\n\
>       \the object from the verb alone.  Some objects also imply verbs; in\n\
>       \particular, \"inventory\" implies \"take inventory\", which causes me to\n\
>       \give you a list of what you're carrying.  The objects have side\n\
>       \effects; for instance, the rod scares the bird.  Usually people having\n\
>       \trouble moving just need to try a few more words.  Usually people\n\
>       \trying unsuccessfully to manipulate an object are attempting something\n\
>       \beyond their (or my!) capabilities and should try a completely\n\
>       \different tack.  To speed the game you can sometimes move long\n\
>       \distances with a single word.  For example, \"building\" usually gets\n\
>       \you to the building from anywhere above ground except when lost in the\n\
>       \forest.  Also, note that cave passages turn a lot, and that leaving a\n\
>       \room to the north does not guarantee entering the next from the south.\n\
>       \Good luck!"
>    ditto "?"
>    newMess "tree"
>       "The trees of the forest are large hardwood oak and maple, with an\n\
>       \occasional grove of pine or spruce.  There is quite a bit of under-\n\
>       \growth, largely birch and ash saplings plus nondescript bushes of\n\
>       \various sorts.  This time of year visibility is quite restricted by\n\
>       \all the leaves, but travel is quite easy if you detour around the\n\
>       \spruce and berry bushes."
>    ditto "trees"
>    newMess "dig"
>       "Digging without a shovel is quite impractical.  Even with a shovel\n\
>       \progress is unlikely."
>    ditto "excav"
>    newMess "lost" "I'm as confused as you are."
>    newMess "splash"
>       "There is a loud explosion and you are suddenly splashed across the\n\
>       \walls of the room."
>    newMess "splash2"
>       "There is a loud explosion and a twenty-foot hole appears in the far\n\
>       \wall, burying the snakes in the rubble.  A river of molten lava pours\n\
>       \in through the hole, destroying everything in its path, including you!"
>    newMess "mist" "\
>       \Mist is a white vapor, usually water, seen from time to time in\n\
>       \caverns.  It can be found anywhere but is frequently a sign of a deep\n\
>       \pit leading down to water."
>    newMess "fuck" "Watch it!"
>    newMess "end"
>       "There is a loud explosion, and a twenty-foot hole appears in the far\n\
>       \wall, burying the dwarves in the rubble.  You march through the hole\n\
>       \and find yourself in the main office, where a cheering band of\n\
>       \friendly elves carry the conquering adventurer off into the sunset."
>    newMess "stop"
>       "I don't know the word \"stop\".  Use \"quit\" if you want to give up."
>    newMess "info"
>       "If you want to end your adventure early, say \"quit\".  To get full\n\
>       \credit for a treasure, you must have left it safely in the building,\n\
>       \though you get partial credit just for locating it.  You lose points\n\
>       \for getting killed, or for quitting, though the former costs you more.\n\
>       \There are also points based on how much (if any) of the cave you've\n\
>       \managed to explore; in particular, there is a large bonus just for\n\
>       \getting in (to distinguish the beginners from the rest of the pack),\n\
>       \and there are other ways to determine whether you've been through some\n\
>       \of the more harrowing sections.  If you think you've found all the\n\
>       \treasures, just keep exploring for a while.  If nothing interesting\n\
>       \happens, you haven't found them all yet.  If something interesting\n\
>       \DOES happen, it means you're getting a bonus and have an opportunity\n\
>       \to garner many more points in the master's section.\n\
>       \I may occasionally offer hints if you seem to be having trouble.\n\
>       \If I do, I'll warn you in advance how much it will affect your score\n\
>       \to accept the hints.  Finally, to save paper, you may specify \"brief\",\n\
>       \which tells me never to repeat the full description of a place\n\
>       \unless you explicitly ask me to."
>    ditto "infor"
>    newMess "swim" "I don't know how."
