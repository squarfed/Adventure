% Colossal cave adventure: a Haskell implementation
% Federico Squartini

This program is a Haskell translation of Don Knuth/Don Wood's colossal
cave adventure game, _the_ text game.

> module Main where


> import qualified Data.Map as  Map
> import           Data.Map (Map)
> import           Prelude hiding (truncate)


> import qualified Motions as M
> import           Motions (Motion)
> import qualified Objects as O
> import           Objects (Object)
> import qualified Actions as A
> import           Actions (Action)
> import qualified Locations as L
> import           Locations (Location)
> import qualified Instructions as I



> main = undefined

The motion words either specify a direction or a simple action or a
place. Motion words take you from one location to another, when the
motion is permitted. Here is a list of their possible meanings.



> data Word = Motion Motion
>           | Action Action
>           | Object Object


And here is how they enter our vocabulary.

If I were writing this program, I would allow the word woods, but Don
apparently didn't want to.

Build the vocabulary
--------------------

> type Vocabulary = Map String Word

Words are truncated to 5 letters

> truncate :: String -> String
> truncate = take 5

The function to build a table


> lookup :: String -> Vocabulary -> Maybe Word
> lookup word = Map.lookup (truncate word)

Building the vocabulary

> vocabulary :: Vocabulary
> vocabulary = Map.fromList $ map (fmap Motion) M.motions  ++
>                             map (fmap Action) A.actions ++
>                             map (fmap Object) O.objects

Miscellaneous messages
----------------------

> miscMsg :: Map String String
> miscMsg = Map.fromList
>    [("abra", "Good try, but that is an old worn-out magic word."),
>     ("abrac",sameAs "abra"),
>     ("opens",sameAs "abra"),
>     ("sesam",sameAs "abra"),
>     ("shaze",sameAs "abra"),
>     ("hocus",sameAs "abra"),
>     ("pocus",sameAs "abra"),
>     ("help",
>       "I know of places, actions, and things. Most of my vocabulary\n\
>       \describes places and is used to move you there. To move, try words\n\
>       \like forest, building, downstream, enter, east, west, north, south,\n\
>       \up, or down. I know about a few special objects, like a black rod\n\
>       \hidden in the cave. These objects can be manipulated using some of\n\
>       \the action words that I know. Usually you will need to give both the\n\
>       \object and action words (in either order), but sometimes I can infer\n\
>       \the object from the verb alone. Some objects also imply verbs; in\n\
>       \particular, \"inventory\" implies \"take inventory\", which causes me to\n\
>       \give you a list of what you're carrying. The objects have side\n\
>       \effects; for instance, the rod scares the bird. Usually people having\n\
>       \trouble moving just need to try a few more words. Usually people\n\
>       \trying unsuccessfully to manipulate an object are attempting something\n\
>       \beyond their (or my!) capabilities and should try a completely\n\
>       \different tack. To speed the game you can sometimes move long\n\
>       \distances with a single word. For example, \"building\" usually gets\n\
>       \you to the building from anywhere above ground except when lost in the\n\
>       \forest. Also, note that cave passages turn a lot, and that leaving a\n\
>       \room to the north does not guarantee entering the next from the south.\n\
>       \Good luck!"),
>     ("?",sameAs "help"),
>     ("tree",
>       "The trees of the forest are large hardwood oak and maple, with an\n\
>       \occasional grove of pine or spruce. There is quite a bit of under-\n\
>       \growth, largely birch and ash saplings plus nondescript bushes of\n\
>       \various sorts. This time of year visibility is quite restricted by\n\
>       \all the leaves, but travel is quite easy if you detour around the\n\
>       \spruce and berry bushes."),
>     ("trees",sameAs "tree"),
>     ("dig",
>       "Digging without a shovel is quite impractical. Even with a shovel\n\
>       \progress is unlikely."),
>     ("excav",sameAs "dig"),
>     ("lost", "I'm as confused as you are."),
>     ("splash",
>       "There is a loud explosion and you are suddenly splashed across the\n\
>       \walls of the room."),
>     ("splash2",
>       "There is a loud explosion and a twenty-foot hole appears in the far\n\
>       \wall, burying the snakes in the rubble. A river of molten lava pours\n\
>       \in through the hole, destroying everything in its path, including you!"),
>     ("mist", "\
>       \Mist is a white vapor, usually water, seen from time to time in\n\
>       \caverns. It can be found anywhere but is frequently a sign of a deep\n\
>       \pit leading down to water."),
>     ("fuck", "Watch it!"),
>     ("end",
>       "There is a loud explosion, and a twenty-foot hole appears in the far\n\
>       \wall, burying the dwarves in the rubble. You march through the hole\n\
>       \and find yourself in the main office, where a cheering band of\n\
>       \friendly elves carry the conquering adventurer off into the sunset."),
>     ("stop",
>       "I don't know the word \"stop\". Use \"quit\" if you want to give up."),
>     ("info",
>       "If you want to end your adventure early, say \"quit\". To get full\n\
>       \credit for a treasure, you must have left it safely in the building,\n\
>       \though you get partial credit just for locating it. You lose points\n\
>       \for getting killed, or for quitting, though the former costs you more.\n\
>       \There are also points based on how much (if any) of the cave you've\n\
>       \managed to explore; in particular, there is a large bonus just for\n\
>       \getting in (to distinguish the beginners from the rest of the pack),\n\
>       \and there are other ways to determine whether you've been through some\n\
>       \of the more harrowing sections. If you think you've found all the\n\
>       \treasures, just keep exploring for a while. If nothing interesting\n\
>       \happens, you haven't found them all yet. If something interesting\n\
>       \DOES happen, it means you're getting a bonus and have an opportunity\n\
>       \to garner many more points in the master's section.\n\
>       \I may occasionally offer hints if you seem to be having trouble.\n\
>       \If I do, I'll warn you in advance how much it will affect your score\n\
>       \to accept the hints. Finally, to save paper, you may specify \"brief\",\n\
>       \which tells me never to repeat the full description of a place\n\
>       \unless you explicitly ask me to."),
>     ("infor",sameAs "info"),
>     ("swim", "I don't know how.")]
>     where
>        sameAs = (miscMsg Map.!)




