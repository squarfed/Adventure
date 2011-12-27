> module Vocabulary where

> import qualified Motions as M
> import           Motions (Motion)
> import qualified Objects as O
> import           Objects (Object)
> import qualified Actions as A
> import           Actions (Action)
> import           Messages

The motion words either specify a direction or a simple action or a
place. Motion words take you from one location to another, when the
motion is permitted. Here is a list of their possible meanings.


> data Word = Motion Motion
>           | Action Action
>           | Object Object
>           | Message Message


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
>                             map (fmap Object) O.objects ++
>                             map (fmap Message) messages
