Building the vocabulary
=======================


> module Vocabulary (Word(..),look) where

> import qualified Motions as M
> import           Motions (Motion)
> import qualified Objects as O
> import           Objects (Object)
> import qualified Actions as A
> import           Actions (Action)
> import           Messages

> import qualified Data.Map as M
> import           Data.Map (Map)
> import           Prelude hiding (truncate)

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

> type Vocabulary = Map String Word

Words are truncated to 5 letters

> truncate :: String -> String
> truncate = take 5

The function to build a table

> look :: String -> Maybe Word
> look word = M.lookup (truncate word) vocabulary

Building the vocabulary

> vocabulary :: Vocabulary
> vocabulary = M.fromList $ map (fmap Motion) M.motionsVoc  ++
>                             map (fmap Action) A.actionsVoc ++
>                             map (fmap Object) O.objectsVoc ++
>                             map (fmap Message) messagesVoc
