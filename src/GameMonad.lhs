The game Monad
==============

> {-# Language GeneralizedNewtypeDeriving #-}

> import Control.Monad.Error (MonadError,Error,ErrorT,noMsg,
>                             runErrorT)
> import Control.Monad.IO.Class (MonadIO)
> import Control.Monad.Random (MonadRandom,RandT,
>                              runRandT)
> import Control.Monad.State (MonadState,StateT,gets,modify,runStateT)
> import System.Random (StdGen)

> import Globals

Game possible outcomes:

> data GameOver = Win | Lose

> instance Error GameOver where
>     noMsg = undefined


A monad stack with exceptions, global state, random numbers and IO:

> newtype Game a =
>     Game {unGame::ErrorT GameOver (StateT Globals (RandT StdGen (IO))) a}
>     deriving (Functor,Monad,MonadIO,MonadState Globals,
>               MonadError GameOver,MonadRandom)

The monad runner:

> runGame :: StdGen -> Globals -> Game a -> IO (Either GameOver a, StdGen)
> runGame g s m = do
>     ((outcome,_),g') <- runRandT (runStateT (runErrorT $ unGame m) s) g
>     return (outcome,g')
