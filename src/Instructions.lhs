Instructions
------------
Speaking of program logic, the complex cave dynamics are essentially kept
in a table. The table tells us what to do when you ask for a particular
motion in a particular location. Each entry of the table is called an
instruction; and each instruction has three parts: a motion, a condition,
and a destination.

The motion part of an instruction is one of the motion
verbs enumerated earlier. The condition part $c$ is a small integer,
interpreted as follows:

    * if $c = 0$, the condition is always true;
    * if $0 < c < 100$, the condition is true with probability $c/100$;
    * if $c = 100$, the condition is always true, except for dwarves;
    * if $100 < c \leq 200$, you must have object $c mod 100$;
    * if $200 < c \leq 300$, object $c mod 100$ must be in the current place;
    * if $300 < c \leq 400$, $prop [c mod 100]$ must not be $0$;
    * if $400 < c \leq 500$, $prop [c mod 100]$ must not be $1$;
    * if $500 < c \leq 600$, $prop [c mod 100]$ must not be $2$; etc.


(We will discuss properties of objects and the prop array later.) The
destination d is either a location or a number greater than max loc ; in
the latter case, if $d \leq max$ spec we perform a special routine, otherwise
we print remarks $[d - max spec ]$ and stay in the current place.

If the motion matches what you said but the condition is not satisfied, we
move on to the next instruction that has a different destination and/or
condition from this one. The next instruction might itself be conditional
in the same way. (Numerous examples appear below.)


> module Instructions where

> import qualified Motions as M
> import           Motions (Motion)
> import qualified Locations as L
> import           Locations (Location)

> data Instruction = Instruction Motion Int Location

> data Flag = Lighted    -- a location that isn’t dark
>           | Oil        -- presence of oil
>           | Liquid     -- presence of a liquid (oil or water)
>           | Cave       -- hint about trying to get in the cave
>           | Bird       -- hint about catching the bird
>           | Snake      -- hint about dealing with the snake
>           | Twist      -- hint about being lost in a maze
>           | Dark       -- hint about the dark room
>           | Witt       -- hint about Witt’s End



