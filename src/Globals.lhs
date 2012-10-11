Global variables
================

> module Globals (Globals(..)) where

> import Data.Map (Map)


> import Actions (Action)
> import Locations (Location)
> import Motions (Motion)
> import Objects (Object)


> data Globals = Globals {

Instructions
------------

>       remCount::Int, -- we've made this many comments
>       visits::Map Location Int, -- how often have you been here?

Objects
-------

>       object:: Map Location [Object], -- objects present at a location
>       place :: Map Location Object,   -- each object's current location
>       holding :: Int, -- how many objects do we have in hand
>       note::Int, -- how many notes have we stored?

main Loop
---------

>       oldoldloc::Location, oldloc::Location,  -- recent and future locations
>       loc::Location, newloc::Location, -- recent and future locations
>       mot::Motion, -- currently specified motion, if any
>       verb::Action, -- currently specified action, if any
>       oldverb::Action, -- verb before it was changed
>       obj::Object, -- currently specified object, if any
>       oldobj::Object, -- former value of obj
>       turns::Int, -- how many times we’ve read your commands
>       westCount::Int, -- how many times have we parsed the word ‘west’ ?
>       wasDark::Bool, -- you’ve recently been in the dark
>       interval::Int, -- = 5; will change to 10000 if you want us to be BRIEF
>       tally::Int, -- treasures awaiting you (15)
>       lostTreasures::Int, -- treasures that you won’t find

Simple verbs
------------

>       gaveUp::Bool, -- did you quit while you were alive?
>       limit::Int, -- countdown till darkness

Other actions
-------------

>       foobar::Int, -- current incantation progress
>       lookCount::Int, -- how many times you’ve asked us to look

Dwarf stuff
-----------

>       dflag::Int, -- how angry are the dwarves?
>       dkill::Int, -- how many of them have you killed?
>       dloc::Map Int Location, -- = {chest loc , hmk , wfiss , y2 , like3 , complex };
>       odloc::Map Int Location, -- prior locations
>       dseen::Map Int Bool, -- have you been spotted?
>       dtotal::Int, -- this many dwarves are in the room with you
>       attack::Int, -- this many have had time to draw their knives
>       stick::Int, -- this many have hurled their knives accurately
>       ploc::[Location], -- potential locations for the next random step
>       knifeLoc::Location, -- place where knife was mentioned, or -1

Closing the cave
----------------

>       clock1::Int, clock2 ::Int, -- clocks that govern closing time
>       panic::Bool, closed ::Bool, -- various stages of closedness

Death and resurrection
----------------------

>       warned::Bool, -- have you been warned about the low power supply?
>       deathCount::Int, -- how often have you kicked the bucket?
>       bonus::Int -- extra points awarded for exceptional adventuring skills
>              }

