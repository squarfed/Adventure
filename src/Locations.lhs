Cave data
---------

  You might be in any of more than 100 places as you wander about in
  Colossal Cave.

  Let's enumerate them now, so that we can build the data structures that
  define the travel restrictions.

  A special negative value called inhand is the location code for objects
  that you are carrying. But you yourself are always situated in a place that
  has a nonnegative location code.

  Nonnegative places $\leq$ outside are outside the cave, while places $\geq$
  inside are inside. The upper part of the cave, places $<$ emist , is the
  easiest part to explore. (We will see later that dwarves do not venture
  this close to the surface; they stay $\geq$ emist .)

  Places between inside and dead2 , inclusive, form the main cave; the next
  places, up to and including barr, form the hidden cave on the other side of
  the troll bridge; then neend and swend are a private cave.

  The remaining places, $\geq$ crack are dummy locations, not really part of
  the maze. As soon as you arrive at a dummy location, the program
  immediately sends you somewhere else. In fact, the last three dummy
  locations aren't really even locations; they invoke special code. This
  device is a convenient way to provide a variety of features without making
  the program logic any more cluttered than it already is.


> module Locations (Location(..)) where

> data Location = Inhand | Limbo
>               | Road | Hill | House | Valley
>               | Forest | Woods | Slit | Outside
>               | Inside | Cobbles | Debris | Awk | Bird | Spit
>               | Emist | Nugget | Efiss | Wfiss | Wmist
>               | Like1 | Like2 | Like3 | Like4
>               | Like5 | Like6 | Like7 | Like8
>               | Like9 | Like10 | Like11 | Like12
>               | Like13 | Like14
>               | Brink | Elong | Wlong
>               | Diff0 | Diff1 | Diff2 | Diff3 | Diff4
>               | Diff5 | Diff6 | Diff7 | Diff8 | Diff9 | Diff10
>               | Pony | Cross | Hmk | West | South | Ns
>               | Y2 | Jumble | Windoe
>               | Dirty | Clean | Wet | Dusty | Complex
>               | Shell | Arch | Ragged | Sac | Ante | Witt
>               | Bedquilt | Cheese | Soft
>               | E2pit | W2pit | Epit | Wpit
>               | Narrow | Giant | Block | Immense
>               | Falls | Steep | Abovep | Sjunc
>               | Tite | Low | Crawl | Window
>               | Oriental | Misty | Alcove | Proom | Droom
>               | Slab | Abover | Mirror | Res
>               | Scan1 | Scan2 | Scan3 | Secret
>               | Wide | Tight | Tall | Boulders
>               | Scorr | Swside
>               | Dead0 | Dead1 | Dead2 | Dead3 | Dead4
>               | Dead5 | Dead6 | Dead7 | Dead8 | Dead9
>               | Dead10 | Dead11
>               | Neside | Corr | Fork | Warm | View
>               | Chamber | Lime | Fbarr | Barr
>               | Neend | Swend
>               | Crack | Neck | Lose | Cant | Climb
>               | Check | Snaked | Thru | Duck | Sewer
>               | Upnout | Didit
>               | Ppass | Pdrop | Troll
>                 deriving (Show,Enum)