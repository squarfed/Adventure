Instructions
------------

> module Instructions (travels) where

> import qualified Data.Map as Map ((!),empty,insert)
> import           Data.Map (Map)

> import           Actions (Action(Eat),defaultMsg)
> import           Motions
> import qualified Locations as L
> import           Locations (Location)
> import           Objects hiding (Nothing,Oil)
> import qualified Objects as O (Object(Nothing,Oil))


> data Cond = Always
>           | NotDwarf
>           | Percent Int
>           | Holds Object
>           | Sees Object
>           | Not Object Int
>           deriving Show

> data MakeLoc = MakeLoc L.Location
>                        String
>                        (Maybe String)
>                        [Flag]
>                        [Instruction]

> data What = To Location | Sayit String
>             deriving Show

> data Instruction = Instruction [Motion]
>                             Cond
>                             What
>                    deriving Show

> data Flag = Lighted    -- a location that isn't dark
>           | Oil        -- presence of oil
>           | Liquid     -- presence of a liquid (oil or water)
>           | Cave_Hint       -- hint about trying to get in the cave
>           | Bird_Hint       -- hint about catching the bird
>           | Snake_Hint      -- hint about dealing with the snake
>           | Twist_Hint      -- hint about being lost in a maze
>           | Dark_Hint       -- hint about the dark room
>           | Witt_Hint       -- hint about Witt's End
>           deriving (Eq,Show)

> allAlike = "You are in a maze of twisty little passages, all alike."

> deadEnd = "Dead end."

> grateRmk = "You can't go through a locked steel grate!"

> loopRmk =
>      "You have crawled around in some little holes and wound up back in the\n\
>      \main passage."


> bridgeRmk = "I respectfully suggest you go across the bridge instead of jumping."

> slitRmk = "You don't fit through a two−inch slit!"

Creating the map
----------------

  The road is where you start; its long desc is now famous, having been
  quoted by Steven Levy in his book Hackers.

  The instructions here say that if you want to go west, or up, or on the
  road, we take you to hill ; if you want to go east, or in, or to the
  house, or if you say 'enter', we take you to house ; etc. Of course you
  won't know about all the motions available at this point until you have
  played the game for awhile.


> travels :: Map Location (String, Maybe String, [Flag], [Instruction])
> travels = foldl step Map.empty travelsList
>     where
>       step acc (MakeLoc loc longDesc shortDesc flags insts) =
>           Map.insert loc (longDesc,shortDesc,flags,insts) acc

> travelsList :: [MakeLoc]
> travelsList = [
>   MakeLoc L.Road
>       "You are standing at the end of a road before a small brick building.\n\
>       \Around you is a forest.  A small stream flows out of the building and\n\
>       \down a gully."
>       (Just "You're at end of road again.")
>       [Lighted,Liquid]
>       [Instruction [W,U,Road] Always (To L.Hill),
>        Instruction [E,In,House,Enter] Always (To L.House),
>        Instruction [S,D,Gully,Stream,Downstream] Always (To L.Valley),
>        Instruction [N,Woods] Always (To L.Forest),
>        Instruction [Depression] Always (To L.Outside)],

  Nothing up the hill, but a good explorer has to try anyway.

>   MakeLoc L.Hill
>       "You have walked up a hill, still in the forest.  The road slopes back\n\
>        \down the other side of the hill.  There is a building in the distance."
>       (Just "You're at hill in road.")
>       [Lighted]
>       [Instruction [Road,House,Forward,E,D] Always (To L.Road),
>        Instruction [Woods,N,S] Always (To L.Forest)],

  The house initially contains several objects: keys, food, a bottle, and a
  lantern. We'll put them in there later.

  Two magic words are understood in this house, for spelunkers who have been
  there and done that.

>   MakeLoc L.House
>       "You are inside a building, a well house for a large spring."
>       (Just "You're inside building.")
>       [Lighted,Liquid]
>       [Instruction [Enter,Out,Outdoors,W] Always (To L.Road),
>        Instruction [XYZZY] Always (To L.Debris),
>        Instruction [Plugh] Always (To L.Y2),
>        Instruction [Downstream,Stream] Always (To L.Sewer)],

  A foolish consistency is the hobgoblin of little minds. (Emerson)

>   MakeLoc L.Valley
>       "You are in a valley in the forest beside a stream tumbling along a\n\
>       \rocky bed."
>       (Just "You're in valley.")
>       [Lighted,Liquid]
>       [Instruction [Upstream,House,N] Always (To L.Road),
>        Instruction [Woods,E,W,U] Always (To L.Forest),
>        Instruction [Downstream,S,D] Always (To L.Slit),
>        Instruction [Depression] Always (To L.Outside)],

  The instructions here keep you in the forest with probability 50%,
  otherwise they take you to the woods . This gives the illusion that we
  maintain more state information about you than we really do.

>   MakeLoc L.Forest
>       "You are in open forest, with a deep valley to one side."
>       (Just "You're in forest.")
>       [Lighted]
>       [Instruction [Valley,E,D] Always (To L.Valley),
>        Instruction [Woods,Forward,N] (Percent 50) (To L.Forest),
>        Instruction [Woods] Always (To L.Woods),
>        Instruction [W,S] Always (To L.Forest)],

>   MakeLoc L.Woods
>       "You are in open forest near both a valley and a road."
>       Nothing
>       [Lighted]
>       [Instruction [Road,N] Always (To L.Road),
>        Instruction [Valley,E,W,D] Always (To L.Valley),
>        Instruction [Woods,S] Always (To L.Forest)],

  You're getting closer. (But the program has forgotten that `Depression`
  leads outside; it knew this when you were at the road or the valley .)

>   MakeLoc L.Slit
>       "At your feet all the water of the stream splashes into a 2-inch slit\n\
>        \in the rock.  Downstream the streambed is bare rock."
>       (Just "You're at slit in streambed.")
>       [Lighted,Liquid]
>       [Instruction [House] Always (To L.Road),
>        Instruction [Upstream,N] Always (To L.Valley),
>        Instruction [Woods,E,W] Always (To L.Forest),
>        Instruction [Downstream,Rock,Bed,S] Always (To L.Outside),
>        Instruction [Slit,Stream,D] Always (Sayit slitRmk)],

  We'll see later that the Grate will change from state 0 to state 1 if
  you unlock it. So let's hope you have the Keys.

>   MakeLoc L.Outside
>       "You are in a 20-foot depression floored with bare dirt.  Set into the\n\
>        \dirt is a strong steel grate mounted in concrete.  A dry streambed\n\
>        \leads into the depression."
>       (Just "You're outside grate.")
>       [Lighted,Cave_Hint]
>       [Instruction [Woods,E,W,S] Always (To L.Forest),
>        Instruction [House] Always (To L.Road),
>        Instruction [Upstream,Gully,N] Always (To L.Slit),
>        Instruction [Enter,In,D] (Not Grate 0) (To L.Inside), -- twice enter in knuth code
>        Instruction [Enter] Always (Sayit grateRmk)],

  If you've come this far, you're probably hooked, although your adventure
  has barely begun.

>   MakeLoc L.Inside
>       "You are in a small chamber beneath a 3x3 steel grate to the surface.\n\
>       \A low crawl over cobbles leads inwards to the west."
>       (Just "You're below the grate.")
>       [Lighted]
>       [Instruction [Out,U] (Not Grate 0) (To L.Outside), -- twice out in knuth code
>        Instruction [Out] Always (Sayit grateRmk), -- ??
>        Instruction [Crawl,Cobbles,In,W] Always (To L.Cobbles),
>        Instruction [Pit] Always (To L.Spit),
>        Instruction [Debris] Always (To L.Debris)],

  Go West, young man. (If you've got a lamp.)

>   MakeLoc L.Cobbles
>       "You are crawling over cobbles in a low passage.  There is a dim light\n\
>       \at the east end of the passage."
>       (Just "You're in cobble crawl.")
>       [Lighted]
>       [Instruction [Out,Surface,Nowhere,E] Always (To L.Inside),
>        Instruction [In,Dark,W,Debris] Always (To L.Debris),
>        Instruction [Pit] Always (To L.Spit)],

>   MakeLoc L.Debris
>       "You are in a debris room filled with stuff washed in from the surface.\n\
>       \A low wide passage with cobbles becomes plugged with mud and debris\n\
>       \here, but an awkward canyon leads upward and west.  A note on the wall\n\
>       \says \"MAGIC WORD XYZZY\"."
>       (Just "You're in debris room.")
>       []

>       [Instruction [Depression] (Not Grate 0) (To L.Outside), -- ??
>        Instruction [Entrance] Always (To L.Inside),
>        Instruction [Crawl,Cobbles,Passage,Low,E] Always (To L.Cobbles),
>        Instruction [Canyon,In,U,W] Always (To L.Awk),
>        Instruction [XYZZY] Always (To L.House),
>        Instruction [Pit] Always (To L.Spit)],

>   MakeLoc L.Awk
>       "You are in an awkward sloping east/west canyon."  Nothing  [] -- ??
>       [Instruction [Depression] (Not Grate 0) (To L.Outside), -- ??
>        Instruction [Entrance] Always (To L.Inside),
>        Instruction [D,E,Debris] Always (To L.Debris),
>        Instruction [In,U,W] Always (To L.Bird),
>        Instruction [Pit] Always (To L.Spit)],

>   MakeLoc L.Bird
>       "You are in a splendid chamber thirty feet high.  The walls are frozen\n\
>       \rivers of orange stone.  An awkward canyon and a good passage exit\n\
>       \from east and west sides of the chamber."
>       (Just "You're in bird chamber.")
>       [Bird_Hint]
>       [Instruction [Depression] (Not Grate 0) (To L.Outside),
>        Instruction [Entrance] Always (To L.Inside),
>        Instruction [Debris] Always (To L.Debris),
>        Instruction [Canyon,E] Always (To L.Awk),
>        Instruction [Passage,Pit,W] Always (To L.Spit)], -- check

>   MakeLoc L.Spit
>       "At your feet is a small pit breathing traces of white mist.  An east\n\
>       \passage ends here except for a small crack leading on."
>       (Just "You're at top of small pit.")
>       []
>       [Instruction [Depression] (Not Grate 0) (To L.Outside),
>        Instruction [Entrance] Always (To L.Inside),
>        Instruction [Debris] Always (To L.Debris),
>        Instruction [Passage,E] Always (To L.Bird),
>        Instruction [D,Pit,Steps] (Holds Gold) (To L.Neck),

  good thing you weren't loaded down with Gold

>        Instruction [D] Always (To L.Emist),
>        Instruction [Crack,W] Always (To L.Crack)],


  Welcome to the main caverns and a deeper level of adventures.

>   MakeLoc L.Emist
>       "You are at one end of a vast hall stretching forward out of sight to\n\
>       \the west.  There are openings to either side.  Nearby, a wide stone\n\
>       \staircase leads downward.  The hall is filled with wisps of white mist\n\
>       \swaying to and fro almost as if alive.  A cold wind blows up the\n\
>       \staircase.  There is a passage at the top of a dome behind you."
>       (Just "You're in Hall of Mists.")
>       []
>       [Instruction [L,S] Always (To L.Nugget),
>        Instruction [Forward,Hall,W] Always (To L.Efiss),
>        Instruction [Stairs,D,N] Always (To L.Hmk),
>        Instruction [U,Pit,Steps,Dome,Passage,E] (Holds Gold) (To L.Cant),
>        Instruction [U] Always (To L.Spit),
>        Instruction [Y2] Always (To L.Jumble)],

  To the left or south of the misty threshold, you might spot the first treasure.

>   MakeLoc L.Nugget
>       "This is a low room with a crude note on the wall.  The note says,\n\
>       \ \"You won't get it up the steps\"."
>       (Just "You're in nugget of gold room.")
>       []
>       [Instruction [Hall,Out,N] Always (To L.Emist)],

  Unless you take a circuitous route to the other side of the Hall of
  Mists, via the Hall of the Mountain King, you should make the CRYSTAL
  bridge appear (by getting it into state 1).

>   MakeLoc L.Efiss
>       "You are on the east bank of a fissure slicing clear across the hall.\n\
>       \The mist is quite thick here, and the fissure is too wide to jump."
>       (Just "You're on east bank of fissure.")
>       []
>       [Instruction [Hall,E] Always (To L.Emist),
>        Instruction [Jump] (Not Crystal 0) (Sayit bridgeRmk),
>        Instruction [Forward] (Not Crystal 1) (To L.Lose),
>        Instruction [Over,Across,W,Cross] (Not Crystal 1)
>                                   (Sayit "There is no way across the fissure."),
>        Instruction [Over] Always (To L.Wfiss)],

>   MakeLoc L.Wfiss
>       "You are on the west side of the fissure in the Hall of Mists."
>       Nothing
>       []
>       [Instruction [Jump] (Not Crystal 0) (Sayit bridgeRmk),
>        Instruction [Forward] (Not Crystal 1) (To L.Lose),
>        -- CHECK FOR CORRETNESS OF NEXT
>        Instruction [Over,Across,E,Cross] (Not Crystal 1)
>                  (Sayit "There is no way across the fissure."),
>        Instruction [Over] Always (To L.Efiss),
>        Instruction [N] Always (To L.Thru),
>        Instruction [W] Always (To L.Wmist)],

  What you see here isn't exactly what you get; N takes you east and S sucks
  you in to an amazing maze.

>   MakeLoc L.Wmist
>       "You are at the west end of the Hall of Mists.  A low wide crawl\n\
>       \continues west and another goes north.  To L.the south is a little\n\
>       \passage 6 feet off the floor."
>       (Just "You're at west end of Hall of Mists.")
>       []
>       [Instruction [S,U,Passage,Climb] Always (To L.Like1),
>        Instruction [E] Always (To L.Wfiss),
>        Instruction [N] Always (To L.Duck),
>        Instruction [W,Crawl] Always (To L.Elong)],

  The twisty little passages of this maze are said to be all alike, but they
  respond differently to  different motions. For example, you  can go north,
  east, south, or west from Like1,  but you can't go north from Like2 .  In
  that way  you can psych out the  whole maze of 14  similar locations. (And
  eventually  you will  want to  know every  place where  treasure  might be
  hidden.) The only exits are to wmist and brink .

>   MakeLoc L.Like1
>       allAlike Nothing [Twist_Hint]
>       [Instruction [U] Always (To L.Wmist),
>        Instruction [N] Always (To L.Like1),
>        Instruction [E] Always (To L.Like2),
>        Instruction [S] Always (To L.Like4),
>        Instruction [W] Always (To L.Like11)],

>   MakeLoc L.Like2
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like1),
>        Instruction [S] Always (To L.Like3),
>        Instruction [E] Always (To L.Like4)],

>   MakeLoc L.Like3
>       allAlike Nothing [Twist_Hint]
>       [Instruction [E] Always (To L.Like2),
>        Instruction [D] Always (To L.Dead5),
>        Instruction [S] Always (To L.Like6),
>        Instruction [N] Always (To L.Dead9)],

>   MakeLoc L.Like4
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like1),
>        Instruction [N] Always (To L.Like2),
>        Instruction [E] Always (To L.Dead3),
>        Instruction [S] Always (To L.Dead4),
>        Instruction [U,D] Always (To L.Like14)],

>   MakeLoc L.Like5
>       allAlike Nothing [Twist_Hint]
>       [Instruction [E] Always (To L.Like6),
>        Instruction [W] Always (To L.Like7)],

>   MakeLoc L.Like6
>       allAlike Nothing [Twist_Hint]
>       [Instruction [E] Always (To L.Like3),
>        Instruction [W] Always (To L.Like5),
>        Instruction [D] Always (To L.Like7),
>        Instruction [S] Always (To L.Like8)],

>   MakeLoc L.Like7
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like5),
>        Instruction [U] Always (To L.Like6),
>        Instruction [E] Always (To L.Like8),
>        Instruction [S] Always (To L.Like9)],

>   MakeLoc L.Like8
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like6),
>        Instruction [E] Always (To L.Like7),
>        Instruction [S] Always (To L.Like8),
>        Instruction [U] Always (To L.Like9),
>        Instruction [N] Always (To L.Like10),
>        Instruction [D] Always (To L.Dead11)],

>   MakeLoc L.Like9
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like7),
>        Instruction [N] Always (To L.Like8),
>        Instruction [S] Always (To L.Dead6)],

>    MakeLoc L.Like10
>       allAlike Nothing [Twist_Hint]
>       [Instruction [W] Always (To L.Like8),
>        Instruction [N] Always (To L.Like10),
>        Instruction [D] Always (To L.Dead7),
>        Instruction [E] Always (To L.Brink)],

>   MakeLoc L.Like11
>       allAlike Nothing [Twist_Hint]
>       [Instruction [N] Always (To L.Like1),
>        Instruction [W,S] Always (To L.Like11),
>        Instruction [E] Always (To L.Dead1)],

>   MakeLoc L.Like12
>       allAlike Nothing [Twist_Hint]
>       [Instruction [S] Always (To L.Brink),
>        Instruction [E] Always (To L.Like13),
>        Instruction [W] Always (To L.Dead10)],

>   MakeLoc L.Like13
>       allAlike Nothing [Twist_Hint]
>       [Instruction [N] Always (To L.Brink),
>        Instruction [W] Always (To L.Like12),
>        Instruction [NW] Always (To L.Dead2)],

>   MakeLoc L.Like14
>       allAlike Nothing [Twist_Hint] [Instruction [U,D] Always (To L.Like4)],

>   MakeLoc L.Brink
>       "You are on the brink of a thirty-foot pit with a massive orange column\n\
>       \down one wall.  You could climb down here but you could not get back\n\
>       \up.  The maze continues at this level."
>       (Just "You're at brink of pit.")
>       []
>       [Instruction [D,Climb] Always (To L.Bird),
>        Instruction [W] Always (To L.Like10),
>        Instruction [S] Always (To L.Dead8),
>        Instruction [N] Always (To L.Like12),
>        Instruction [E] Always (To L.Like13)],

  Crawling west from wmist instead of south, you encounter this.

>   MakeLoc L.Elong
>       "You are at the east end of a very long hall apparently without side\n\
>       \chambers.  To the east a low wide crawl slants up.  To the north a\n\
>       \round two-foot hole slants down."
>       (Just "You're at east end of long hall.")
>       []
>       [Instruction [E,U,Crawl] Always (To L.Wmist),
>        Instruction [W] Always (To L.Wlong),
>        Instruction [N,D,Hole] Always (To L.Cross)],

>   MakeLoc L.Wlong
>       "You are at the west end of a very long featureless hall.  The hall\n\
>       \joins up with a narrow north/south passage."
>       (Just "You're at west end of long hall.")
>       []
>       [Instruction [E] Always (To L.Elong),
>        Instruction [N] Always (To L.Cross),
>        Instruction [S] NotDwarf (To L.Diff0)],

  Recall that the last instruction above means, "Dwarves not
  permitted." It keeps them out of the following maze, which is based on an
  11 × 11 latin square. (Each of the eleven locations leads to each of the
  others under the ten motions N, S, E, W, NE, SE, NW, SW, U, D — except
  that diff0 goes down to the entrance location wlong instead of to diff10,
  and diff10 goes south to the dead-end location pony instead of to diff0.
  Furthermore, each location is accessible from all ten possible
  directions.)  Incidentally, if you ever get into a "little twisting maze
  of passages," you're really lost.

>   MakeLoc L.Diff0
>       "You are in a maze of twisty little passages, all different."
>       Nothing
>       []
>       [Instruction [S] Always (To L.Diff1),
>        Instruction [SW] Always (To L.Diff2),
>        Instruction [NE] Always (To L.Diff3),
>        Instruction [SE] Always (To L.Diff4),
>        Instruction [U] Always (To L.Diff5),
>        Instruction [NW] Always (To L.Diff6),
>        Instruction [E] Always (To L.Diff7),
>        Instruction [W] Always (To L.Diff8),
>        Instruction [N] Always (To L.Diff9),
>        Instruction [D] Always (To L.Wlong)],

>   MakeLoc L.Diff1
>       "You are in a maze of twisting little passages, all different."
>       Nothing
>       []
>       [Instruction [W] Always (To L.Diff0),
>        Instruction [SE] Always (To L.Diff2),
>        Instruction [NW] Always (To L.Diff3),
>        Instruction [SW] Always (To L.Diff4),
>        Instruction [NE] Always (To L.Diff5),
>        Instruction [U] Always (To L.Diff6),
>        Instruction [D] Always (To L.Diff7),
>        Instruction [N] Always (To L.Diff8),
>        Instruction [S] Always (To L.Diff9),
>        Instruction [E] Always (To L.Diff10)],

>   MakeLoc L.Diff2
>       "You are in a little maze of twisty passages, all different."
>       Nothing
>       []
>       [Instruction [NW] Always (To L.Diff0),
>        Instruction [U] Always (To L.Diff1),
>        Instruction [N] Always (To L.Diff3),
>        Instruction [S] Always (To L.Diff4),
>        Instruction [W] Always (To L.Diff5),
>        Instruction [SW] Always (To L.Diff6),
>        Instruction [NE] Always (To L.Diff7),
>        Instruction [E] Always (To L.Diff8),
>        Instruction [D] Always (To L.Diff9),
>        Instruction [SE] Always (To L.Diff10)],

>   MakeLoc L.Diff3
>       "You are in a twisting maze of little passages, all different."
>       Nothing
>       []
>       [Instruction [U] Always (To L.Diff0),
>        Instruction [D] Always (To L.Diff1),
>        Instruction [W] Always (To L.Diff2),
>        Instruction [NE] Always (To L.Diff4),
>        Instruction [SW] Always (To L.Diff5),
>        Instruction [E] Always (To L.Diff6),
>        Instruction [N] Always (To L.Diff7),
>        Instruction [NW] Always (To L.Diff8),
>        Instruction [SE] Always (To L.Diff9),
>        Instruction [S] Always (To L.Diff10)],

>   MakeLoc L.Diff4
>       "You are in a twisting little maze of passages, all different."
>       Nothing
>       []
>       [Instruction [NE] Always (To L.Diff0),
>        Instruction [N] Always (To L.Diff1),
>        Instruction [NW] Always (To L.Diff2),
>        Instruction [SE] Always (To L.Diff3),
>        Instruction [E] Always (To L.Diff5),
>        Instruction [D] Always (To L.Diff6),
>        Instruction [S] Always (To L.Diff7),
>        Instruction [U] Always (To L.Diff8),
>        Instruction [W] Always (To L.Diff9),
>        Instruction [SW] Always (To L.Diff10)],

>   MakeLoc L.Diff5
>       "You are in a twisty little maze of passages, all different."
>       Nothing
>       []
>       [Instruction [N] Always (To L.Diff0),
>        Instruction [SE] Always (To L.Diff1),
>        Instruction [D] Always (To L.Diff2),
>        Instruction [S] Always (To L.Diff3),
>        Instruction [E] Always (To L.Diff4),
>        Instruction [W] Always (To L.Diff6),
>        Instruction [SW] Always (To L.Diff7),
>        Instruction [NE] Always (To L.Diff8),
>        Instruction [NW] Always (To L.Diff9),
>        Instruction [U] Always (To L.Diff10)],

>   MakeLoc L.Diff6
>       "You are in a twisty maze of little passages, all different."
>       Nothing
>       []
>       [Instruction [E] Always (To L.Diff0),
>        Instruction [W] Always (To L.Diff1),
>        Instruction [U] Always (To L.Diff2),
>        Instruction [SW] Always (To L.Diff3),
>        Instruction [D] Always (To L.Diff4),
>        Instruction [S] Always (To L.Diff5),
>        Instruction [NW] Always (To L.Diff7),
>        Instruction [SE] Always (To L.Diff8),
>        Instruction [NE] Always (To L.Diff9),
>        Instruction [N] Always (To L.Diff10)],

>   MakeLoc L.Diff7
>       "You are in a little twisty maze of passages, all different."
>       Nothing
>       []
>       [Instruction [SE] Always (To L.Diff0),
>        Instruction [NE] Always (To L.Diff1),
>        Instruction [S] Always (To L.Diff2),
>        Instruction [D] Always (To L.Diff3),
>        Instruction [U] Always (To L.Diff4),
>        Instruction [NW] Always (To L.Diff5),
>        Instruction [N] Always (To L.Diff6),
>        Instruction [SW] Always (To L.Diff8),
>        Instruction [E] Always (To L.Diff9),
>        Instruction [W] Always (To L.Diff10)],

>   MakeLoc L.Diff8
>       "You are in a maze of little twisting passages, all different."
>       Nothing
>       []
>       [Instruction [D] Always (To L.Diff0),
>        Instruction [E] Always (To L.Diff1),
>        Instruction [NE] Always (To L.Diff2),
>        Instruction [U] Always (To L.Diff3),
>        Instruction [W] Always (To L.Diff4),
>        Instruction [N] Always (To L.Diff5),
>        Instruction [S] Always (To L.Diff6),
>        Instruction [SE] Always (To L.Diff7),
>        Instruction [SW] Always (To L.Diff9),
>        Instruction [NW] Always (To L.Diff10)],

>   MakeLoc L.Diff9
>       "You are in a maze of little twisty passages, all different."
>       Nothing
>       []
>       [Instruction [SW] Always (To L.Diff0),
>        Instruction [NW] Always (To L.Diff1),
>        Instruction [E] Always (To L.Diff2),
>        Instruction [W] Always (To L.Diff3),
>        Instruction [N] Always (To L.Diff4),
>        Instruction [D] Always (To L.Diff5),
>        Instruction [SE] Always (To L.Diff6),
>        Instruction [U] Always (To L.Diff7),
>        Instruction [S] Always (To L.Diff8),
>        Instruction [NE] Always (To L.Diff10)],

>   MakeLoc L.Diff10
>       "You are in a little maze of twisting passages, all different."
>       Nothing
>       []
>       [Instruction [SW] Always (To L.Diff1),
>        Instruction [N] Always (To L.Diff2),
>        Instruction [E] Always (To L.Diff3),
>        Instruction [NW] Always (To L.Diff4),
>        Instruction [SE] Always (To L.Diff5),
>        Instruction [NE] Always (To L.Diff6),
>        Instruction [W] Always (To L.Diff7),
>        Instruction [D] Always (To L.Diff8),
>        Instruction [U] Always (To L.Diff9),
>        Instruction [S] Always (To L.Pony)],
>
>   MakeLoc L.Pony
>       deadEnd Nothing [] [Instruction [N,Out] Always (To L.Diff10)],

  Going north of the long hall, we come to the vicinity of another large
  room, with royal treasures nearby.  (You probably first reached this part
  of the cavern from the east, via the Hall of Mists.) Unfortunately, a
  vicious snake is here too; the conditional instructions for getting past
  the snake are worthy of study.

>   MakeLoc L.Cross
>       "You are at a crossover of a high N/S passage and a low E/W one."
>       Nothing []
>       [Instruction [W] Always (To L.Elong),
>        Instruction [N] Always (To L.Dead0),
>        Instruction [E] Always (To L.West),
>        Instruction [S] Always (To L.Wlong)],

>   MakeLoc L.Hmk
>       "You are in the Hall of the Mountain King  with passages off in all\n\
>       \directions."
>       (Just "You're in Hall of Mt King.")
>       [Snake_Hint]
>       [Instruction [Stairs,U,E] Always (To L.Emist),
>        Instruction [N,L] (Not Snake 0) (To L.NS),
>        Instruction [S,R] (Not Snake 0) (To L.South),
>        Instruction [W,Forward] (Not Snake 0) (To L.West),
>        Instruction [N] Always (To L.Snaked),
>        Instruction [SW] (Percent 35) (To L.Secret),
>        Instruction [SW] (Sees Snake) (To L.Snaked),
>        Instruction [Secret] Always (To L.Secret)],

>   MakeLoc L.West
>       "You are in the west side chamber of the Hall of the Mountain King.\n\
>       \A passage continues west and up here."
>       (Just "You're in west side chamber.")
>       []
>       [Instruction [Hall,Out,E] Always (To L.Hmk),
>        Instruction [W,U] Always (To L.Cross)],

>   MakeLoc L.South
>       "You are in the south side chamber." Nothing []
>       [Instruction [Hall,Out,N] Always (To L.Hmk)],

  North of the mountain king's domain is a curious shuttle station called
  Y2, with magic connections to two other places.  (Real-world cave maps
  often use the symbol Y to stand for an entrance, and Y2 for a secondary
  entrance.

>   MakeLoc L.NS
>       "You are in a low N/S passage at a hole in the floor.  The hole goes\n\
>       \down to an E/W passage."
>       (Just "You're in N/S passage.")
>      []
>       [Instruction [Hall,Out,S] Always (To L.Hmk),
>        Instruction [N,Y2] Always (To L.Y2),
>        Instruction [D,Hole] Always (To L.Dirty)],

>   MakeLoc L.Y2
>       "You are in a large room, with a passage to the south, a passage to the\n\
>       \west, and a wall of broken rock to the east.  There is a large \"Y2\" on\n\
>       \a rock in the room's center."
>       (Just "You're at \"Y2\".")
>      []
>       [Instruction [Plugh] Always (To L.House),
>        Instruction [S] Always (To L.NS),
>        Instruction [E,Wall,Broken] Always (To L.Jumble),
>        Instruction [W] Always (To L.Windoe),
>        Instruction [Plover] (Holds Emerald) (To L.Pdrop),
>        Instruction [Plover] Always (To L.Proom)],

>   MakeLoc L.Jumble
>       "You are in a jumble of rock, with cracks everywhere."  Nothing  []
>       [Instruction [D,Y2] Always (To L.Y2),
>        Instruction [U] Always (To L.Emist)],

>   MakeLoc L.Windoe
>       "You're at a low window overlooking a huge pit, which extends up out of\n\
>       \sight.  A floor is indistinctly visible over 50 feet below.  Traces of\n\
>       \white mist cover the floor of the pit, becoming thicker to the right.\n\
>       \Marks in the dust around the window would seem to indicate that\n\
>       \someone has been here recently.  Directly across the pit from you and\n\
>       \25 feet away there is a similar window looking into a lighted room.\n\
>       \A shadowy figure can be seen there peering back at you."
>       (Just "You're at window on pit.")
>       []
>       [Instruction [E,Y2] Always (To L.Y2),
>        Instruction [Jump] Always (To L.Neck)],

  Next let's consider the east/west passage below ns.

>   MakeLoc L.Dirty
>       "You are in a dirty broken passage.  To the east is a crawl.  To the\n\
>       \west is a large passage.  Above you is a hole to another passage."
>       (Just "You're in dirty passage.")
>       []
>       [Instruction [E,Crawl] Always (To L.Clean),
>        Instruction [U,Hole] Always (To L.NS),
>        Instruction [W] Always (To L.Dusty),
>        Instruction [Bedquilt] Always (To L.Bedquilt)],

>   MakeLoc L.Clean
>       "You are on the brink of a small clean climbable pit.  A crawl leads\n\
>        \west."
>       (Just "You're by a clean pit.")
>       []
>       [Instruction [W,Crawl] Always (To L.Dirty),
>        Instruction [D,Pit,Climb] Always (To L.Wet)],

>   MakeLoc L.Wet
>       "You are in the bottom of a small pit with a little stream, which\n\
>       \enters and exits through tiny slits."
>       (Just "You're in pit by stream.") [Liquid]
>       [Instruction [Climb,U,Out] Always (To L.Clean),
>        Instruction [Slit,Stream,D,Upstream,Downstream] Always (Sayit slitRmk)],

>   MakeLoc L.Dusty
>       "You are in a large room full of dusty rocks.  There is a big hole in\n\
>       \the floor.  There are cracks everywhere, and a passage leading east."
>       (Just "You're in dusty rock room.")
>       []
>       [Instruction [E,Passage] Always (To L.Dirty),
>        Instruction [D,Hole,Floor] Always (To L.Complex),
>        Instruction [Bedquilt] Always (To L.Bedquilt)],

>   MakeLoc L.Complex
>       "You are at a complex junction.  A low hands-and-knees passage from the\n\
>       \north joins a higher crawl from the east to make a walking passage\n\
>       \going west.  There is also a large room above.  The air is damp here."
>       (Just "You're at complex junction.")
>       []
>       [Instruction [U,Climb,Room] Always (To L.Dusty),
>        Instruction [W,Bedquilt] Always (To L.Bedquilt),
>        Instruction [N,Shell] Always (To L.Shell),
>        Instruction [E] Always (To L.Ante)],

  A more-or-less self-contained cavelet can be found north of the complex
  passage. Its connections are more vertical than horizontal.

>   MakeLoc L.Shell
>       "You're in a large room carved out of sedimentary rock.  The floor\n\
>       \and walls are littered with bits of shells embedded in the stone.\n\
>       \A shallow passage proceeds downward  and a somewhat steeper one\n\
>       \leads up.  A low hands-and-knees passage enters from the south."
>       (Just "You're in Shell Room.")
>       []
>       [Instruction [U,Hall] Always (To L.Arch),
>        Instruction [D] Always (To L.Ragged),
>        Instruction [S] (Holds Clam)
>           (Sayit "You can't fit this five-foot clam through that little passage!"),
>        Instruction [S] (Holds Oyster)
>           (Sayit "You can't fit this five-foot oyster through that little passage!"),
>        Instruction [S] Always (To L.Complex)],

>   MakeLoc L.Arch
>       "You are in an arched hall.  A coral passage once continued up and east\n\
>       \from here, but is now blocked by debris.  The air smells of sea water."
>       (Just "You're in arched hall.")
>       []
>       [Instruction [D,Shell,Out] Always (To L.Shell)],

>   MakeLoc L.Ragged
>       "You are in a long sloping corridor with ragged sharp walls." Nothing []
>       [Instruction [U,Shell] Always (To L.Shell),
>        Instruction [D] Always (To L.Sac)],

>   MakeLoc L.Sac
>       "You are in a cul-de-sac about eight feet across." Nothing []
>       [Instruction [U,Out] Always (To L.Ragged),
>        Instruction [Shell] Always (To L.Shell)],

  A dangerous section lies east of the complex junction.

>   MakeLoc L.Ante
>       "You are in an anteroom leading to a large passage to the east.  Small\n\
>       \passages go west and up.  The remnants of recent digging are evident.\n\
>       \A sign in midair here says \"CAVE UNDER CONSTRUCTION BEYOND THIS POINT.\n\
>       \PROCEED AT OWN RISK. [WITT CONSTRUCTION COMPANY]\""
>       (Just "You're in anteroom.")
>       []
>       [Instruction [U] Always (To L.Complex),
>        Instruction [W] Always (To L.Bedquilt),
>        Instruction [E] Always (To L.Witt)],

>   MakeLoc L.Witt
>       "You are at Witt's End.  Passages lead off in \"all\" directions."
>       (Just "You're at Witt's End.")
>       [Witt_Hint]
>       [Instruction [E,N,S,NE,SE,SW,NW,U,D] (Percent 95) (Sayit loopRmk),
>        Instruction [E] Always (To L.Ante),

  one chance in 20

>        Instruction [W] Always (Sayit "You have crawled around in some little holes and found your way\n\
>                                   \blocked by a recent cave-in.  You are now back in the main passage.")],

  Will Crowther,  who actively  explored and mapped  many caves  in Kentucky
  before inventing Adventure, named  Bedquilt after the Bedquilt Entrance to
  Colossal Cave. (The real Colossal Cave was discovered near Mammoth Cave in
  1895, and its Bedquilt Entrance was found in 1896; see The Longest Cave by
  Brucker and Watson  (New York: Knopf, 1976) for  further details.)  Random
  exploration is the name of the game here.

>   MakeLoc L.Bedquilt
>       "You are in Bedquilt, a long east/west passage with holes everywhere.\n\
>       \To explore at random select north, south, up, or down."
>       (Just "You're in Bedquilt.")
>       []
>       [Instruction [E] Always (To L.Complex),
>        Instruction [W] Always (To L.Cheese),
>        Instruction [S] (Percent 80) (Sayit loopRmk),
>        Instruction [Slab] Always (To L.Slab),
>        Instruction [U] (Percent 80) (Sayit loopRmk),
>        Instruction [U] (Percent 50) (To L.Abovep),
>        Instruction [U] Always (To L.Dusty),
>        Instruction [N] (Percent 60) (Sayit loopRmk),
>        Instruction [N] (Percent 75) (To L.Low),
>        Instruction [N] Always (To L.Sjunc),
>        Instruction [D] (Percent 80) (Sayit loopRmk),
>        Instruction [D] Always (To L.Ante)],

>   MakeLoc L.Cheese
>       "You are in a room whose walls resemble Swiss cheese.  Obvious passages\n\
>       \go west, east, NE, and NW. Part of the room is occupied by a large\n\
>       \bedrock block."
>       (Just "You're in Swiss cheese room.")
>       []
>       [Instruction [NE] Always (To L.Bedquilt),
>        Instruction [W] Always (To L.E2pit),
>        Instruction [S] (Percent 80) (Sayit loopRmk),
>        Instruction [Canyon] Always (To L.Tall),
>        Instruction [E] Always (To L.Soft),
>        Instruction [NW] (Percent 50) (Sayit loopRmk),
>        Instruction [Oriental] Always (To L.Oriental)],

>   MakeLoc L.Soft
>       "You are in the Soft Room.  The walls are covered with heavy curtains,\n\
>       \the floor with a thick pile carpet.  Moss covers the ceiling."
>       (Just "You're in Soft Room.")
>       []
>       [Instruction [W,Out] Always (To L.Cheese)],

  West of the quilt and the cheese is a room with two pits. Why would you
  want to descend into the pits? Keep playing and you'll find out.

>   MakeLoc L.E2pit
>       "You are at the east end of the Twopit Room.  The floor here is\n\
>       \littered with thin rock slabs, which make it easy to descend the pits.\n\
>       \There is a path here bypassing the pits to connect passages from east\n\
>       \and west.  There are holes all over, but the only big one is on the\n\
>       \wall directly over the west pit where you can't get to it."
>       (Just "You're at east end of Twopit Room.")
>       []
>       [Instruction [E] Always (To L.Cheese),
>        Instruction [W,Across] Always (To L.W2pit),
>        Instruction [D,Pit] Always (To L.Epit)],

>   MakeLoc L.W2pit
>       "You are at the west end of the Twopit Room.  There is a large hole in\n\
>       \the wall above the pit at this end of the room."
>       (Just "You're at west end of Twopit Room.")
>       []
>       [Instruction [E,Across] Always (To L.E2pit),
>        Instruction [W,Slab] Always (To L.Slab),
>        Instruction [D,Pit] Always (To L.Wpit),
>        Instruction [Hole] Always (Sayit "It is too far up for you to reach.")],

>   MakeLoc L.Epit
>       "You are at the bottom of the eastern pit in the Twopit Room.  There is\n\
>       \a small pool of oil in one corner of the pit."
>       (Just "You're in east pit.")
>       [Liquid,Oil]
>       [Instruction [U,Out] Always (To L.E2pit)],

>   MakeLoc L.Wpit
>       "You are at the bottom of the western pit in the Twopit Room.  There is\n\
>       \a large hole in the wall about 25 feet above you."
>       (Just "You're in west pit.")
>       []
>       [Instruction [U,Out] Always (To L.W2pit),
>        Instruction [Climb] (Not Plant 4) (To L.Check),
>        Instruction [Climb] Always (To L.Climb)],

  Oho, you climbed the plant in the west pit! Now you're in another scenic
  area with rare treasures—if you can get through the door.

>   MakeLoc L.Narrow
>       "You are in a long, narrow corridor stretching out of sight to the\n\
>       \west.  At the eastern end is a hole through which you can see a\n\
>       \profusion of leaves."
>       (Just "You're in narrow corridor.")
>       []
>       [Instruction [D,Climb,E] Always (To L.Wpit),
>        Instruction [Jump] Always (To L.Neck),
>        Instruction [W,Giant] Always (To L.Giant)],

>   MakeLoc L.Giant
>       "You are in the Giant Room.  The ceiling here is too high up for your\n\
>       \lamp to show it.  Cavernous passages lead east, north, and south.  On\n\
>       \the west wall is scrawled the inscription, \"FEE FIE FOE FOO\" [sic]."
>       (Just "You're in Giant Room.")
>       []
>       [Instruction [S] Always (To L.Narrow),
>        Instruction [E] Always (To L.Block),
>        Instruction [N] Always (To L.Immense)],

>   MakeLoc L.Block
>       "The passage here is blocked by a recent cave-in." Nothing []
>       [Instruction [S,Giant,Out] Always (To L.Giant)],

>   MakeLoc L.Immense
>       "You are at one end of an immense north/south passage." Nothing []
>       [Instruction [S,Giant,Passage] Always (To L.Giant),
>        Instruction [N,Enter,Cavern] (Not Door 0) (To L.Falls),
>        Instruction [N] Always
>                     (Sayit "The door is extremely rusty and refuses to open.")],

>   MakeLoc L.Falls
>       "You are in a magnificent cavern with a rushing stream, which cascades\n\
>       \over a sparkling waterfall into a roaring whirlpool that disappears\n\
>       \through a hole in the floor.  Passages exit to the south and west."
>       (Just "You're in cavern with waterfall.")
>       [Liquid]
>       [Instruction [S,Out] Always (To L.Immense),
>        Instruction [Giant] Always (To L.Giant),
>        Instruction [W] Always (To L.Steep)],

>   MakeLoc L.Steep
>       "You are at the top of a steep incline above a large room.  You could\n\
>       \climb down here, but you would not be able to climb up.  There is a\n\
>       \passage leading back to the north."
>       (Just "You're at steep incline above large room.")
>       []
>       [Instruction [N,Cavern,Passage] Always (To L.Falls),
>        Instruction [D,Climb] Always (To L.Low)],

  Meanwhile let's backtrack to another part of the cave possibly reachable
  from Bedquilt.

>   MakeLoc L.Abovep
>       "You are in a secret N/S canyon above a sizable passage." Nothing []
>       [Instruction [N] Always (To L.Sjunc),
>        Instruction [D,Passage] Always (To L.Bedquilt),
>        Instruction [S] Always (To L.Tite)],

>   MakeLoc L.Sjunc
>       "You are in a secret canyon at a junction of three canyons, bearing\n\
>       \north, south, and SE. The north one is as tall as the other two\n\
>       \combined."
>       (Just "You're at junction of three secret canyons.")
>       []
>       [Instruction [SE] Always (To L.Bedquilt),
>        Instruction [S] Always (To L.Abovep),
>        Instruction [N] Always (To L.Window)],

>   MakeLoc L.Tite
>       "A large stalactite extends from the roof and almost reaches the floor\n\
>       \below.  You could climb down it, and jump from it to the floor, but\n\
>       \having done so you would be unable to reach it to climb back up."
>       (Just "You're on top of stalactite.")
>       []
>       [Instruction [N] Always (To L.Abovep),
>        Instruction [D,Jump,Climb] (Percent 40) (To L.Like6),
>        Instruction [D] (Percent 50) (To L.Like9),
>        Instruction [D] Always (To L.Like4)],

  oh dear, you're in a random part of the maze

>   MakeLoc L.Low
>       "You are in a large low room.  Crawls lead north, SE, and SW."
>       Nothing
>       []
>       [Instruction [Bedquilt] Always (To L.Bedquilt),
>        Instruction [SW] Always (To L.Scorr),
>        Instruction [N] Always (To L.Crawl),
>        Instruction [SE,Oriental] Always (To L.Oriental)],

>   MakeLoc L.Crawl
>       "Dead end crawl." Nothing []
>       [Instruction [S,Crawl,Out] Always (To L.Low)],

  The described view from the west window, window, is identical to the view
  from the east window, windoe, except for one word. What on earth do you
  see from those windows? (Don Woods has confided that the shadowy figure is
  actually your own reflection, because mirror lies between the two window
  rooms.  An intentional false clue.

>   MakeLoc L.Window
>       "You're at a low window overlooking a huge pit, which extends up out of\n\
>       \sight.  A floor is indistinctly visible over 50 feet below.  Traces of\n\
>       \white mist cover the floor of the pit, becoming thicker to the left.\n\
>       \Marks in the dust around the window would seem to indicate that\n\
>       \someone has been here recently.  Directly across the pit from you and\n\
>       \25 feet away there is a similar window looking into a lighted room.\n\
>       \A shadowy figure can be seen there peering back to you."
>       (shortDesc L.Windoe)
>       []
>       [Instruction [W] Always (To L.Sjunc),
>       Instruction [Jump] Always (To L.Neck)],

  More treasures await you via the low corridor.

>   MakeLoc L.Oriental
>       "This is the Oriental Room.  Ancient oriental cave drawings cover the\n\
>       \walls.  A gently sloping passage leads upward to the north, another\n\
>       \passage leads SE, and a hands-and-knees crawl leads west."
>       (Just "You're in Oriental Room.")
>       []
>       [Instruction [SE] Always (To L.Cheese),
>        Instruction [W,Crawl] Always (To L.Low),
>        Instruction [U,N,Cavern] Always (To L.Misty)],

>   MakeLoc L.Misty
>       "You are following a wide path around the outer edge of a large cavern.\n\
>       \Far below, through a heavy white mist, strange splashing noises can be\n\
>       \heard.  The mist rises up through a fissure in the ceiling.  The path\n\
>       \exits to the south and west."
>       (Just "You're in misty cavern.")
>       []
>       [Instruction [S,Oriental] Always (To L.Oriental),
>        Instruction [W] Always (To L.Alcove)],

  One of the darkest secrets is hidden here. You will discover that you must
  take the emerald from the Plover Room to the alcove. But you don't learn
  the name of the Plover Room until the second time you've been there, since
  your first visit will be lampless until you know the secret.

>   MakeLoc L.Alcove
>       "You are in an alcove.  A small NW path seems to widen after a short\n\
>       \distance.  An extremely tight tunnel leads east.  It looks like a very\n\
>       \tight squeeze.  An eerie light can be seen at the other end."
>       (Just "You're in alcove.")
>       [Dark_Hint]
>       [Instruction [NW,Cavern] Always (To L.Misty),
>        Instruction [E,Passage] Always (To L.Ppass),
>        Instruction [E] Always (To L.Proom)],

  never performed, but seen by 'go back'

>   MakeLoc L.Proom
>       "You're in a small chamber lit by an eerie green light.  An extremely\n\
>       \narrow tunnel exits to the west.  A dark corridor leads NE."
>       (Just "You're in Plover Room.")
>       [Dark_Hint]
>       [Instruction [W,Passage,Out] Always (To L.Ppass),
>        Instruction [W] Always (To L.Alcove),

  never performed, but seen by 'go back'

>        Instruction [Plover] (Holds Emerald) (To L.Pdrop),
>        Instruction [Plover] Always (To L.Y2),
>        Instruction [NE,Dark] Always (To L.Droom)],

>   MakeLoc L.Droom
>       "You're in the Dark-Room.  A corridor leading south is the only exit."
>       (Just "You're in Dark-Room.")
>       [Dark_Hint]
>       [Instruction [S,Plover,Out] Always (To L.Proom)],

  We forgot to mention the circuitous passage leading west from the Twopit
  Room. It winds around and takes you to a somewhat more mundane area, yet
  not without interest.

>   MakeLoc L.Slab
>       "You are in a large low circular chamber whose floor is an immense slab\n\
>       \fallen from the ceiling (Slab Room). There once were large passages\n\
>       \to the east and west, but they are now filled with boulders.  Low\n\
>       \small passages go north and south, and the south one quickly bends\n\
>       \west around the boulders."
>       (Just "You're in Slab Room.")
>       []
>       [Instruction [S] Always (To L.W2pit),
>        Instruction [U,Climb] Always (To L.Abover),
>        Instruction [N] Always (To L.Bedquilt)],

>   MakeLoc L.Abover
>       "You are in a secret N/S canyon above a large room." Nothing []
>       [Instruction [D,Slab] Always (To L.Slab),
>        Instruction [S] (Not Dragon 0) (To L.Scan2),
>        Instruction [S] Always (To L.Scan1),
>        Instruction [N] Always (To L.Mirror),
>        Instruction [Reservoir] Always (To L.Res)],

>   MakeLoc L.Mirror
>       "You are in a north/south canyon about 25 feet across.  The floor is\n\
>       \covered by white mist seeping in from the north.  The walls extend\n\
>       \upward for well over 100 feet.  Suspended from some unseen point far\n\
>       \above you, an enormous two-sided mirror is hanging parallel to and\n\
>       \midway between the canyon walls. (The mirror is obviously provided\n\
>       \for the use of the dwarves, who as you know are extremely vain.)\n\
>       \A small window can be seen in either wall, some fifty feet up."
>       (Just "You're in mirror canyon.")
>       []
>       [Instruction [S] Always (To L.Abover),
>        Instruction [N,Reservoir] Always (To L.Res)],

>   MakeLoc L.Res
>       "You are at the edge of a large underground reservoir.  An opaque cloud\n\
>       \of white mist fills the room and rises rapidly upward.  The lake is\n\
>       \fed by a stream, which tumbles out of a hole in the wall about 10 feet\n\
>       \overhead and splashes noisily into the water somewhere within the\n\
>       \mist.  The only passage goes back toward the south."
>       (Just "You're at reservoir.")
>       [Liquid]
>       [Instruction [S,Out] Always (To L.Mirror)],

  Four more secret canyons lead back to the Hall of the Mountain King. Three
  of them are actually the same, but the dragon blocks the connection
  between the northern passage (to abover ) and the eastern passage (to
  secret ). Once you've vanquished the dragon, scan2 takes the place of
  scan1 and scan3 .

>   MakeLoc L.Scan1
>       "You are in a secret canyon that exits to the north and east."
>       Nothing
>       []
>       [Instruction [N,Out] Always (To L.Abover),
>        Instruction [E,Forward] Always
>           (Sayit "The dragon looks rather nasty.  You'd best not try to get by.")],

>   MakeLoc L.Scan2
>       (longDesc L.Scan1) Nothing []
>       [Instruction [N] Always (To L.Abover),
>        Instruction [E] Always (To L.Secret)],

>   MakeLoc L.Scan3
>       (longDesc L.Scan1) Nothing []
>       [Instruction [E,Out] Always (To L.Secret),
>        Instruction [N,Forward] Always (Sayit  "The dragon looks rather nasty.  You'd best not try to get by.")],

>   MakeLoc L.Secret
>       "You are in a secret canyon, which here runs E/W. It crosses over a\n\
>       \very tight canyon 15 feet below.  If you go down you may not be able\n\
>       \to get back up."
>       (Just "You're in secret E/W canyon above tight canyon.")
>       []
>       [Instruction [E] Always (To L.Hmk),
>        Instruction [W] (Not Dragon 0) (To L.Scan2),
>        Instruction [W] Always (To L.Scan3),
>        Instruction [D] Always (To L.Wide)],

   Below secret there's another way to reach the cheese.

>   MakeLoc L.Wide
>       "You are at a wide place in a very tight N/S canyon." Nothing []
>       [Instruction [S] Always (To L.Tight),
>        Instruction [N] Always (To L.Tall)],

>   MakeLoc L.Tight
>       "The canyon here becomes too tight to go further south." Nothing []
>       [Instruction [N] Always (To L.Wide)],

>   MakeLoc L.Tall
>       "You are in a tall E/W canyon.  A low tight crawl goes 3 feet north and\n\
>       \seems to open up."
>       (Just "You're in tall E/W canyon.")
>       []
>       [Instruction [E] Always (To L.Wide),
>        Instruction [W] Always (To L.Boulders),
>        Instruction [N,Crawl] Always (To L.Cheese)],

>   MakeLoc L.Boulders
>       "The canyon runs into a mass of boulders --- dead end."
>       Nothing
>       []
>       [Instruction [S] Always (To L.Tall)],

  If you aren't having fun yet, wait till you meet the troll. The only way
  to get here is to crawl southwest from the low room. And then you have a
  new problem to solve; we'll see later that the Troll and the BRIDGE are
  here.  (Don Woods got the idea for the mist-covered bridge after an early
  morning visit to Mount Diablo; see Steven Levy, Hackers (New York: (To Delta),
  1994), Chapter 7.)

>   MakeLoc L.Scorr
>       "You are in a long winding corridor sloping out of sight in both\n\
>       \directions."
>       (Just "You're in sloping corridor.")
>       []
>       [Instruction [D] Always (To L.Low),
>        Instruction [U] Always (To L.SWside)],

>   MakeLoc L.SWside
>       "You are on one side of a large, deep chasm.  A heavy white mist rising\n\
>       \up from below obscures all view of the far side.  A SW path leads away\n\
>       \from the chasm into a winding corridor."
>       (Just "You're on SW side of chasm.")
>       []
>       [Instruction [SW] Always (To L.Scorr),
>        Instruction [Over,Across,Cross,NE] (Sees Troll)
>                      (Sayit "The troll refuses to let you cross."),
>        Instruction [Over] (Not Troll 0)
>                      (Sayit "There is no longer any way across the chasm."),
>        Instruction [Over] Always (To L.Troll),
>        Instruction [Jump] (Not Troll 0) (To L.Lose),
>        Instruction [Jump] Always (Sayit bridgeRmk)], -- ??

  The only things not yet explored on this side of the troll bridge are a
  dozen dead ends. They appear at this place in the ordering of all
  locations because of the pirate logic explained later: The pirate will
  never go to locations ≥ dead3 .

 #define max pirate loc dead2

>   MakeLoc L.Dead0
>       deadEnd  Nothing [] [Instruction [S,Out] Always (To L.Cross)],

>   MakeLoc L.Dead1
>       deadEnd  Nothing [Twist_Hint] [Instruction [W,Out] Always (To L.Like11)],

>   MakeLoc L.Dead2
>       deadEnd  Nothing [] [Instruction [SE] Always (To L.Like13)],

>   MakeLoc L.Dead3
>       deadEnd  Nothing [Twist_Hint] [Instruction [W,Out] Always (To L.Like4)],

>   MakeLoc L.Dead4
>       deadEnd  Nothing [Twist_Hint] [Instruction [E,Out] Always (To L.Like4)],

>   MakeLoc L.Dead5
>       deadEnd  Nothing [Twist_Hint] [Instruction [U,Out] Always (To L.Like3)],

>   MakeLoc L.Dead6
>       deadEnd  Nothing [Twist_Hint] [Instruction [W,Out] Always (To L.Like9)],

>   MakeLoc L.Dead7
>       deadEnd  Nothing [Twist_Hint] [Instruction [U,Out] Always (To L.Like10)],

>   MakeLoc L.Dead8
>       deadEnd  Nothing [] [Instruction [E,Out] Always (To L.Brink)],

>   MakeLoc L.Dead9
>       deadEnd  Nothing [Twist_Hint] [Instruction [S,Out] Always (To L.Like3)],

>   MakeLoc L.Dead10
>       deadEnd  Nothing [Twist_Hint] [Instruction [E,Out] Always (To L.Like12)],

>   MakeLoc L.Dead11
>       deadEnd  Nothing [Twist_Hint] [Instruction [U,Out] Always (To L.Like8)],

  A whole nuther cave with nine sites and additional treasures is on tuther
  side of the troll bridge! This cave was inspired in part by
  J. R. R. Tolkien's stories.

>   MakeLoc L.Neside
>       "You are on the far side of the chasm.  A NE path leads away from the\n\
>       \chasm on this side."
>       (Just "You're on NE side of chasm.")
>       []
>       [Instruction [NE] Always (To L.Corr),
>        Instruction [Over,Across,Cross,SW] (Sees Troll) (Sayit "The troll refuses to let you cross."), -- Maybe wrong
>        Instruction [Over] Always (To L.Troll),
>        Instruction [Jump] Always (Sayit bridgeRmk),
>        Instruction [Fork] Always (To L.Fork),
>        Instruction [View] Always (To L.View),
>        Instruction [Barren] Always (To L.Fbarr)],

>   MakeLoc L.Corr
>       "You're in a long east/west corridor.  A faint rumbling noise can be\n\
>       \heard in the distance."
>       (Just "You're in corridor.")
>       []
>       [Instruction [W] Always (To L.Neside),
>        Instruction [E,Fork] Always (To L.Fork),
>        Instruction [View] Always (To L.View),
>        Instruction [Barren] Always (To L.Fbarr)],

>   MakeLoc L.Fork
>       "The path forks here.  The left fork leads northeast.  A dull rumbling\n\
>       \seems to get louder in that direction.  The right fork leads southeast\n\
>       \down a gentle slope.  The main corridor enters from the west."
>       (Just "You're at fork in path.")
>       []
>       [Instruction [W] Always (To L.Corr),
>        Instruction [NE,L] Always (To L.Warm),
>        Instruction [SE,R,D] Always (To L.Lime),
>        Instruction [View] Always (To L.View),
>        Instruction [Barren] Always (To L.Fbarr)],

>   MakeLoc L.Warm
>       "The walls are quite warm here.  From the north can be heard a steady\n\
>       \roar, so loud that the entire cave seems to be trembling.  Another\n\
>       \passage leads south, and a low crawl goes east."
>       (Just "You're at junction with warm walls.")
>       []
>       [Instruction [S,Fork] Always (To L.Fork),
>        Instruction [N,View] Always (To L.View),
>        Instruction [E,Crawl] Always (To L.Chamber)],

>   MakeLoc L.View
>       "You are on the edge of a breath-taking view.  Far below you is an\n\
>       \active volcano, from which great gouts of molten lava come surging\n\
>       \out, cascading back down into the depths.  The glowing rock fills the\n\
>       \farthest reaches of the cavern with a blood-red glare, giving every-\n\
>       \thing an eerie, macabre appearance.  The air is filled with flickering\n\
>       \sparks of ash and a heavy smell of brimstone.  The walls are hot to\n\
>       \the touch, and the thundering of the volcano drowns out all other\n\
>       \sounds.  Embedded in the jagged roof far overhead are myriad twisted\n\
>       \formations, composed of pure white alabaster, which scatter the murky\n\
>       \light into sinister apparitions upon the walls.  To one side is a deep\n\
>       \gorge, filled with a bizarre chaos of tortured rock that seems to have\n\
>       \been crafted by the Devil himself.  An immense river of fire crashes\n\
>       \out from the depths of the volcano, burns its way through the gorge,\n\
>       \and plummets into a bottomless pit far off to your left.  To the\n\
>       \right, an immense geyser of blistering steam erupts continuously\n\
>       \from a barren island in the center of a sulfurous lake, which bubbles\n\
>       \ominously.  The far right wall is aflame with an incandescence of its\n\
>       \own, which lends an additional infernal splendor to the already\n\
>       \hellish scene.  A dark, foreboding passage exits to the south."
>       (Just "You're at breath-taking view.")
>       [Lighted]
>       [Instruction [S,Passage,Out] Always (To L.Warm),
>        Instruction [Fork] Always (To L.Fork),
>        Instruction [D,Jump] Always (Sayit $ defaultMsg Map.! Eat)],

>   MakeLoc L.Chamber
>       "You are in a small chamber filled with large boulders.  The walls are\n\
>       \very warm, causing the air in the room to be almost stifling from the\n\
>       \heat.  The only exit is a crawl heading west, through which a low\n\
>       \rumbling noise is coming."
>       (Just "You're in chamber of boulders.")
>       []
>       [Instruction [W,Out,Crawl] Always (To L.Warm),
>        Instruction [Fork] Always (To L.Fork),
>        Instruction [View] Always (To L.View)],

>   MakeLoc L.Lime
>       "You are walking along a gently sloping north/south passage lined with\n\
>       \oddly shapped limestone formations."
>       (Just "You're in limestone passage.")
>       []
>       [Instruction [N,U,Fork] Always (To L.Fork),
>        Instruction [S,D,Barren] Always (To L.Fbarr),
>        Instruction [View] Always (To L.View)],

>   MakeLoc L.Fbarr
>       "You are standing at the entrance to a large, barren room.  A sign\n\
>       \posted above the entrance reads: \"CAUTION! BEAR IN ROOM!\""
>       (Just "You're in front of barren room.")
>       []

  don't laugh too loud

>       [Instruction [W,U] Always (To L.Lime),
>        Instruction [Fork] Always (To L.Fork),
>        Instruction [E,In,Barren,Enter] Always (To L.Barr),
>        Instruction [View] Always (To L.View)],

>   MakeLoc L.Barr
>       "You are inside a barren room.  The center of the room is completely\n\
>       \empty except for some dust.  Marks in the dust lead away toward the\n\
>       \far end of the room.  The only exit is the way you came in."
>       (Just "You're in barren room.")
>       []
>       [Instruction [W,Out] Always (To L.Fbarr),
>        Instruction [Fork] Always (To L.Fork),
>        Instruction [View] Always (To L.View)],

  The two storage locations are accessible only from each other, and they
  lead only to each other.

>   MakeLoc L.Neend
>       "You are at the northeast end of an immense room, even larger than the\n\
>       \Giant Room.  It appears to be a repository for the \"Adventure\"\n\
>       \program.  Massive torches far overhead bathe the room with smoky\n\
>       \yellow light.  Scattered about you can be seen a pile of bottles (all\n\
>       \of them empty), a nursery of young beanstalks murmuring quietly, a bed\n\
>       \of oysters, a bundle of black rods with rusty stars on their ends, and\n\
>       \a collection of brass lanterns.  Off to one side a great many dwarves\n\
>       \are sleeping on the floor, snoring loudly.  A sign nearby reads: \"DO\n\
>       \NOT DISTURB THE DWARVES!\" An immense mirror is hanging against one\n\
>       \wall, and stretches to the other end of the room, where various other\n\
>       \sundry objects can be glimpsed dimly in the distance."
>       (Just "You're at NE end.")
>       [Lighted]
>       [Instruction [SW] Always (To L.Swend)],

>   MakeLoc L.Swend
>       "You are at the southwest end of the repository.  To one side is a pit\n\
>       \full of fierce green snakes.  On the other side is a row of small\n\
>       \wicker cages, each of which contains a little sulking bird.  In one\n\
>       \corner is a bundle of black rods with rusty marks on their ends.\n\
>       \A large number of velvet pillows are scattered about on the floor.\n\
>       \A vast mirror stretches off to the northeast.  At your feet is a\n\
>       \large steel grate, next to which is a sign that reads, \"TREASURE\n\
>       \VAULT. KEYS IN MAIN OFFICE.\""
>       (Just "You're at SW end.")
>       [Lighted]
>       [Instruction [NE] Always (To L.Neend),
>        Instruction [D] Always (Sayit grateRmk)],

  When the current location is crack or higher, it's a pseudo-location. In
  such cases we don't ask you for input; we assume that you have told us to
  force another instruction through. For example, if you try to go through
  the crack by the small pit in the upper cave (location Spit ), the
  instruction there sends you to crack, which immediately sends you back to
  Spit .

 #define forced move (loc ) (loc ≥ min forced loc )
 #define Force 0
/∗ actually any value will do here ∗/

>   MakeLoc L.Crack
>       "The crack is far too small for you to follow." Nothing []
>       [Instruction [Force] Always (To L.Spit)],

  Here are some forced actions that are less pleasant.

>   MakeLoc L.Neck
>       "You are at the bottom of the pit with a broken neck." Nothing []
>       [Instruction [Force] Always (To L.Limbo)],

>   MakeLoc L.Lose
>       "You didn't make it." Nothing [] [Instruction [Force] Always (To L.Limbo)],

  The rest are more-or-less routine, except for check -- which executes a
  conditional forced command.

>   MakeLoc L.Cant
>       "The dome is unclimbable." Nothing [] [Instruction [Force] Always (To L.Emist)],

>   MakeLoc L.Climb
>       "You clamber up the plant and scurry through the hole at the top."
>       Nothing
>       []
>       [Instruction [Force] Always (To L.Narrow)],

>   MakeLoc L.Check
>       "" Nothing []
>       [Instruction [Force] (Not Plant 2) (To L.Upnout),
>        Instruction [Force] Always (To L.Didit)],

>   MakeLoc L.Snaked
>       "You can't get by the snake." Nothing [] [Instruction [Force] Always (To L.Hmk)],

>   MakeLoc L.Thru
>       "You have crawled through a very low wide passage parallel to and north\n\
>       \of the Hall of Mists."
>       Nothing
>       []
>       [Instruction [Force] Always (To L.Wmist)],

>   MakeLoc L.Duck
>       (longDesc L.Thru) Nothing [] [Instruction [Force] Always (To L.Wfiss)],

>   MakeLoc L.Sewer
>       "The stream flows out through a pair of 1-foot-diameter sewer pipes.\n\
>       \It would be advisable to use the exit."
>       Nothing
>       []
>       [Instruction [Force] Always (To L.House)],

>   MakeLoc L.Upnout
>       "There is nothing here to climb.  Use \"up\" or \"out\" to leave the pit."
>       Nothing
>       []
>       [Instruction [Force] Always (To L.Wpit)],

>   MakeLoc L.Didit
>       "You have climbed up the plant and out of the pit."
>       Nothing
>       []
>       [Instruction [Force] Always (To L.W2pit)]
>  ]

>       where
>         longDesc x = findLongDesc x travelsList
>         shortDesc x = findShortDesc x travelsList
>         findLongDesc x ((MakeLoc y d _ _ _):xs) | x == y = d
>                                             | otherwise = findLongDesc x xs
>         findShortDesc x ((MakeLoc y _ d _ _):xs) | x == y = d
>                                                  | otherwise = findShortDesc x xs

  The table of instructions ends here; the remaining "locations" Ppass,
  pdrop, and troll are special.

page---------------- 42
