Instructions
------------

> module Instructions where

> import           Data.Map ((!))

> import           Actions (Action(Eat),defaultMsg)
> import           Motions hiding (Dark,Cave)
> import qualified Motions as M (Motion(Dark,Cave))
> import           Locations hiding (Crawl,Climb,Cross,Road,Hill,
>                                    Woods,Valley,House,Slab,Secret,
>                                    Bedquilt,Giant,Oriental,Shell,Debris,
>                                    View,Fork,Slit,Crack,Cobbles,Low,Y2,Witt,Bird)
> import qualified Locations as L (Location(Crawl,Climb,Cross,Road,Hill,
>                                  Woods,Valley,House,Slab,Secret,
>                                  Bedquilt,Giant,Oriental,Shell,Debris,
>                                  View,Fork,Slit,Crack,Cobbles,Low,Y2,Witt,Bird))
> import           Objects hiding (Nothing,Oil,Troll,Mirror,Pony,Snake,Bird)
> import qualified Objects as O (Object(Nothing,Snake,Troll))

> data Cond = Always
>           | NotDwarf
>           | Percent Int
>           | Holds Object
>           | Sees Object
>           | Not Object Int


> data Description = MakeLoc L.Location
>                            String
>                            (Maybe String)
>                            [Flag]
>                            [Instruction]

> data What = To Location | Sayit String

> data Instruction = MakeInst [Motion]
>                             Cond
>                             What


> data Flag = Lighted    -- a location that isn't dark
>           | Oil        -- presence of oil
>           | Liquid     -- presence of a liquid (oil or water)
>           | Cave       -- hint about trying to get in the cave
>           | Bird       -- hint about catching the bird
>           | Snake      -- hint about dealing with the snake
>           | Twist      -- hint about being lost in a maze
>           | Dark       -- hint about the dark room
>           | Witt       -- hint about Witt's End
>           deriving Eq

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

> instructions :: [Description]
> instructions = [
>   MakeLoc L.Road
>       "You are standing at the end of a road before a small brick building.\n\
>       \Around you is a forest.  A small stream flows out of the building and\n\
>       \down a gully."
>       (Just "You're at end of road again.")
>       [Lighted,Liquid]
>       [MakeInst [W,U,Road] Always (To L.Hill),
>        MakeInst [E,In,House,Enter] Always (To L.House),
>        MakeInst [S,D,Gully,Stream,Downstream] Always (To L.Valley),
>        MakeInst [N,Woods] Always (To Forest),
>        MakeInst [Depression] Always (To Outside)],

  Nothing up the hill, but a good explorer has to try anyway.

>   MakeLoc L.Hill
>       "You have walked up a hill, still in the forest.  The road slopes back\n\
>        \down the other side of the hill.  There is a building in the distance."
>       (Just "You're at hill in road.")
>       [Lighted]
>       [MakeInst [Road,House,Forward,E,D] Always (To L.Road),
>        MakeInst [Woods,N,S] Always (To Forest)],

  The house initially contains several objects: keys, food, a bottle, and a
  lantern. We'll put them in there later.

  Two magic words are understood in this house, for spelunkers who have been
  there and done that.

>   MakeLoc L.House
>       "You are inside a building, a well house for a large spring."
>       (Just "You're inside building.")
>       [Lighted,Liquid]
>       [MakeInst [Enter,Out,Outdoors,W] Always (To L.Road),
>        MakeInst [XYZZY] Always (To L.Debris),
>        MakeInst [Plugh] Always (To L.Y2),
>        MakeInst [Downstream,Stream] Always (To Sewer)],

  A foolish consistency is the hobgoblin of little minds. (Emerson)

>   MakeLoc L.Valley
>       "You are in a valley in the forest beside a stream tumbling along a\n\
>       \rocky bed."
>       (Just "You're in valley.")
>       [Lighted,Liquid]
>       [MakeInst [Upstream,House,N] Always (To L.Road),
>        MakeInst [Woods,E,W,U] Always (To Forest),
>        MakeInst [Downstream,S,D] Always (To L.Slit),
>        MakeInst [Depression] Always (To Outside)],

  The instructions here keep you in the forest with probability 50%,
  otherwise they take you to the woods . This gives the illusion that we
  maintain more state information about you than we really do.

>   MakeLoc Forest
>       "You are in open forest, with a deep valley to one side."
>       (Just "You're in forest.")
>       [Lighted]
>       [MakeInst [Valley,E,D] Always (To L.Valley),
>        MakeInst [Woods,Forward,N] (Percent 50) (To Forest),
>        MakeInst [Woods] Always (To L.Woods),
>        MakeInst [W,S] Always (To Forest)],

>   MakeLoc L.Woods
>       "You are in open forest near both a valley and a road."
>       Nothing
>       [Lighted]
>       [MakeInst [Road,N] Always (To L.Road),
>        MakeInst [Valley,E,W,D] Always (To L.Valley),
>        MakeInst [Woods,S] Always (To Forest)],

  You're getting closer. (But the program has forgotten that `Depression`
  leads outside; it knew this when you were at the road or the valley .)

>   MakeLoc L.Slit
>       "At your feet all the water of the stream splashes into a 2-inch slit\n\
>        \in the rock.  Downstream the streambed is bare rock."
>       (Just "You're at slit in streambed.")
>       [Lighted,Liquid]
>       [MakeInst [House] Always (To L.Road),
>        MakeInst [Upstream,N] Always (To L.Valley),
>        MakeInst [Woods,E,W] Always (To Forest),
>        MakeInst [Downstream,Rock,Bed,S] Always (To Outside),
>        MakeInst [Slit,Stream,D] Always (Sayit slitRmk)],

  We'll see later that the Grate will change from state 0 to state 1 if
  you unlock it. So let's hope you have the Keys.

>   MakeLoc Outside
>       "You are in a 20-foot depression floored with bare dirt.  Set into the\n\
>        \dirt is a strong steel grate mounted in concrete.  A dry streambed\n\
>        \leads into the depression."
>       (Just "You're outside grate.")
>       [Lighted,Cave]
>       [MakeInst [Woods,E,W,S] Always (To Forest),
>        MakeInst [House] Always (To L.Road),
>        MakeInst [Upstream,Gully,N] Always (To L.Slit),
>        MakeInst [Enter,In,D] (Not Grate 0) (To Inside), -- twice enter in knuth code
>        MakeInst [Enter] Always (Sayit grateRmk)],

  If you've come this far, you're probably hooked, although your adventure
  has barely begun.

>   MakeLoc Inside
>       "You are in a small chamber beneath a 3x3 steel grate to the surface.\n\
>       \A low crawl over cobbles leads inwards to the west."
>       (Just "You're below the grate.")
>       [Lighted]
>       [MakeInst [Out,U] (Not Grate 0) (To Outside), -- twice out in knuth code
>        MakeInst [Out] Always (Sayit grateRmk), -- ??
>        MakeInst [Crawl,Cobbles,In,W] Always (To L.Cobbles),
>        MakeInst [Pit] Always (To Spit),
>        MakeInst [Debris] Always (To L.Debris)],

  Go West, young man. (If you've got a lamp.)

>   MakeLoc L.Cobbles
>       "You are crawling over cobbles in a low passage.  There is a dim light\n\
>       \at the east end of the passage."
>       (Just "You're in cobble crawl.")
>       [Lighted]
>       [MakeInst [Out,Surface,Nowhere,E] Always (To Inside),
>        MakeInst [In,M.Dark,W,Debris] Always (To L.Debris),
>        MakeInst [Pit] Always (To Spit)],

>   MakeLoc L.Debris
>       "You are in a debris room filled with stuff washed in from the surface.\n\
>       \A low wide passage with cobbles becomes plugged with mud and debris\n\
>       \here, but an awkward canyon leads upward and west.  A note on the wall\n\
>       \says \"MAGIC WORD XYZZY\"."
>       (Just "You're in debris room.")
>       []

>       [MakeInst [Depression] (Not Grate 0) (To Outside), -- ??
>        MakeInst [Entrance] Always (To Inside),
>        MakeInst [Crawl,Cobbles,Passage,Low,E] Always (To L.Cobbles),
>        MakeInst [Canyon,In,U,W] Always (To Awk),
>        MakeInst [XYZZY] Always (To L.House),
>        MakeInst [Pit] Always (To Spit)],

>   MakeLoc Awk
>       "You are in an awkward sloping east/west canyon."  Nothing  [] -- ??
>       [MakeInst [Depression] (Not Grate 0) (To Outside), -- ??
>        MakeInst [Entrance] Always (To Inside),
>        MakeInst [D,E,Debris] Always (To L.Debris),
>        MakeInst [In,U,W] Always (To L.Bird),
>        MakeInst [Pit] Always (To Spit)],

>   MakeLoc L.Bird
>       "You are in a splendid chamber thirty feet high.  The walls are frozen\n\
>       \rivers of orange stone.  An awkward canyon and a good passage exit\n\
>       \from east and west sides of the chamber."
>       (Just "You're in bird chamber.")
>       [Bird]
>       [MakeInst [Depression] (Not Grate 0) (To Outside),
>        MakeInst [Entrance] Always (To Inside),
>        MakeInst [Debris] Always (To L.Debris),
>        MakeInst [Canyon,E] Always (To Awk),
>        MakeInst [Passage,Pit,W] Always (To Spit)], -- check

>   MakeLoc Spit
>       "At your feet is a small pit breathing traces of white mist.  An east\n\
>       \passage ends here except for a small crack leading on."
>       (Just "You're at top of small pit.")
>       []
>       [MakeInst [Depression] (Not Grate 0) (To Outside),
>        MakeInst [Entrance] Always (To Inside),
>        MakeInst [Debris] Always (To L.Debris),
>        MakeInst [Passage,E] Always (To L.Bird),
>        MakeInst [D,Pit,Steps] (Holds Gold) (To Neck),

  good thing you weren't loaded down with Gold

>        MakeInst [D] Always (To Emist),
>        MakeInst [Crack,W] Always (To L.Crack)],


  Welcome to the main caverns and a deeper level of adventures.

>   MakeLoc Emist
>       "You are at one end of a vast hall stretching forward out of sight to\n\
>       \the west.  There are openings to either side.  Nearby, a wide stone\n\
>       \staircase leads downward.  The hall is filled with wisps of white mist\n\
>       \swaying to and fro almost as if alive.  A cold wind blows up the\n\
>       \staircase.  There is a passage at the top of a dome behind you."
>       (Just "You're in Hall of Mists.")
>       []
>       [MakeInst [L,S] Always (To Nugget),
>        MakeInst [Forward,Hall,W] Always (To Efiss),
>        MakeInst [Stairs,D,N] Always (To Hmk),
>        MakeInst [U,Pit,Steps,Dome,Passage,E] (Holds Gold) (To Cant),
>        MakeInst [U] Always (To Spit),
>        MakeInst [Y2] Always (To Jumble)],

  To the left or south of the misty threshold, you might spot the first treasure.

>   MakeLoc Nugget
>       "This is a low room with a crude note on the wall.  The note says,\n\
>       \ \"You won't get it up the steps\"."
>       (Just "You're in nugget of gold room.")
>       []
>       [MakeInst [Hall,Out,N] Always (To Emist)],

  Unless you take a circuitous route to the other side of the Hall of
  Mists, via the Hall of the Mountain King, you should make the CRYSTAL
  bridge appear (by getting it into state 1).

>   MakeLoc Efiss
>       "You are on the east bank of a fissure slicing clear across the hall.\n\
>       \The mist is quite thick here, and the fissure is too wide to jump."
>       (Just "You're on east bank of fissure.")
>       []
>       [MakeInst [Hall,E] Always (To Emist),
>        MakeInst [Jump] (Not Crystal 0) (Sayit bridgeRmk),
>        MakeInst [Forward] (Not Crystal 1) (To Lose),
>        MakeInst [Over,Across,W,Cross] (Not Crystal 1)
>                                   (Sayit "There is no way across the fissure."),
>        MakeInst [Over] Always (To Wfiss)],

>   MakeLoc Wfiss
>       "You are on the west side of the fissure in the Hall of Mists."
>       Nothing
>       []
>       [MakeInst [Jump] (Not Crystal 0) (Sayit bridgeRmk),
>        MakeInst [Forward] (Not Crystal 1) (To Lose),
>        -- CHECK FOR CORRETNESS OF NEXT
>        MakeInst [Over,Across,E,Cross] (Not Crystal 1)
>                  (Sayit "There is no way across the fissure."),
>        MakeInst [Over] Always (To Efiss),
>        MakeInst [N] Always (To Thru),
>        MakeInst [W] Always (To Wmist)],

  What you see here isn't exactly what you get; N takes you east and S sucks
  you in to an amazing maze.

>   MakeLoc Wmist
>       "You are at the west end of the Hall of Mists.  A low wide crawl\n\
>       \continues west and another goes north.  To the south is a little\n\
>       \passage 6 feet off the floor."
>       (Just "You're at west end of Hall of Mists.")
>       []
>       [MakeInst [S,U,Passage,Climb] Always (To Like1),
>        MakeInst [E] Always (To Wfiss),
>        MakeInst [N] Always (To Duck),
>        MakeInst [W,Crawl] Always (To Elong)],

  The twisty little passages of this maze are said to be all alike, but they
  respond differently to  different motions. For example, you  can go north,
  east, south, or west from Like1,  but you can't go north from Like2 .  In
  that way  you can psych out the  whole maze of 14  similar locations. (And
  eventually  you will  want to  know every  place where  treasure  might be
  hidden.) The only exits are to wmist and brink .

>   MakeLoc Like1
>       allAlike Nothing [Twist]
>       [MakeInst [U] Always (To Wmist),
>        MakeInst [N] Always (To Like1),
>        MakeInst [E] Always (To Like2),
>        MakeInst [S] Always (To Like4),
>        MakeInst [W] Always (To Like11)],

>   MakeLoc Like2
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like1),
>        MakeInst [S] Always (To Like3),
>        MakeInst [E] Always (To Like4)],

>   MakeLoc Like3
>       allAlike Nothing [Twist]
>       [MakeInst [E] Always (To Like2),
>        MakeInst [D] Always (To Dead5),
>        MakeInst [S] Always (To Like6),
>        MakeInst [N] Always (To Dead9)],

>   MakeLoc Like4
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like1),
>        MakeInst [N] Always (To Like2),
>        MakeInst [E] Always (To Dead3),
>        MakeInst [S] Always (To Dead4),
>        MakeInst [U,D] Always (To Like14)],

>   MakeLoc Like5
>       allAlike Nothing [Twist]
>       [MakeInst [E] Always (To Like6),
>        MakeInst [W] Always (To Like7)],

>   MakeLoc Like6
>       allAlike Nothing [Twist]
>       [MakeInst [E] Always (To Like3),
>        MakeInst [W] Always (To Like5),
>        MakeInst [D] Always (To Like7),
>        MakeInst [S] Always (To Like8)],

>   MakeLoc Like7
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like5),
>        MakeInst [U] Always (To Like6),
>        MakeInst [E] Always (To Like8),
>        MakeInst [S] Always (To Like9)],

>   MakeLoc Like8
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like6),
>        MakeInst [E] Always (To Like7),
>        MakeInst [S] Always (To Like8),
>        MakeInst [U] Always (To Like9),
>        MakeInst [N] Always (To Like10),
>        MakeInst [D] Always (To Dead11)],

>   MakeLoc Like9
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like7),
>        MakeInst [N] Always (To Like8),
>        MakeInst [S] Always (To Dead6)],

>    MakeLoc Like10
>       allAlike Nothing [Twist]
>       [MakeInst [W] Always (To Like8),
>        MakeInst [N] Always (To Like10),
>        MakeInst [D] Always (To Dead7),
>        MakeInst [E] Always (To Brink)],

>   MakeLoc Like11
>       allAlike Nothing [Twist]
>       [MakeInst [N] Always (To Like1),
>        MakeInst [W,S] Always (To Like11),
>        MakeInst [E] Always (To Dead1)],

>   MakeLoc Like12
>       allAlike Nothing [Twist]
>       [MakeInst [S] Always (To Brink),
>        MakeInst [E] Always (To Like13),
>        MakeInst [W] Always (To Dead10)],

>   MakeLoc Like13
>       allAlike Nothing [Twist]
>       [MakeInst [N] Always (To Brink),
>        MakeInst [W] Always (To Like12),
>        MakeInst [NW] Always (To Dead2)],

>   MakeLoc Like14
>       allAlike Nothing [Twist] [MakeInst [U,D] Always (To Like4)],

>   MakeLoc Brink
>       "You are on the brink of a thirty-foot pit with a massive orange column\n\
>       \down one wall.  You could climb down here but you could not get back\n\
>       \up.  The maze continues at this level."
>       (Just "You're at brink of pit.")
>       []
>       [MakeInst [D,Climb] Always (To L.Bird),
>        MakeInst [W] Always (To Like10),
>        MakeInst [S] Always (To Dead8),
>        MakeInst [N] Always (To Like12),
>        MakeInst [E] Always (To Like13)],

  Crawling west from wmist instead of south, you encounter this.

>   MakeLoc Elong
>       "You are at the east end of a very long hall apparently without side\n\
>       \chambers.  To the east a low wide crawl slants up.  To the north a\n\
>       \round two-foot hole slants down."
>       (Just "You're at east end of long hall.")
>       []
>       [MakeInst [E,U,Crawl] Always (To Wmist),
>        MakeInst [W] Always (To Wlong),
>        MakeInst [N,D,Hole] Always (To L.Cross)],

>   MakeLoc Wlong
>       "You are at the west end of a very long featureless hall.  The hall\n\
>       \joins up with a narrow north/south passage."
>       (Just "You're at west end of long hall.")
>       []
>       [MakeInst [E] Always (To Elong),
>        MakeInst [N] Always (To L.Cross),
>        MakeInst [S] NotDwarf (To Diff0)],

  Recall that the last instruction above means, "Dwarves not
  permitted." It keeps them out of the following maze, which is based on an
  11 × 11 latin square. (Each of the eleven locations leads to each of the
  others under the ten motions N, S, E, W, NE, SE, NW, SW, U, D — except
  that diff0 goes down to the entrance location wlong instead of to diff10,
  and diff10 goes south to the dead-end location pony instead of to diff0.
  Furthermore, each location is accessible from all ten possible
  directions.)  Incidentally, if you ever get into a "little twisting maze
  of passages," you're really lost.

>   MakeLoc Diff0
>       "You are in a maze of twisty little passages, all different."
>       Nothing
>       []
>       [MakeInst [S] Always (To Diff1),
>        MakeInst [SW] Always (To Diff2),
>        MakeInst [NE] Always (To Diff3),
>        MakeInst [SE] Always (To Diff4),
>        MakeInst [U] Always (To Diff5),
>        MakeInst [NW] Always (To Diff6),
>        MakeInst [E] Always (To Diff7),
>        MakeInst [W] Always (To Diff8),
>        MakeInst [N] Always (To Diff9),
>        MakeInst [D] Always (To Wlong)],

>   MakeLoc Diff1
>       "You are in a maze of twisting little passages, all different."
>       Nothing
>       []
>       [MakeInst [W] Always (To Diff0),
>        MakeInst [SE] Always (To Diff2),
>        MakeInst [NW] Always (To Diff3),
>        MakeInst [SW] Always (To Diff4),
>        MakeInst [NE] Always (To Diff5),
>        MakeInst [U] Always (To Diff6),
>        MakeInst [D] Always (To Diff7),
>        MakeInst [N] Always (To Diff8),
>        MakeInst [S] Always (To Diff9),
>        MakeInst [E] Always (To Diff10)],

>   MakeLoc Diff2
>       "You are in a little maze of twisty passages, all different."
>       Nothing
>       []
>       [MakeInst [NW] Always (To Diff0),
>        MakeInst [U] Always (To Diff1),
>        MakeInst [N] Always (To Diff3),
>        MakeInst [S] Always (To Diff4),
>        MakeInst [W] Always (To Diff5),
>        MakeInst [SW] Always (To Diff6),
>        MakeInst [NE] Always (To Diff7),
>        MakeInst [E] Always (To Diff8),
>        MakeInst [D] Always (To Diff9),
>        MakeInst [SE] Always (To Diff10)],

>   MakeLoc Diff3
>       "You are in a twisting maze of little passages, all different."
>       Nothing
>       []
>       [MakeInst [U] Always (To Diff0),
>        MakeInst [D] Always (To Diff1),
>        MakeInst [W] Always (To Diff2),
>        MakeInst [NE] Always (To Diff4),
>        MakeInst [SW] Always (To Diff5),
>        MakeInst [E] Always (To Diff6),
>        MakeInst [N] Always (To Diff7),
>        MakeInst [NW] Always (To Diff8),
>        MakeInst [SE] Always (To Diff9),
>        MakeInst [S] Always (To Diff10)],

>   MakeLoc Diff4
>       "You are in a twisting little maze of passages, all different."
>       Nothing
>       []
>       [MakeInst [NE] Always (To Diff0),
>        MakeInst [N] Always (To Diff1),
>        MakeInst [NW] Always (To Diff2),
>        MakeInst [SE] Always (To Diff3),
>        MakeInst [E] Always (To Diff5),
>        MakeInst [D] Always (To Diff6),
>        MakeInst [S] Always (To Diff7),
>        MakeInst [U] Always (To Diff8),
>        MakeInst [W] Always (To Diff9),
>        MakeInst [SW] Always (To Diff10)],

>   MakeLoc Diff5
>       "You are in a twisty little maze of passages, all different."
>       Nothing
>       []
>       [MakeInst [N] Always (To Diff0),
>        MakeInst [SE] Always (To Diff1),
>        MakeInst [D] Always (To Diff2),
>        MakeInst [S] Always (To Diff3),
>        MakeInst [E] Always (To Diff4),
>        MakeInst [W] Always (To Diff6),
>        MakeInst [SW] Always (To Diff7),
>        MakeInst [NE] Always (To Diff8),
>        MakeInst [NW] Always (To Diff9),
>        MakeInst [U] Always (To Diff10)],

>   MakeLoc Diff6
>       "You are in a twisty maze of little passages, all different."
>       Nothing
>       []
>       [MakeInst [E] Always (To Diff0),
>        MakeInst [W] Always (To Diff1),
>        MakeInst [U] Always (To Diff2),
>        MakeInst [SW] Always (To Diff3),
>        MakeInst [D] Always (To Diff4),
>        MakeInst [S] Always (To Diff5),
>        MakeInst [NW] Always (To Diff7),
>        MakeInst [SE] Always (To Diff8),
>        MakeInst [NE] Always (To Diff9),
>        MakeInst [N] Always (To Diff10)],

>   MakeLoc Diff7
>       "You are in a little twisty maze of passages, all different."
>       Nothing
>       []
>       [MakeInst [SE] Always (To Diff0),
>        MakeInst [NE] Always (To Diff1),
>        MakeInst [S] Always (To Diff2),
>        MakeInst [D] Always (To Diff3),
>        MakeInst [U] Always (To Diff4),
>        MakeInst [NW] Always (To Diff5),
>        MakeInst [N] Always (To Diff6),
>        MakeInst [SW] Always (To Diff8),
>        MakeInst [E] Always (To Diff9),
>        MakeInst [W] Always (To Diff10)],

>   MakeLoc Diff8
>       "You are in a maze of little twisting passages, all different."
>       Nothing
>       []
>       [MakeInst [D] Always (To Diff0),
>        MakeInst [E] Always (To Diff1),
>        MakeInst [NE] Always (To Diff2),
>        MakeInst [U] Always (To Diff3),
>        MakeInst [W] Always (To Diff4),
>        MakeInst [N] Always (To Diff5),
>        MakeInst [S] Always (To Diff6),
>        MakeInst [SE] Always (To Diff7),
>        MakeInst [SW] Always (To Diff9),
>        MakeInst [NW] Always (To Diff10)],

>   MakeLoc Diff9
>       "You are in a maze of little twisty passages, all different."
>       Nothing
>       []
>       [MakeInst [SW] Always (To Diff0),
>        MakeInst [NW] Always (To Diff1),
>        MakeInst [E] Always (To Diff2),
>        MakeInst [W] Always (To Diff3),
>        MakeInst [N] Always (To Diff4),
>        MakeInst [D] Always (To Diff5),
>        MakeInst [SE] Always (To Diff6),
>        MakeInst [U] Always (To Diff7),
>        MakeInst [S] Always (To Diff8),
>        MakeInst [NE] Always (To Diff10)],

>   MakeLoc Diff10
>       "You are in a little maze of twisting passages, all different."
>       Nothing
>       []
>       [MakeInst [SW] Always (To Diff1),
>        MakeInst [N] Always (To Diff2),
>        MakeInst [E] Always (To Diff3),
>        MakeInst [NW] Always (To Diff4),
>        MakeInst [SE] Always (To Diff5),
>        MakeInst [NE] Always (To Diff6),
>        MakeInst [W] Always (To Diff7),
>        MakeInst [D] Always (To Diff8),
>        MakeInst [U] Always (To Diff9),
>        MakeInst [S] Always (To Pony)],
>
>   MakeLoc Pony
>       deadEnd Nothing [] [MakeInst [N,Out] Always (To Diff10)],

  Going north of the long hall, we come to the vicinity of another large
  room, with royal treasures nearby.  (You probably first reached this part
  of the cavern from the east, via the Hall of Mists.) Unfortunately, a
  vicious snake is here too; the conditional instructions for getting past
  the snake are worthy of study.

>   MakeLoc L.Cross
>       "You are at a crossover of a high N/S passage and a low E/W one."
>       Nothing []
>       [MakeInst [W] Always (To Elong),
>        MakeInst [N] Always (To Dead0),
>        MakeInst [E] Always (To West),
>        MakeInst [S] Always (To Wlong)],

>   MakeLoc Hmk
>       "You are in the Hall of the Mountain King  with passages off in all\n\
>       \directions."
>       (Just "You're in Hall of Mt King.")
>       [Snake]
>       [MakeInst [Stairs,U,E] Always (To Emist),
>        MakeInst [N,L] (Not O.Snake 0) (To NS),
>        MakeInst [S,R] (Not O.Snake 0) (To South),
>        MakeInst [W,Forward] (Not O.Snake 0) (To West),
>        MakeInst [N] Always (To Snaked),
>        MakeInst [SW] (Percent 35) (To L.Secret),
>        MakeInst [SW] (Sees O.Snake) (To Snaked),
>        MakeInst [Secret] Always (To L.Secret)],

>   MakeLoc West
>       "You are in the west side chamber of the Hall of the Mountain King.\n\
>       \A passage continues west and up here."
>       (Just "You're in west side chamber.")
>       []
>       [MakeInst [Hall,Out,E] Always (To Hmk),
>        MakeInst [W,U] Always (To L.Cross)],

>   MakeLoc South
>       "You are in the south side chamber." Nothing []
>       [MakeInst [Hall,Out,N] Always (To Hmk)],

  North of the mountain king's domain is a curious shuttle station called
  Y2, with magic connections to two other places.  (Real-world cave maps
  often use the symbol Y to stand for an entrance, and Y2 for a secondary
  entrance.

>   MakeLoc NS
>       "You are in a low N/S passage at a hole in the floor.  The hole goes\n\
>       \down to an E/W passage."
>       (Just "You're in N/S passage.")
>      []
>       [MakeInst [Hall,Out,S] Always (To Hmk),
>        MakeInst [N,Y2] Always (To L.Y2),
>        MakeInst [D,Hole] Always (To Dirty)],

>   MakeLoc L.Y2
>       "You are in a large room, with a passage to the south, a passage to the\n\
>       \west, and a wall of broken rock to the east.  There is a large \"Y2\" on\n\
>       \a rock in the room's center."
>       (Just "You're at \"Y2\".")
>      []
>       [MakeInst [Plugh] Always (To L.House),
>        MakeInst [S] Always (To NS),
>        MakeInst [E,Wall,Broken] Always (To Jumble),
>        MakeInst [W] Always (To Windoe),
>        MakeInst [Plover] (Holds Emerald) (To Pdrop),
>        MakeInst [Plover] Always (To Proom)],

>   MakeLoc Jumble
>       "You are in a jumble of rock, with cracks everywhere."  Nothing  []
>       [MakeInst [D,Y2] Always (To L.Y2),
>        MakeInst [U] Always (To Emist)],

>   MakeLoc Windoe
>       "You're at a low window overlooking a huge pit, which extends up out of\n\
>       \sight.  A floor is indistinctly visible over 50 feet below.  Traces of\n\
>       \white mist cover the floor of the pit, becoming thicker to the right.\n\
>       \Marks in the dust around the window would seem to indicate that\n\
>       \someone has been here recently.  Directly across the pit from you and\n\
>       \25 feet away there is a similar window looking into a lighted room.\n\
>       \A shadowy figure can be seen there peering back at you."
>       (Just "You're at window on pit.")
>       []
>       [MakeInst [E,Y2] Always (To L.Y2),
>        MakeInst [Jump] Always (To Neck)],

  Next let's consider the east/west passage below ns.

>   MakeLoc Dirty
>       "You are in a dirty broken passage.  To the east is a crawl.  To the\n\
>       \west is a large passage.  Above you is a hole to another passage."
>       (Just "You're in dirty passage.")
>       []
>       [MakeInst [E,Crawl] Always (To Clean),
>        MakeInst [U,Hole] Always (To NS),
>        MakeInst [W] Always (To Dusty),
>        MakeInst [Bedquilt] Always (To L.Bedquilt)],

>   MakeLoc Clean
>       "You are on the brink of a small clean climbable pit.  A crawl leads\n\
>        \west."
>       (Just "You're by a clean pit.")
>       []
>       [MakeInst [W,Crawl] Always (To Dirty),
>        MakeInst [D,Pit,Climb] Always (To Wet)],

>   MakeLoc Wet
>       "You are in the bottom of a small pit with a little stream, which\n\
>       \enters and exits through tiny slits."
>       (Just "You're in pit by stream.") [Liquid]
>       [MakeInst [Climb,U,Out] Always (To Clean),
>        MakeInst [Slit,Stream,D,Upstream,Downstream] Always (Sayit slitRmk)],

>   MakeLoc Dusty
>       "You are in a large room full of dusty rocks.  There is a big hole in\n\
>       \the floor.  There are cracks everywhere, and a passage leading east."
>       (Just "You're in dusty rock room.")
>       []
>       [MakeInst [E,Passage] Always (To Dirty),
>        MakeInst [D,Hole,Floor] Always (To Complex),
>        MakeInst [Bedquilt] Always (To L.Bedquilt)],

>   MakeLoc Complex
>       "You are at a complex junction.  A low hands-and-knees passage from the\n\
>       \north joins a higher crawl from the east to make a walking passage\n\
>       \going west.  There is also a large room above.  The air is damp here."
>       (Just "You're at complex junction.")
>       []
>       [MakeInst [U,Climb,Room] Always (To Dusty),
>        MakeInst [W,Bedquilt] Always (To L.Bedquilt),
>        MakeInst [N,Shell] Always (To L.Shell),
>        MakeInst [E] Always (To Ante)],

  A more-or-less self-contained cavelet can be found north of the complex
  passage. Its connections are more vertical than horizontal.

>   MakeLoc L.Shell
>       "You're in a large room carved out of sedimentary rock.  The floor\n\
>       \and walls are littered with bits of shells embedded in the stone.\n\
>       \A shallow passage proceeds downward  and a somewhat steeper one\n\
>       \leads up.  A low hands-and-knees passage enters from the south."
>       (Just "You're in Shell Room.")
>       []
>       [MakeInst [U,Hall] Always (To Arch),
>        MakeInst [D] Always (To Ragged),
>        MakeInst [S] (Holds Clam)
>           (Sayit "You can't fit this five-foot clam through that little passage!"),
>        MakeInst [S] (Holds Oyster)
>           (Sayit "You can't fit this five-foot oyster through that little passage!"),
>        MakeInst [S] Always (To Complex)],

>   MakeLoc Arch
>       "You are in an arched hall.  A coral passage once continued up and east\n\
>       \from here, but is now blocked by debris.  The air smells of sea water."
>       (Just "You're in arched hall.")
>       []
>       [MakeInst [D,Shell,Out] Always (To L.Shell)],

>   MakeLoc Ragged
>       "You are in a long sloping corridor with ragged sharp walls." Nothing []
>       [MakeInst [U,Shell] Always (To L.Shell),
>        MakeInst [D] Always (To Sac)],

>   MakeLoc Sac
>       "You are in a cul-de-sac about eight feet across." Nothing []
>       [MakeInst [U,Out] Always (To Ragged),
>        MakeInst [Shell] Always (To L.Shell)],

  A dangerous section lies east of the complex junction.

>   MakeLoc Ante
>       "You are in an anteroom leading to a large passage to the east.  Small\n\
>       \passages go west and up.  The remnants of recent digging are evident.\n\
>       \A sign in midair here says \"CAVE UNDER CONSTRUCTION BEYOND THIS POINT.\n\
>       \PROCEED AT OWN RISK. [WITT CONSTRUCTION COMPANY]\""
>       (Just "You're in anteroom.")
>       []
>       [MakeInst [U] Always (To Complex),
>        MakeInst [W] Always (To L.Bedquilt),
>        MakeInst [E] Always (To L.Witt)],

>   MakeLoc L.Witt
>       "You are at Witt's End.  Passages lead off in \"all\" directions."
>       (Just "You're at Witt's End.")
>       [Witt]
>       [MakeInst [E,N,S,NE,SE,SW,NW,U,D] (Percent 95) (Sayit loopRmk),
>        MakeInst [E] Always (To Ante),

  one chance in 20

>        MakeInst [W] Always (Sayit "You have crawled around in some little holes and found your way\n\
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
>       [MakeInst [E] Always (To Complex),
>        MakeInst [W] Always (To Cheese),
>        MakeInst [S] (Percent 80) (Sayit loopRmk),
>        MakeInst [Slab] Always (To L.Slab),
>        MakeInst [U] (Percent 80) (Sayit loopRmk),
>        MakeInst [U] (Percent 50) (To Abovep),
>        MakeInst [U] Always (To Dusty),
>        MakeInst [N] (Percent 60) (Sayit loopRmk),
>        MakeInst [N] (Percent 75) (To L.Low),
>        MakeInst [N] Always (To Sjunc),
>        MakeInst [D] (Percent 80) (Sayit loopRmk),
>        MakeInst [D] Always (To Ante)],

>   MakeLoc Cheese
>       "You are in a room whose walls resemble Swiss cheese.  Obvious passages\n\
>       \go west, east, NE, and NW. Part of the room is occupied by a large\n\
>       \bedrock block."
>       (Just "You're in Swiss cheese room.")
>       []
>       [MakeInst [NE] Always (To L.Bedquilt),
>        MakeInst [W] Always (To E2pit),
>        MakeInst [S] (Percent 80) (Sayit loopRmk),
>        MakeInst [Canyon] Always (To Tall),
>        MakeInst [E] Always (To Soft),
>        MakeInst [NW] (Percent 50) (Sayit loopRmk),
>        MakeInst [Oriental] Always (To L.Oriental)],

>   MakeLoc Soft
>       "You are in the Soft Room.  The walls are covered with heavy curtains,\n\
>       \the floor with a thick pile carpet.  Moss covers the ceiling."
>       (Just "You're in Soft Room.")
>       []
>       [MakeInst [W,Out] Always (To Cheese)],

  West of the quilt and the cheese is a room with two pits. Why would you
  want to descend into the pits? Keep playing and you'll find out.

>   MakeLoc E2pit
>       "You are at the east end of the Twopit Room.  The floor here is\n\
>       \littered with thin rock slabs, which make it easy to descend the pits.\n\
>       \There is a path here bypassing the pits to connect passages from east\n\
>       \and west.  There are holes all over, but the only big one is on the\n\
>       \wall directly over the west pit where you can't get to it."
>       (Just "You're at east end of Twopit Room.")
>       []
>       [MakeInst [E] Always (To Cheese),
>        MakeInst [W,Across] Always (To W2pit),
>        MakeInst [D,Pit] Always (To Epit)],

>   MakeLoc W2pit
>       "You are at the west end of the Twopit Room.  There is a large hole in\n\
>       \the wall above the pit at this end of the room."
>       (Just "You're at west end of Twopit Room.")
>       []
>       [MakeInst [E,Across] Always (To E2pit),
>        MakeInst [W,Slab] Always (To L.Slab),
>        MakeInst [D,Pit] Always (To Wpit),
>        MakeInst [Hole] Always (Sayit "It is too far up for you to reach.")],

>   MakeLoc Epit
>       "You are at the bottom of the eastern pit in the Twopit Room.  There is\n\
>       \a small pool of oil in one corner of the pit."
>       (Just "You're in east pit.")
>       [Liquid,Oil]
>       [MakeInst [U,Out] Always (To E2pit)],

>   MakeLoc Wpit
>       "You are at the bottom of the western pit in the Twopit Room.  There is\n\
>       \a large hole in the wall about 25 feet above you."
>       (Just "You're in west pit.")
>       []
>       [MakeInst [U,Out] Always (To W2pit),
>        MakeInst [Climb] (Not Plant 4) (To Check),
>        MakeInst [Climb] Always (To L.Climb)],

  Oho, you climbed the plant in the west pit! Now you're in another scenic
  area with rare treasures—if you can get through the door.

>   MakeLoc Narrow
>       "You are in a long, narrow corridor stretching out of sight to the\n\
>       \west.  At the eastern end is a hole through which you can see a\n\
>       \profusion of leaves."
>       (Just "You're in narrow corridor.")
>       []
>       [MakeInst [D,Climb,E] Always (To Wpit),
>        MakeInst [Jump] Always (To Neck),
>        MakeInst [W,Giant] Always (To L.Giant)],

>   MakeLoc L.Giant
>       "You are in the Giant Room.  The ceiling here is too high up for your\n\
>       \lamp to show it.  Cavernous passages lead east, north, and south.  On\n\
>       \the west wall is scrawled the inscription, \"FEE FIE FOE FOO\" [sic]."
>       (Just "You're in Giant Room.")
>       []
>       [MakeInst [S] Always (To Narrow),
>        MakeInst [E] Always (To Block),
>        MakeInst [N] Always (To Immense)],

>   MakeLoc Block
>       "The passage here is blocked by a recent cave-in." Nothing []
>       [MakeInst [S,Giant,Out] Always (To L.Giant)],

>   MakeLoc Immense
>       "You are at one end of an immense north/south passage." Nothing []
>       [MakeInst [S,Giant,Passage] Always (To L.Giant),
>        MakeInst [N,Enter,Cavern] (Not Door 0) (To Falls),
>        MakeInst [N] Always
>                     (Sayit "The door is extremely rusty and refuses to open.")],

>   MakeLoc Falls
>       "You are in a magnificent cavern with a rushing stream, which cascades\n\
>       \over a sparkling waterfall into a roaring whirlpool that disappears\n\
>       \through a hole in the floor.  Passages exit to the south and west."
>       (Just "You're in cavern with waterfall.")
>       [Liquid]
>       [MakeInst [S,Out] Always (To Immense),
>        MakeInst [Giant] Always (To L.Giant),
>        MakeInst [W] Always (To Steep)],

>   MakeLoc Steep
>       "You are at the top of a steep incline above a large room.  You could\n\
>       \climb down here, but you would not be able to climb up.  There is a\n\
>       \passage leading back to the north."
>       (Just "You're at steep incline above large room.")
>       []
>       [MakeInst [N,Cavern,Passage] Always (To Falls),
>        MakeInst [D,Climb] Always (To L.Low)],

  Meanwhile let's backtrack to another part of the cave possibly reachable
  from Bedquilt.

>   MakeLoc Abovep
>       "You are in a secret N/S canyon above a sizable passage." Nothing []
>       [MakeInst [N] Always (To Sjunc),
>        MakeInst [D,Passage] Always (To L.Bedquilt),
>        MakeInst [S] Always (To Tite)],

>   MakeLoc Sjunc
>       "You are in a secret canyon at a junction of three canyons, bearing\n\
>       \north, south, and SE. The north one is as tall as the other two\n\
>       \combined."
>       (Just "You're at junction of three secret canyons.")
>       []
>       [MakeInst [SE] Always (To L.Bedquilt),
>        MakeInst [S] Always (To Abovep),
>        MakeInst [N] Always (To Window)],

>   MakeLoc Tite
>       "A large stalactite extends from the roof and almost reaches the floor\n\
>       \below.  You could climb down it, and jump from it to the floor, but\n\
>       \having done so you would be unable to reach it to climb back up."
>       (Just "You're on top of stalactite.")
>       []
>       [MakeInst [N] Always (To Abovep),
>        MakeInst [D,Jump,Climb] (Percent 40) (To Like6),
>        MakeInst [D] (Percent 50) (To Like9),
>        MakeInst [D] Always (To Like4)],

  oh dear, you're in a random part of the maze

>   MakeLoc L.Low
>       "You are in a large low room.  Crawls lead north, SE, and SW."
>       Nothing
>       []
>       [MakeInst [Bedquilt] Always (To L.Bedquilt),
>        MakeInst [SW] Always (To Scorr),
>        MakeInst [N] Always (To L.Crawl),
>        MakeInst [SE,Oriental] Always (To L.Oriental)],

>   MakeLoc L.Crawl
>       "Dead end crawl." Nothing []
>       [MakeInst [S,Crawl,Out] Always (To L.Low)],

  The described view from the west window, window, is identical to the view
  from the east window, windoe, except for one word. What on earth do you
  see from those windows? (Don Woods has confided that the shadowy figure is
  actually your own reflection, because mirror lies between the two window
  rooms.  An intentional false clue.

>   MakeLoc Window
>       "You're at a low window overlooking a huge pit, which extends up out of\n\
>       \sight.  A floor is indistinctly visible over 50 feet below.  Traces of\n\
>       \white mist cover the floor of the pit, becoming thicker to the left.\n\
>       \Marks in the dust around the window would seem to indicate that\n\
>       \someone has been here recently.  Directly across the pit from you and\n\
>       \25 feet away there is a similar window looking into a lighted room.\n\
>       \A shadowy figure can be seen there peering back to you."
>       (shortDesc Windoe)
>       []
>       [MakeInst [W] Always (To Sjunc),
>       MakeInst [Jump] Always (To Neck)],

  More treasures await you via the low corridor.

>   MakeLoc L.Oriental
>       "This is the Oriental Room.  Ancient oriental cave drawings cover the\n\
>       \walls.  A gently sloping passage leads upward to the north, another\n\
>       \passage leads SE, and a hands-and-knees crawl leads west."
>       (Just "You're in Oriental Room.")
>       []
>       [MakeInst [SE] Always (To Cheese),
>        MakeInst [W,Crawl] Always (To L.Low),
>        MakeInst [U,N,Cavern] Always (To Misty)],

>   MakeLoc Misty
>       "You are following a wide path around the outer edge of a large cavern.\n\
>       \Far below, through a heavy white mist, strange splashing noises can be\n\
>       \heard.  The mist rises up through a fissure in the ceiling.  The path\n\
>       \exits to the south and west."
>       (Just "You're in misty cavern.")
>       []
>       [MakeInst [S,Oriental] Always (To L.Oriental),
>        MakeInst [W] Always (To Alcove)],

  One of the darkest secrets is hidden here. You will discover that you must
  take the emerald from the Plover Room to the alcove. But you don't learn
  the name of the Plover Room until the second time you've been there, since
  your first visit will be lampless until you know the secret.

>   MakeLoc Alcove
>       "You are in an alcove.  A small NW path seems to widen after a short\n\
>       \distance.  An extremely tight tunnel leads east.  It looks like a very\n\
>       \tight squeeze.  An eerie light can be seen at the other end."
>       (Just "You're in alcove.")
>       [Dark]
>       [MakeInst [NW,Cavern] Always (To Misty),
>        MakeInst [E,Passage] Always (To Ppass),
>        MakeInst [E] Always (To Proom)],

  never performed, but seen by 'go back'

>   MakeLoc Proom
>       "You're in a small chamber lit by an eerie green light.  An extremely\n\
>       \narrow tunnel exits to the west.  A dark corridor leads NE."
>       (Just "You're in Plover Room.")
>       [Dark]
>       [MakeInst [W,Passage,Out] Always (To Ppass),
>        MakeInst [W] Always (To Alcove),

  never performed, but seen by 'go back'

>        MakeInst [Plover] (Holds Emerald) (To Pdrop),
>        MakeInst [Plover] Always (To L.Y2),
>        MakeInst [NE,M.Dark] Always (To Droom)],

>   MakeLoc Droom
>       "You're in the Dark-Room.  A corridor leading south is the only exit."
>       (Just "You're in Dark-Room.")
>       [Dark]
>       [MakeInst [S,Plover,Out] Always (To Proom)],

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
>       [MakeInst [S] Always (To W2pit),
>        MakeInst [U,Climb] Always (To Abover),
>        MakeInst [N] Always (To L.Bedquilt)],

>   MakeLoc Abover
>       "You are in a secret N/S canyon above a large room." Nothing []
>       [MakeInst [D,Slab] Always (To L.Slab),
>        MakeInst [S] (Not Dragon 0) (To Scan2),
>        MakeInst [S] Always (To Scan1),
>        MakeInst [N] Always (To Mirror),
>        MakeInst [Reservoir] Always (To Res)],

>   MakeLoc Mirror
>       "You are in a north/south canyon about 25 feet across.  The floor is\n\
>       \covered by white mist seeping in from the north.  The walls extend\n\
>       \upward for well over 100 feet.  Suspended from some unseen point far\n\
>       \above you, an enormous two-sided mirror is hanging parallel to and\n\
>       \midway between the canyon walls. (The mirror is obviously provided\n\
>       \for the use of the dwarves, who as you know are extremely vain.)\n\
>       \A small window can be seen in either wall, some fifty feet up."
>       (Just "You're in mirror canyon.")
>       []
>       [MakeInst [S] Always (To Abover),
>        MakeInst [N,Reservoir] Always (To Res)],

>   MakeLoc Res
>       "You are at the edge of a large underground reservoir.  An opaque cloud\n\
>       \of white mist fills the room and rises rapidly upward.  The lake is\n\
>       \fed by a stream, which tumbles out of a hole in the wall about 10 feet\n\
>       \overhead and splashes noisily into the water somewhere within the\n\
>       \mist.  The only passage goes back toward the south."
>       (Just "You're at reservoir.")
>       [Liquid]
>       [MakeInst [S,Out] Always (To Mirror)],

  Four more secret canyons lead back to the Hall of the Mountain King. Three
  of them are actually the same, but the dragon blocks the connection
  between the northern passage (to abover ) and the eastern passage (to
  secret ). Once you've vanquished the dragon, scan2 takes the place of
  scan1 and scan3 .

>   MakeLoc Scan1
>       "You are in a secret canyon that exits to the north and east."
>       Nothing
>       []
>       [MakeInst [N,Out] Always (To Abover),
>        MakeInst [E,Forward] Always
>           (Sayit "The dragon looks rather nasty.  You'd best not try to get by.")],

>   MakeLoc Scan2
>       (longDesc Scan1) Nothing []
>       [MakeInst [N] Always (To Abover),
>        MakeInst [E] Always (To L.Secret)],

>   MakeLoc Scan3
>       (longDesc Scan1) Nothing []
>       [MakeInst [E,Out] Always (To L.Secret),
>        MakeInst [N,Forward] Always (Sayit  "The dragon looks rather nasty.  You'd best not try to get by.")],

>   MakeLoc L.Secret
>       "You are in a secret canyon, which here runs E/W. It crosses over a\n\
>       \very tight canyon 15 feet below.  If you go down you may not be able\n\
>       \to get back up."
>       (Just "You're in secret E/W canyon above tight canyon.")
>       []
>       [MakeInst [E] Always (To Hmk),
>        MakeInst [W] (Not Dragon 0) (To Scan2),
>        MakeInst [W] Always (To Scan3),
>        MakeInst [D] Always (To Wide)],

   Below secret there's another way to reach the cheese.

>   MakeLoc Wide
>       "You are at a wide place in a very tight N/S canyon." Nothing []
>       [MakeInst [S] Always (To Tight),
>        MakeInst [N] Always (To Tall)],

>   MakeLoc Tight
>       "The canyon here becomes too tight to go further south." Nothing []
>       [MakeInst [N] Always (To Wide)],

>   MakeLoc Tall
>       "You are in a tall E/W canyon.  A low tight crawl goes 3 feet north and\n\
>       \seems to open up."
>       (Just "You're in tall E/W canyon.")
>       []
>       [MakeInst [E] Always (To Wide),
>        MakeInst [W] Always (To Boulders),
>        MakeInst [N,Crawl] Always (To Cheese)],

>   MakeLoc Boulders
>       "The canyon runs into a mass of boulders --- dead end."
>       Nothing
>       []
>       [MakeInst [S] Always (To Tall)],

  If you aren't having fun yet, wait till you meet the troll. The only way
  to get here is to crawl southwest from the low room. And then you have a
  new problem to solve; we'll see later that the Troll and the BRIDGE are
  here.  (Don Woods got the idea for the mist-covered bridge after an early
  morning visit to Mount Diablo; see Steven Levy, Hackers (New York: (To Delta),
  1994), Chapter 7.)

>   MakeLoc Scorr
>       "You are in a long winding corridor sloping out of sight in both\n\
>       \directions."
>       (Just "You're in sloping corridor.")
>       []
>       [MakeInst [D] Always (To L.Low),
>        MakeInst [U] Always (To SWside)],

>   MakeLoc SWside
>       "You are on one side of a large, deep chasm.  A heavy white mist rising\n\
>       \up from below obscures all view of the far side.  A SW path leads away\n\
>       \from the chasm into a winding corridor."
>       (Just "You're on SW side of chasm.")
>       []
>       [MakeInst [SW] Always (To Scorr),
>        MakeInst [Over,Across,Cross,NE] (Sees O.Troll)
>                      (Sayit "The troll refuses to let you cross."),
>        MakeInst [Over] (Not O.Troll 0)
>                      (Sayit "There is no longer any way across the chasm."),
>        MakeInst [Over] Always (To Troll),
>        MakeInst [Jump] (Not O.Troll 0) (To Lose),
>        MakeInst [Jump] Always (Sayit bridgeRmk)], -- ??

  The only things not yet explored on this side of the troll bridge are a
  dozen dead ends. They appear at this place in the ordering of all
  locations because of the pirate logic explained later: The pirate will
  never go to locations ≥ dead3 .

 #define max pirate loc dead2

>   MakeLoc Dead0
>       deadEnd  Nothing [] [MakeInst [S,Out] Always (To L.Cross)],

>   MakeLoc Dead1
>       deadEnd  Nothing [Twist] [MakeInst [W,Out] Always (To Like11)],

>   MakeLoc Dead2
>       deadEnd  Nothing [] [MakeInst [SE] Always (To Like13)],

>   MakeLoc Dead3
>       deadEnd  Nothing [Twist] [MakeInst [W,Out] Always (To Like4)],

>   MakeLoc Dead4
>       deadEnd  Nothing [Twist] [MakeInst [E,Out] Always (To Like4)],

>   MakeLoc Dead5
>       deadEnd  Nothing [Twist] [MakeInst [U,Out] Always (To Like3)],

>   MakeLoc Dead6
>       deadEnd  Nothing [Twist] [MakeInst [W,Out] Always (To Like9)],

>   MakeLoc Dead7
>       deadEnd  Nothing [Twist] [MakeInst [U,Out] Always (To Like10)],

>   MakeLoc Dead8
>       deadEnd  Nothing [] [MakeInst [E,Out] Always (To Brink)],

>   MakeLoc Dead9
>       deadEnd  Nothing [Twist] [MakeInst [S,Out] Always (To Like3)],

>   MakeLoc Dead10
>       deadEnd  Nothing [Twist] [MakeInst [E,Out] Always (To Like12)],

>   MakeLoc Dead11
>       deadEnd  Nothing [Twist] [MakeInst [U,Out] Always (To Like8)],

  A whole nuther cave with nine sites and additional treasures is on tuther
  side of the troll bridge! This cave was inspired in part by
  J. R. R. Tolkien's stories.

>   MakeLoc Neside
>       "You are on the far side of the chasm.  A NE path leads away from the\n\
>       \chasm on this side."
>       (Just "You're on NE side of chasm.")
>       []
>       [MakeInst [NE] Always (To Corr),
>        MakeInst [Over,Across,Cross,SW] (Sees O.Troll) (Sayit "The troll refuses to let you cross."), -- Maybe wrong
>        MakeInst [Over] Always (To Troll),
>        MakeInst [Jump] Always (Sayit bridgeRmk),
>        MakeInst [Fork] Always (To L.Fork),
>        MakeInst [View] Always (To L.View),
>        MakeInst [Barren] Always (To Fbarr)],

>   MakeLoc Corr
>       "You're in a long east/west corridor.  A faint rumbling noise can be\n\
>       \heard in the distance."
>       (Just "You're in corridor.")
>       []
>       [MakeInst [W] Always (To Neside),
>        MakeInst [E,Fork] Always (To L.Fork),
>        MakeInst [View] Always (To L.View),
>        MakeInst [Barren] Always (To Fbarr)],

>   MakeLoc L.Fork
>       "The path forks here.  The left fork leads northeast.  A dull rumbling\n\
>       \seems to get louder in that direction.  The right fork leads southeast\n\
>       \down a gentle slope.  The main corridor enters from the west."
>       (Just "You're at fork in path.")
>       []
>       [MakeInst [W] Always (To Corr),
>        MakeInst [NE,L] Always (To Warm),
>        MakeInst [SE,R,D] Always (To Lime),
>        MakeInst [View] Always (To L.View),
>        MakeInst [Barren] Always (To Fbarr)],

>   MakeLoc Warm
>       "The walls are quite warm here.  From the north can be heard a steady\n\
>       \roar, so loud that the entire cave seems to be trembling.  Another\n\
>       \passage leads south, and a low crawl goes east."
>       (Just "You're at junction with warm walls.")
>       []
>       [MakeInst [S,Fork] Always (To L.Fork),
>        MakeInst [N,View] Always (To L.View),
>        MakeInst [E,Crawl] Always (To Chamber)],

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
>       [MakeInst [S,Passage,Out] Always (To Warm),
>        MakeInst [Fork] Always (To L.Fork),
>        MakeInst [D,Jump] Always (Sayit $ defaultMsg!Eat)],

>   MakeLoc Chamber
>       "You are in a small chamber filled with large boulders.  The walls are\n\
>       \very warm, causing the air in the room to be almost stifling from the\n\
>       \heat.  The only exit is a crawl heading west, through which a low\n\
>       \rumbling noise is coming."
>       (Just "You're in chamber of boulders.")
>       []
>       [MakeInst [W,Out,Crawl] Always (To Warm),
>        MakeInst [Fork] Always (To L.Fork),
>        MakeInst [View] Always (To L.View)],

>   MakeLoc Lime
>       "You are walking along a gently sloping north/south passage lined with\n\
>       \oddly shapped limestone formations."
>       (Just "You're in limestone passage.")
>       []
>       [MakeInst [N,U,Fork] Always (To L.Fork),
>        MakeInst [S,D,Barren] Always (To Fbarr),
>        MakeInst [View] Always (To L.View)],

>   MakeLoc Fbarr
>       "You are standing at the entrance to a large, barren room.  A sign\n\
>       \posted above the entrance reads: \"CAUTION! BEAR IN ROOM!\""
>       (Just "You're in front of barren room.")
>       []

  don't laugh too loud

>       [MakeInst [W,U] Always (To Lime),
>        MakeInst [Fork] Always (To L.Fork),
>        MakeInst [E,In,Barren,Enter] Always (To Barr),
>        MakeInst [View] Always (To L.View)],

>   MakeLoc Barr
>       "You are inside a barren room.  The center of the room is completely\n\
>       \empty except for some dust.  Marks in the dust lead away toward the\n\
>       \far end of the room.  The only exit is the way you came in."
>       (Just "You're in barren room.")
>       []
>       [MakeInst [W,Out] Always (To Fbarr),
>        MakeInst [Fork] Always (To L.Fork),
>        MakeInst [View] Always (To L.View)],

  The two storage locations are accessible only from each other, and they
  lead only to each other.

>   MakeLoc Neend
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
>       [MakeInst [SW] Always (To Swend)],

>   MakeLoc Swend
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
>       [MakeInst [NE] Always (To Neend),
>        MakeInst [D] Always (Sayit grateRmk)],

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
>       [MakeInst [Force] Always (To Spit)],

  Here are some forced actions that are less pleasant.

>   MakeLoc Neck
>       "You are at the bottom of the pit with a broken neck." Nothing []
>       [MakeInst [Force] Always (To Limbo)],

>   MakeLoc Lose
>       "You didn't make it." Nothing [] [MakeInst [Force] Always (To Limbo)],

  The rest are more-or-less routine, except for check -- which executes a
  conditional forced command.

>   MakeLoc Cant
>       "The dome is unclimbable." Nothing [] [MakeInst [Force] Always (To Emist)],

>   MakeLoc L.Climb
>       "You clamber up the plant and scurry through the hole at the top."
>       Nothing
>       []
>       [MakeInst [Force] Always (To Narrow)],

>   MakeLoc Check
>       "" Nothing []
>       [MakeInst [Force] (Not Plant 2) (To Upnout),
>        MakeInst [Force] Always (To Didit)],

>   MakeLoc Snaked
>       "You can't get by the snake." Nothing [] [MakeInst [Force] Always (To Hmk)],

>   MakeLoc Thru
>       "You have crawled through a very low wide passage parallel to and north\n\
>       \of the Hall of Mists."
>       Nothing
>       []
>       [MakeInst [Force] Always (To Wmist)],

>   MakeLoc Duck
>       (longDesc Thru) Nothing [] [MakeInst [Force] Always (To Wfiss)],

>   MakeLoc Sewer
>       "The stream flows out through a pair of 1-foot-diameter sewer pipes.\n\
>       \It would be advisable to use the exit."
>       Nothing
>       []
>       [MakeInst [Force] Always (To L.House)],

>   MakeLoc Upnout
>       "There is nothing here to climb.  Use \"up\" or \"out\" to leave the pit."
>       Nothing
>       []
>       [MakeInst [Force] Always (To Wpit)],

>   MakeLoc Didit
>       "You have climbed up the plant and out of the pit."
>       Nothing
>       []
>       [MakeInst [Force] Always (To W2pit)]
>  ]

>       where
>         longDesc x = findLongDesc x instructions
>         shortDesc x = findShortDesc x instructions
>         findLongDesc x ((MakeLoc y d _ _ _):xs) | x == y = d
>                                             | otherwise = findLongDesc x xs
>         findShortDesc x ((MakeLoc y _ d _ _):xs) | x == y = d
>                                                  | otherwise = findShortDesc x xs

  The table of instructions ends here; the remaining "locations" Ppass,
  pdrop, and troll are special.

page---------------- 42
