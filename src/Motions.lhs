Motions
-------

> module Motions (Motion(..),motions) where


> data Motion = N | S | E | W | NE | SE | NW | SW | U | D | L | R
>             | In | Out | Forward | Back
>             | Over | Across | Upstream | Downstream
>             | Enter | Crawl | Jump | Climb | Look | Cross
>             | Road | Hill | Woods | Valley | House | Gully | Stream
>             | Depression | Entrance | Cave
>             | Rock | Slab| Bed | Passage | Cavern | Canyon
>             | Awkward | Secret | Bedquilt | Reservoir
>             | Giant | Oriental | Shell | Barren | Broken | Debris
>             | View | Fork | Pit | Slit | Crack | Dome | Hole
>             | Wall | Hall | Room | Floor | Stairs | Steps
>             | Cobbles | Surface | Dark | Low | Outdoors
>             | Y2 | XYZZY | Plugh | Plover | Office | Nowhere
>             deriving (Show)


> data MotionVoc = MotionVoc [String] Motion
>                 deriving Show


> flatten = concatMap (\(MotionVoc strings motion) -> zip strings (repeat motion))

> motions :: [(String, Motion)]
> motions = flatten [
>     MotionVoc ["n","north"] N,
>     MotionVoc ["s","south"] S,
>     MotionVoc ["e","east"] E,
>     MotionVoc ["w","west"] W,
>     MotionVoc ["ne"] NE,
>     MotionVoc ["se"] SE,
>     MotionVoc ["nw"] NW,
>     MotionVoc ["sw"] SW,
>     MotionVoc ["ascen","above","u","up","upwar"] U,
>     MotionVoc ["desce","d","down","downw"] D,
>     MotionVoc ["left"] L,
>     MotionVoc ["right"] R,
>     MotionVoc ["in","insid","inwar"] In,
>     MotionVoc ["leave","exit","outsi","out"] Out,
>     MotionVoc ["onwar","conti","forwa"] Forward,
>     MotionVoc ["retre","retur","back"] Back,
>     MotionVoc ["over"] Over,
>     MotionVoc ["acros"] Across,
>     MotionVoc ["upstr"] Upstream,
>     MotionVoc ["downs"] Downstream,
>     MotionVoc ["enter"] Enter,
>     MotionVoc ["crawl"] Crawl,
>     MotionVoc ["jump"] Jump,
>     MotionVoc ["climb"] Climb,
>     MotionVoc ["descr","touch","exami","look"] Look,
>     MotionVoc ["cross"] Cross,
>     MotionVoc ["road"] Road,
>     MotionVoc ["hill"] Hill,
>     MotionVoc ["forest"] Woods,
>     MotionVoc ["valle"] Valley,
>     MotionVoc ["house","build"] House,
>     MotionVoc ["gully"] Gully,
>     MotionVoc ["strea"] Stream,
>     MotionVoc ["depre"] Depression,
>     MotionVoc ["entra"] Entrance,
>     MotionVoc ["cave"] Cave,
>     MotionVoc ["rock"] Rock,
>     MotionVoc ["slabr","slab"] Slab,
>     MotionVoc ["bed"] Bed,
>     MotionVoc ["tunne","passa"] Passage,
>     MotionVoc ["caver"] Cavern,
>     MotionVoc ["canyo"] Canyon,
>     MotionVoc ["awkwa"] Awkward,
>     MotionVoc ["secre"] Secret,
>     MotionVoc ["bedqu"] Bedquilt,
>     MotionVoc ["reser"] Reservoir,
>     MotionVoc ["giant"] Giant,
>     MotionVoc ["orien"] Oriental,
>     MotionVoc ["shell"] Shell,
>     MotionVoc ["barre"] Barren,
>     MotionVoc ["broke"] Broken,
>     MotionVoc ["debri"] Debris,
>     MotionVoc ["view"] View,
>     MotionVoc ["fork"] Fork,
>     MotionVoc ["pit"] Pit,
>     MotionVoc ["slit"] Slit,
>     MotionVoc ["crack"] Crack,
>     MotionVoc ["dome"] Dome,
>     MotionVoc ["hole"] Hole,
>     MotionVoc ["wall"] Wall,
>     MotionVoc ["hall"] Hall,
>     MotionVoc ["room"] Room,
>     MotionVoc ["floor"] Floor,
>     MotionVoc ["stair"] Stairs,
>     MotionVoc ["steps"] Steps,
>     MotionVoc ["cobbl"] Cobbles,
>     MotionVoc ["surfa"] Surface,
>     MotionVoc ["dark"] Dark,
>     MotionVoc ["low"] Low,
>     MotionVoc ["outdo"] Outdoors,
>     MotionVoc ["y2"] Y2,
>     MotionVoc ["xyzzy"] XYZZY,
>     MotionVoc ["plugh"] Plugh,
>     MotionVoc ["plove"] Plover,
>     MotionVoc ["offic","main"] Office,
>     MotionVoc ["nowhe","null"] Nowhere]
