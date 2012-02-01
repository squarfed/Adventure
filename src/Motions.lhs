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



> motions :: [(String, Motion)]
> motions = [("north", N),("n", N),
>            ("south", S),("s", S),
>            ("east", E),("e", E),
>            ("west", W),("w", W),
>            ("ne", NE),
>            ("se", SE),
>            ("nw", NW),
>            ("sw", SW),
>            ("upwar", U),("up", U),("u", U),("above", U),
>            ("ascen", U),
>            ("downw", D),("down", D),("d", D),("desce", D),
>            ("left", L),
>            ("right", R),
>            ("inwar", In),("insid", In),("in", In),
>            ("out", Out),("outsi", Out),
>            ("exit", Out),
>            ("leave", Out),
>            ("forwa", Forward),("conti", Forward),("onwar", Forward),
>            ("back", Back),("retur", Back),("retre", Back),
>            ("over", Over),
>            ("acros", Across),
>            ("upstr", Upstream),
>            ("downs", Downstream),
>            ("enter", Enter),
>            ("crawl", Crawl),
>            ("jump", Jump),
>            ("climb", Climb),
>            ("look", Look),("exami", Look),("touch", Look),
>            ("descr", Look),
>            ("cross", Cross),
>            ("road", Road),
>            ("hill", Hill),
>            ("forest", Woods),
>            ("valle", Valley),
>            ("build", House),("house", House),
>            ("gully", Gully),
>            ("strea", Stream),
>            ("depre", Depression),
>            ("entra", Entrance),
>            ("cave", Cave),
>            ("rock", Rock),
>            ("slab", Slab),("slabr", Slab),
>            ("bed", Bed),
>            ("passa", Passage),("tunne", Passage),
>            ("caver", Cavern),
>            ("canyo", Canyon),
>            ("awkwa", Awkward),
>            ("secre", Secret),
>            ("bedqu", Bedquilt),
>            ("reser", Reservoir),
>            ("giant", Giant),
>            ("orien", Oriental),
>            ("shell", Shell),
>            ("barre", Barren),
>            ("broke", Broken),
>            ("debri", Debris),
>            ("view", View),
>            ("fork", Fork),
>            ("pit", Pit),
>            ("slit", Slit),
>            ("crack", Crack),
>            ("dome", Dome),
>            ("hole", Hole),
>            ("wall", Wall),
>            ("hall", Hall),
>            ("room", Room),
>            ("floor", Floor),
>            ("stair", Stairs),
>            ("steps", Steps),
>            ("cobbl", Cobbles),
>            ("surfa", Surface),
>            ("dark", Dark),
>            ("low", Low),
>            ("outdo", Outdoors),
>            ("y2", Y2),
>            ("xyzzy", XYZZY),
>            ("plugh", Plugh),
>            ("plove", Plover),
>            ("main", Office), ("offic", Office),
>            ("null", Nowhere), ("nowhe", Nowhere)]
