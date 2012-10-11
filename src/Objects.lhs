Objects
=======

> module Objects (Object(..),objectsVoc) where

> data Object = Nothing | Keys | Lamp | Grate | Grate_
>             | Cage | Rod | Rod2 | Treads | Treads_
>             | Bird | Door | Pillow | Snake | Crystal
>             | Crystal_ | Tablet | Clam | Oyster
>             | Mag| Dwarf | Knife | Food | Bottle | Water | Oil
>             | Mirror| Mirror_ | Plant | Plant2 | Plant2_
>             | Stalactite| Shadow | Shadow_
>             | Axe | Art | Pirate | Dragon | Dragon_ | Bridge
>             | Bridge_ | Troll | Troll_ | Troll2 | Troll2_
>             | Bear | Message | Geyser | Pony | Batteries | Moss
>             | Gold | Diamonds | Silver | Jewels | Coins
>             | Chest | Eggs | Trident | Vase
>             | Emerald | Pyramid | Pearl | Rug | Rug_ | Spices | Chain
>             deriving (Eq,Ord,Enum,Show)

> objectsVoc :: [(String, Object)]
> objectsVoc = [("key", Keys),("keys", Keys),
>            ("lamp", Lamp),("lante", Lamp),("headl", Lamp),
>            ("grate", Grate),
>            ("cage", Cage),
>            ("rod", Rod),
>            ("bird", Bird),
>            ("door", Door),
>            ("pillo", Pillow),
>            ("snake", Snake),
>            ("fissu", Crystal),
>            ("table", Tablet),
>            ("clam", Clam),
>            ("oyste", Oyster),
>            ("magaz", Mag),("issue", Mag),("spelu", Mag),
>            ("\"spel", Mag),
>            ("dwarf", Dwarf),("dwarv", Dwarf),
>            ("knife", Knife),("knive", Knife),
>            ("food", Food),("ratio", Food),
>            ("bottl", Bottle),("jar", Bottle),
>            ("water", Water),("h2o", Water),
>            ("oil", Oil),
>            ("mirro", Mirror),
>            ("plant", Plant),("beans", Plant),
>            ("stala", Stalactite),
>            ("shado", Shadow),("figur", Shadow),
>            ("axe", Axe),
>            ("drawi", Art),
>            ("pirat", Pirate),
>            ("drago", Dragon),
>            ("chasm", Bridge),
>            ("troll", Troll),
>            ("bear", Bear),
>            ("messa", Message),
>            ("volca", Geyser),("geyse", Geyser),
>            ("vendi", Pony),("machi", Pony),
>            ("batte", Batteries),
>            ("moss", Moss),("carpe", Moss),
>            ("gold", Gold),("nugge", Gold),
>            ("diamo", Diamonds),
>            ("silve", Silver),("bars", Silver),
>            ("jewel", Jewels),
>            ("coins", Coins),
>            ("chest", Chest),("box", Chest),("treas", Chest),
>            ("eggs", Eggs),("egg", Eggs),("nest", Eggs),
>            ("tride", Trident),
>            ("ming", Vase),("vase", Vase),("shard", Vase),
>            ("potte", Vase),
>            ("emera", Emerald),
>            ("plati", Pyramid),("pyram", Pyramid),
>            ("pearl", Pearl),
>            ("persi", Rug),("rug", Rug),
>            ("spice", Spices),
>            ("chain", Chain)]

> minTreasure :: Object
> minTreasure = Gold

> isTreasure :: Object -> Bool
> isTreasure = (> minTreasure)

