import Super.Canvas (scale, combine, translate, circle, rekt)
import Super.Trees
import Super.Canvas.JS
import Super.Canvas.Types

main = cx >>= (\c -> (write c . draws) 
                       ((scale (1,1)) 
                         testshapes2))

testshapes = combine [ circle (200, 150) 10 True Blue 
                     , rekt (100, 300) (10,60) Green  ]

testshapes2 = translate (450,50) ((prepSTree . fst) (randomTrees 3 89))
