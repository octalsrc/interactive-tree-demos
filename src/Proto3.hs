import Super.Canvas.Types
import Super.Canvas.JS

main = cx >>= (\c -> (write c . prims) testshapes) 

testshapes = Node (50,50) (2,2) [ Prim (Circle 30 True Red) ]
