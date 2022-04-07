triangles = [ (x, y, z) 
            | x <- [1..]
            , y <- [1..x]
            , z <- [x..(x^2 + y^2)]
            , z^2 == x^2 + y^2 
            ]

getTriangles = flip take triangles

main = print . getTriangles . read =<< getLine
