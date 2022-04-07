import Data.Array

paths :: Integer -> Integer -> Integer
paths width height = table ! (width, height)
    where
        table = array ((0, 0), (width, height)) $
            ((0, 0), 0) :
            [((w, 0), 1) | w <- [0..width]] ++
            [((0, h), 1) | h <- [0..height]] ++
            [((x, y), table ! (x-1, y) + table ! (x, y-1)) | x <- [1..width], y <- [1..height]]

main = do
    [width, height] <- (map read.words) `fmap` getLine
    print $ paths width height
