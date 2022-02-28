module Point where

data Point = PointI Int Int | PointD Double Double

instance Eq Point where
    PointI _ _ == PointD _ _     = False
    PointD _ _ == PointI _ _     = False
    PointI x1 y1 == PointI x2 y2 = (x1 == x2) && (y1 == y2)
    PointD x1 y1 == PointD x2 y2 = (x1 == x2) && (y1 == y2)

instance Show Point where
    show (PointI 0 0) = "*orig*"
    show (PointD 0 0) = "$orig$"
    show (PointI x y) = "integral point (" ++ show x ++ ", " ++ show y ++ ")"
    show (PointD x y) = "real point (" ++ show x ++ ", " ++ show y ++ ")"
