module Infinite where

data Infinite a = NaN | NegInf | F a | PosInf deriving (Eq)

instance Show a => Show (Infinite a) where
    show NaN = "NaN"
    show NegInf = "-∞"
    show PosInf = "∞"
    show (F x) = show x

instance Ord a => Ord (Infinite a) where
    compare NegInf NegInf = EQ
    compare PosInf PosInf = EQ
    compare NegInf _ = LT
    compare _ NegInf = GT
    compare PosInf _ = GT
    compare _ PosInf = LT
    compare (F x) (F y) = compare x y

-- Warning: NaN progragates without errors!!
instance (Num a, Eq a) => Num (Infinite a) where
    abs PosInf = PosInf
    abs NegInf = PosInf
    abs NaN = NaN
    abs (F x) = F $ abs x

    PosInf + PosInf = PosInf
    PosInf + F{} = PosInf
    F{} + PosInf = PosInf
    NegInf + NegInf = NegInf
    NegInf + F{} = NegInf
    F{} + NegInf = NegInf
    (F x) + (F y) = F (x + y)
    _ + _ = NaN

    negate PosInf = NegInf
    negate NegInf = PosInf
    negate (F x) = F $ negate x
    negate _ = NaN

    PosInf * (F 0) = NaN
    NegInf * (F 0) = NaN
    (F 0) * PosInf = NaN
    (F 0) * NegInf = NaN
    PosInf * (F x) = if signum x == 1 then PosInf else NegInf
    (F x) * PosInf = if signum x == 1 then PosInf else NegInf
    NegInf * (F x) = if signum x == 1 then NegInf else PosInf
    (F x) * NegInf = if signum x == 1 then NegInf else PosInf
    (F x) * (F y) = F (x * y)
    _ * _ = NaN

    fromInteger = F . fromInteger

    signum PosInf = F 1
    signum NegInf = F (-1)
    signum (F x) = F $ signum x
    signum _ = NaN
