{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-

Similar to functions, data types in haskell can be polymorphic.
Means that they can use some type variables as placeholders, representing
general types.

eg: Function1 :: [a] -> [[a]]  -- a polmorphic function
    Function1 [a] = [a] : []


-}


showEither :: Either String Int -> String
showEither (Left msg) = "Left with string: " ++ msg
showEither (Right n) = "Right with number:" ++ show n


showMaybe :: Maybe Int -> String
showMaybe (Just n) = "Show the number:" ++ show n
showMaybe (Nothing) = "Show Nothing" 


data List a
    = Empty
    | Cons a (List a)
    deriving (Show)

data CustomType = Maybe Int | Dragon

addElement1 :: a -> [a]
addElement1 s = s : []

addElement :: a -> List a
addElement s = Cons s (Empty)


class ArchEnemy a where
    getArchEnemy :: a -> String

instance ArchEnemy Bool where
    getArchEnemy :: Bool -> String
    getArchEnemy True = "False"
    getArchEnemy False = "True"

instance ArchEnemy Int where
    getArchEnemy :: Int -> String
    getArchEnemy i = case i of
        0 -> "Division"
        _ -> "Derivative"

instance ArchEnemy Double where
    getArchEnemy :: Double -> String
    getArchEnemy n
        | isNaN n = "Infinity"
        | isInfinite n = "Limit"
        | otherwise = "NaN"

revealArchEnemy :: (ArchEnemy a, Show a) => a -> String
revealArchEnemy x =
    "The arch-enemy of " ++ show x ++ " is " ++ getArchEnemy x



{- TYPECLASSES -}

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday deriving (Enum, Show)

class TypeClass a where
    action1 :: a -> a
    action2 :: a -> a -> [a]

instance TypeClass String where
    action1 :: String -> String
    action1 x = x ++ x

    action2 :: String -> String -> [String]
    action2 x y = x : y : []

instance TypeClass Weekday where
    action1 :: Weekday -> Weekday
    action1 day = succ day

    action2 :: Weekday -> Weekday -> [Weekday]
    action2 day1 day2 = enumFromTo day1 day2


function1 :: Show a => a -> String
function1 x = action1 $ show x


-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p


-- recursive go
sumGo :: forall a . Num a => [a] -> a
sumGo = go 0
    where
        go :: a -> [a] -> a
        go !acc [] = acc
        go acc (x:xs) = go (acc + x) xs