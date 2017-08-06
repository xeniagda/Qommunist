module Vec
    ( Vec2(..)
    ) where


data Vec2 a =
    Vec2 a a
    deriving (Eq)

instance (Show a) => Show (Vec2 a) where
    show (Vec2 x y) = "[" ++ show x ++ " " ++ show y ++ "]"

instance Functor Vec2 where
    fmap f (Vec2 x y) = Vec2 (f x) (f y)

instance Applicative Vec2 where
    pure f = Vec2 f f
    (Vec2 fx fy) <*> (Vec2 x y) = Vec2 (fx x) (fy y)

