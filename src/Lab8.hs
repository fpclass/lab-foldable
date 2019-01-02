--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 8: Foldables                                                           --
--------------------------------------------------------------------------------

module Lab8 where

import Control.Applicative
import Data.Foldable hiding (asum)

--------------------------------------------------------------------------------

data Expr a = Var a
            | Val Int
            | Add (Expr a) (Expr a)

e1 :: Expr String
e1 = Var "x"

e2 :: Expr (String, Int)
e2 = Var ("x",22)

e3 :: Expr a
e3 = Val 4

e4 :: Expr String
e4 = Add (Val 8) (Var "y")

e5 :: Expr Int
e5 = Add (Val 8) (Var 7)

e6 :: Expr (String, Int)
e6 = Add e2 (Var ("y",42))

e7 :: Expr (String, Int)
e7 = Add e6 e6

instance Foldable Expr where
    foldr = undefined

--------------------------------------------------------------------------------

data Zipper a = Zipper [a] a [a]
    deriving (Eq, Show)

fromList :: [a] -> Zipper a
fromList = undefined

view :: Zipper a -> a
view = undefined

left :: Zipper a -> Zipper a
left = undefined

right :: Zipper a -> Zipper a
right = undefined

instance Foldable Zipper where
    foldr = undefined

--------------------------------------------------------------------------------

-- | `filterF` is a generalisiation of `filter` which works on all
-- data structures which have an instance of `Foldable`. That is, `filterF`
-- @p xs@ reduces @xs@ to a list of elements which satisfy the predicate @p@.
filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF = undefined

-- | `asum` @xs@ combines all computations in @xs@ as alternatives in one big
-- computation of type @f a@.
asum :: (Alternative f, Foldable t) => t (f a) -> f a
asum = undefined

--------------------------------------------------------------------------------
