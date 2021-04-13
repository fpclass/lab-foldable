--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Foldables                                                             --
--------------------------------------------------------------------------------

module Lab where

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
    -- foldr :: (a -> b -> b) -> b -> Expr a -> b
    foldr f z (Var x)   = f x z
    foldr f z (Val n)   = z
    foldr f z (Add l r) = foldr f (foldr f z r) l

--------------------------------------------------------------------------------

data Zipper a = Zipper [a] a [a]
    deriving (Eq, Show)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

view :: Zipper a -> a
view (Zipper _ v _) = v

-- left (Zipper [] 1 [2,3]) => Zipper [1] 2 [3]
left :: Zipper a -> Zipper a
left (Zipper ls v [])     = Zipper ls v []
left (Zipper ls v (r:rs)) = Zipper (v:ls) r rs 

right :: Zipper a -> Zipper a
right (Zipper [] v rs) = Zipper [] v rs
right (Zipper (l:ls) v rs) = Zipper ls l (v:rs)

instance Foldable Zipper where
    -- foldr :: (a -> b -> b) -> b -> Zipper a -> b
    -- Zipper [] 1 [2,3,4]
    -- Zipper [1] 2 [3,4]
    -- Zipper [2,1] 3 [4]
    -- ~ [1,2,3,4]
    -- foldr f z (Zipper ls v rs) = 
    --     foldr f (f v (foldr f z rs)) ls
    foldr f z (Zipper ls v rs) = 
        foldl (flip f) (f v (foldr f z rs)) ls

--------------------------------------------------------------------------------

-- | `filterF` is a generalisiation of `filter` which works on all
-- data structures which have an instance of `Foldable`. That is, `filterF`
-- @p xs@ reduces @xs@ to a list of elements which satisfy the predicate @p@.
filterF :: Foldable f => (a -> Bool) -> f a -> [a]
-- filterF p = filter p . toList
filterF p = foldr (\x r -> if p x then x : r else r) []

class Insertable t where
    insert :: a -> t a -> t a

instance Insertable [] where
    insert = (:)

-- data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

-- binsert :: Ord a => a -> BinaryTree a -> BinaryTree a
-- binsert x Leaf = Node Leaf x Leaf
-- binsert x (Node l y r)
--     | x==y = Node l y r
--     | x<y = Node (binsert x l) y r
--     | otherwise = Node l y (binsert x r)

-- instance Insertable BinaryTree where
--     insert = binsert

filterFA :: (Foldable f, Monoid (g a), Insertable g) 
         => (a -> Bool) -> f a -> g a
filterFA p = foldr (\x r -> if p x then insert x r else r) mempty

-- | `asum` @xs@ combines all computations in @xs@ as alternatives in one big
-- computation of type @f a@.
asum :: (Alternative f, Foldable t) => t (f a) -> f a
asum = foldr (<|>) empty

--------------------------------------------------------------------------------
