--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 8: Foldables                                                           --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

import Data.Foldable

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Lab8 as L

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Foldable instance for Expr" $ do
        it "toList e3 ==> []" $
            toList (L.e3 :: L.Expr Int) `shouldBe` []
        it "toList e5 ==> [7]" $
            toList L.e5 `shouldBe` [7]
        it "sum e5    ==> 7" $
            sum L.e5 `shouldBe` 7
        it "toList e7 ==> [(\"x\",22),(\"y\",42),(\"x\",22),(\"y\",42)]" $
            toList L.e7 `shouldBe` [("x",22),("y",42),("x",22),("y",42)]
        it "length e7 ==> 4" $
            length L.e7 `shouldBe` 4
    describe "fromList for Zipper" $
        prop "converts non-empty lists into zippers" $
            \(NonEmpty xs :: NonEmptyList Int) ->
                L.fromList xs === L.Zipper [] (head xs) (tail xs)
    describe "view, left, and right" $ do
        prop "view retrieves the element in the view" $
            \(x :: Int) xs ys -> L.view (L.Zipper xs x ys) === x
        prop "left shifts elements to the left" $
            \(xs :: [Int]) (x :: Int) (NonEmpty ys :: NonEmptyList Int) ->
                L.left (L.Zipper xs x ys) === L.Zipper (x:xs) (head ys) (tail ys)
        prop "right shifts elements to the right" $
            \(ys :: [Int]) (x :: Int) (NonEmpty xs :: NonEmptyList Int) ->
                L.right (L.Zipper xs x ys) === L.Zipper (tail xs) (head xs) (x:ys)
    describe "Foldable instance for Zipper" $ do
        prop "toList (fromList xs) == xs" $
            \(NonEmpty xs :: NonEmptyList Int) ->
                toList (L.fromList xs) === xs
        prop "sum (fromList xs) == sum xs" $
            \(xs :: [Int]) (x :: Int) (ys :: [Int]) ->
            sum (L.Zipper xs x ys) === sum (xs ++ [x] ++ ys)
        prop "product (fromList xs) == product xs" $
            \(xs :: [Int]) (x :: Int) (ys :: [Int]) ->
            product (L.Zipper xs x ys) === product (xs ++ [x] ++ ys)
    describe "filterF" $ do
        it "filterF ((==\"x\") . fst) e7 ==> [(\"x\",22),(\"x\",22)]" $
            L.filterF ((=="x") . fst) L.e7 `shouldBe` [("x",22),("x",22)]
        prop "for lists: filterF p xs == filter p xs" $ \(xs :: [Int]) x ->
            L.filterF (>x) xs === filter (>x) xs
        prop "for zippers: all p (filterF p z)" $ \(xs :: [Int]) x ys y ->
             all (>y) (L.filterF (>y) (L.Zipper xs x ys))
    describe "asum" $ do
        it "asum [Nothing, Just 4, Nothing] ==> Just 4" $
            L.asum [Nothing, Just 4, Nothing] `shouldBe` Just 4
        it "asum (Zipper [Just 4, Nothing] (Just 5) []) ==> Just 4" $
            L.asum (L.Zipper [Just 4, Nothing] (Just 5) []) `shouldBe` Just 4
        it "asum (Zipper [Nothing, Nothing] (Just 5) []) ==> Just 5" $
            L.asum (L.Zipper [Nothing, Nothing] (Just 5) []) `shouldBe` Just 5
        it "asum (Zipper [Nothing, Nothing] Nothing [Just 1]) ==> Just 1" $
            L.asum (L.Zipper [Nothing, Nothing] Nothing [Just 1]) `shouldBe` Just 1

--------------------------------------------------------------------------------
