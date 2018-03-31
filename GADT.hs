{-# LANGUAGE GADTs, CPP #-}

module Expr where

#define USE_GADT 1

#if USE_GADT

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Eq  x y) = eval x == eval y

#else

data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Eq Expr Expr

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)
eval (Add x y) = applyInt (+) (eval x) (eval y)
eval (Mul x y) = applyInt (*) (eval x) (eval y)
eval (Eq x y)  = applyBool (==) (eval x) (eval y)

applyInt :: (Int -> Int -> Int) -> Maybe (Either Int Bool) -> Maybe (Either Int Bool) -> Maybe (Either Int Bool)
applyInt f (Just (Left x)) (Just (Left y)) = Just (Left $ f x y)
applyInt f _               _               = Nothing

applyBool :: (Int -> Int -> Bool) -> Maybe (Either Int Bool) -> Maybe (Either Int Bool) -> Maybe (Either Int Bool)
applyBool f (Just (Left x)) (Just (Left y)) = Just (Right $ f x y)
applyBool f _               _               = Nothing

#endif