module Language.Hasmtlib.Example.Codec where

import Language.Hasmtlib
import GHC.Generics (Generic)

data Foo a = Foo a a deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Applicative Foo where
  pure x = Foo x x
  (Foo f g) <*> (Foo x y) = Foo (f x) (g y)

instance Equatable a => Equatable (Foo a) 
-- Leverage Functor, Foldable, Traversable and Applicative and get default instances
instance Codec a => Codec (Foo a)
instance Variable a => Variable (Foo a)

main :: IO ()
main = do
  res <- solveWith (solver z3) $ do
    setLogic "ALL"

-- These do all the same:    
    let (c1,c2) :: (Expr IntSort, Expr RealSort) = (5, 10)
--  let (c1,c2) :: (Expr IntSort, Expr RealSort) = encode (5, 10)
--  let (c1,c2) :: (Expr IntSort, Expr RealSort) = (constant 5, constant 10)
--  let (c1,c2) :: (Expr IntSort, Expr RealSort) = (fromInteger 5, fromInteger 10)

    foo :: Foo (Expr IntSort) <- variable
    let constantFoo = encode $ Foo 123 456

    assert $ foo === constantFoo

    return (foo,(c1, [c2]))

  print res
