module Task4_2 where
import Control.Monad (ap, liftM)

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { n <- FourOf 1 2 3 4; m <- FourOf 4 6 7 8; return $ n + m } === FourOf 5 8 10 12

instance Functor FourOf where fmap = liftM

instance Applicative FourOf where 
  pure = return 
  (<*>) = ap
	
instance Monad FourOf where
  return n = FourOf n n n n
  (FourOf n1 n2 n3 n4) >>= f = FourOf m1 m2 m3 m4 where
    FourOf m1 _ _ _ = f n1
    FourOf _ m2 _ _ = f n2
    FourOf _ _ m3 _ = f n3
    FourOf _ _ _ m4 = f n4