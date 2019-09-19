module Task3_3 where

import Data.Bits

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Сложение множеств - это множество, содержащее в себе все элементы исходных множеств 

newtype PSetAdd a = PSetAdd{ c_Add :: (a -> Bool) }

instance Semigroup (PSetAdd a) 
   where
    (<>) (PSetAdd m) (PSetAdd n) = PSetAdd (\x -> m x || n x)

instance Monoid (PSetAdd a) 
  where
    mempty = PSetAdd (\x -> False)

-- Пересечение множеств - это множество, которому принадлежат те и только те элементы, 
-- которые одновременно принадлежат всем данным множествам.

newtype PSetCmp a = PSetCmp{ c_Cmp :: (a -> Bool) }

instance Semigroup (PSetCmp a) 
  where
    (<>) (PSetCmp m) (PSetCmp n) = PSetCmp (\x -> m x && n x)

instance Monoid (PSetCmp a) 
   where
     mempty = PSetCmp (\x -> False)

-- Разность множеств - теоретико-множественная операция, результатом которой является новое множество, 
-- включающее все элементы первого множества, не входящие во второе.

newtype PSetDif a = PSetDif{ c_Dif :: (a -> Bool) }

instance Semigroup (PSetDif a) 
   where
    (<>) (PSetDif m) (PSetDif n) = PSetDif (\x -> (/=) (m x) (n x))

instance Monoid (PSetDif a) 
  where
    mempty = PSetDif (\x -> False)


instance Functor PSetDif 
  where
   fmap _ _ = PSetDif (\x -> False)