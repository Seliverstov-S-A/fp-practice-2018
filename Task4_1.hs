module Task4_1 where

-- Интересным (но почти бесполезным) примером монады является обычная функция, 
--если типовой аргумент присутствует в её возвращаемом значении. 
--В файле Task4_1.hs приведён тип данных, построенный на основе функции из String в a. 
--Реализуйте для него классы Functor, Applicative и Monad.

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`.

data FunMonad a = FunMonad { fun :: String -> a }

-- Реализация класса `Functor` для типа `FunMonad`.
instance Functor FunMonad 
   where
     fmap f (FunMonad foo) = FunMonad (f . foo)
-- Реализация класса `Applicative` для типа `FunMonad`.	
instance Applicative FunMonad 
  where
    pure a = FunMonad (\x -> a)
    (<*>) (FunMonad m) (FunMonad n) = FunMonad (\x -> m x $ n x)
	
-- Реализация класса `Monad` для типа `FunMonad`.		
instance Monad FunMonad 
  where
    return = pure
    (>>=) m n = FunMonad (\x -> fun (n $ (fun m) x) x)	