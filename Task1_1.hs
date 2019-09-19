module Task1_1 where

import Todo(todo)


data B_opr = Umn | Plus | Minus
    deriving (Show, Eq)

-- тип данных
data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: B_opr, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant x) (IntConstant y) = IntConstant (x + y)
(|+|) x y = BinaryTerm Plus x y

infixl 6 |+|
-- (IntConstant 5) |+| (IntConstant 6) == (IntConstant 11) == BinaryTerm Plus (IntConstant 5) (IntConstant 6)
-- (IntConstant 6) |+| (Variable "x") == BinaryTerm Plus (IntConstant 6) (Variable "x")
-- (IntConstant 6) |+| (Variable "x") |+| (IntConstant 6) == (IntConstant 12) |+| (Variable "x")

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant x) (IntConstant y) = IntConstant (x - y)
(|-|) x y = BinaryTerm Minus x y

infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant x) (IntConstant y) = IntConstant (x * y)
(|*|) x y = BinaryTerm Umn x y

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`


replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ i@(IntConstant x) = i
replaceVar varName replacement v@(Variable x) | (x == varName) = replacement
                                              | otherwise = v
replaceVar varName replacement b@(BinaryTerm o l r) = BinaryTerm o (replaceVar varName replacement l) (replaceVar varName replacement r)                                               



-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant x) (IntConstant y)) = IntConstant (x + y)
evaluate (BinaryTerm Plus (IntConstant 0) term) = term
evaluate (BinaryTerm Plus term (IntConstant 0)) = term

evaluate (BinaryTerm Umn (IntConstant x) (IntConstant y)) = IntConstant (x * y)
evaluate (BinaryTerm Umn (IntConstant 0) term) = IntConstant 0
evaluate (BinaryTerm Umn term (IntConstant 0)) = IntConstant 0
evaluate (BinaryTerm Umn (IntConstant 1) term) = term
evaluate (BinaryTerm Umn term (IntConstant 1)) = term

evaluate (BinaryTerm Minus (IntConstant x) (IntConstant y)) = IntConstant (x - y)
evaluate (BinaryTerm Minus term (IntConstant 0)) = term

evaluate (BinaryTerm op x y) = evaluate (BinaryTerm op (evaluate x) (evaluate y))
evaluate exp = exp

--Test
-- evaluate (BinaryTerm Umn (BinaryTerm Plus (IntConstant 5) (IntConstant 6)) (BinaryTerm Plus (IntConstant 5) (IntConstant 6))) = IntConstant {intValue = 121}
-- replaceVar "x" (IntConstant 10) (BinaryTerm Plus (Variable "x") (Variable "x"))