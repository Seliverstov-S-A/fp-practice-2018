module Task1_2 where

import Prelude hiding (gcd, pow, is_prime)
import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел

gcd :: Integer -> Integer -> Integer
gcd x y  | x < 0            = error "x must be x > 0 !!!"
         | y < 0            = error "y must be y > 0 !!!"
         | x == 0 && y == 0 = error "Invalid input x != 0, y != 0!!!"
         | y == 0           = x
         | otherwise = gcd y (rem x y)
		 

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?

doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = if from == to then error "from must be unequal to (from != to) " 
                                                else (ceiling $ sqrt $ fromIntegral from) <= (floor $ sqrt $ fromIntegral to-1)

												
-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?

isRealDate :: Int -> Int -> Int -> Bool
isRealDate day month year = if year  < 1 || 
                                 month < 1|| 
								   month > 12 || 
								      day  < 1 then False 
									             else if month == 2 
												    then if year `mod` 4 == 0 
												           then day <= 29 else day <= 28
                                                         else day <= md!!(month - 1)
															where md = [31, 28, 31, 30, 31, 30,
             															31, 31, 30, 31, 30, 31]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя

pow:: Integer->Integer->Integer
pow x n  | n < 0                       = error "!!!Pow must be positive"
		 | x == 1 || n == 0 || x == 0  = 1
         | n == 1                      = x
         | even n                      = (pow x ( div n 2))*(pow x ( div n 2)) 
         | odd n                       = x * (pow x (n-1))


-- является ли данное число простым?

is_prime :: Integer -> Bool
is_prime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

--type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
--shapeArea :: [Point2D] -> Double
--shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
--triangleKind :: Point2D -> Point2D -> Point2D -> Integer
--triangleKind a b c = todo
