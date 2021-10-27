--Лабораторна робота №3
--студентки групи КН-32 підгрупа 1
--Кавун Анни
--Варіант 12

--Мета:Набути досвiду визначення та використання функцiй вищого порядку.

--Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
--застосуванням вбудованих функцiй.

import Data.List ()
import System.IO


-- Завдання 1. Роздiлити список на двi частини при заданiй довжинi першої n,
-- напр. при n=3: "abcdefghik"⇒ ("abc "defghik")

--а) без застосування вбудованих функцій
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

divideListN' :: [a] -> Int -> ([a], [a])
divideListN' [] _ = ([],[])
divideListN' all@(x:xs) n
    | n > 0 = (x:ys, zs)
    | otherwise = ([], all)
    where 
        (ys, zs) = divideListN' xs (n-1)

 --Тестування

-- ghci> divideListN' "qagj213ghj" 5
-- ("qagj2","13ghj")
-- ghci> divideListN' "qagj213ghj" 15
-- ("qagj213ghj","")
-- ghci> divideListN' "qagj213ghj" 0
-- ("","qagj213ghj")
-- ghci> divideListN' "11111111000000000" 8
-- ("11111111","000000000")

--б) з застосуванням вбудованих функцiй

divideListN :: [a] -> Int -> ([a], [a])
divideListN [] _ = error "The list is empty"
divideListN xs 0 = error "Can't divide a list to (0, _)"
divideListN xs n
    | n < l = splitAt n xs
    | otherwise = error "n is more or equal than lenght"
    where 
        l = length xs


 --Тестування

-- ghci> divideListN "1234567" 2
-- ("12","34567")

-- ghci> divideListN "123" 4
-- Exception: n is more or equal than lenght
-- CallStack (from HasCallStack):
--   error, called at lab3.hs:24:19 in main:Main

-- ghci> divideListN "" 2
-- Exception: The list is empty
-- CallStack (from HasCallStack):
--   error, called at lab3.hs:20:20 in main:Main

-- ghci> divideListN "123456" 0
-- Exception: Can't divide a list to (0, _)
-- CallStack (from HasCallStack):
--   error, called at lab3.hs:21:20 in main:Main

-- ghci> divideListN "asdfwwv12dwjn1h4bk" 7
-- ("asdfwwv","12dwjn1h4bk")



--Завдання 2. Перевiрити гiпотезу Ґольдбаха у вказаному дiапазонi.

--а) без застосування вбудованих функцій

myHead :: [a] -> a
myHead (x:xs) = x

goldbach :: Int -> (Int, Int)
goldbach a = myHead $
                    filter (\(x,y) -> isPrime x && isPrime y) $
                    map (\c -> (c, a - c)) [3,5..a `div` 2]
  where
  factors a = filter (isFactor a) [2..a-1]
  isFactor a b = a `mod` b == 0
  isPrime 1 = False
  isPrime a = null $ factors a

goldbachRange :: [Int] -> [(Int, Int)]
goldbachRange [] = []
goldbachRange (x:xs) = goldbach x : goldbachRange xs

--Тестування

-- ghci> goldbachRange [1..10]
-- [*** Exception: lab3.hs:84:1-17: Non-exhaustive patterns in function myHead

-- ghci> goldbachRange [4,6..10]
-- [*** Exception: lab3.hs:84:1-17: Non-exhaustive patterns in function myHead

-- ghci> goldbachRange [6,10..17]
-- [(3,3),(3,7),(3,11)]
-- ghci> goldbachRange [14,16..25]
-- [(3,11),(3,13),(5,13),(3,17),(3,19),(5,19)]


--б) з застосуванням вбудованих функцiй

goldbach' :: Int -> (Int, Int)
goldbach' a = head $
                    filter (\(x,y) -> isPrime x && isPrime y) $
                    map (\c -> (c, a - c)) [3,5..a `div` 2]
  where
  factors a = filter (isFactor a) [2..a-1]
  isFactor a b = a `mod` b == 0
  isPrime 1 = False
  isPrime a = null $ factors a

goldbachRange' :: [Int] -> [(Int, Int)]
goldbachRange' = map goldbach'

 --Тестування

-- ghci> goldbachRange' [1..10]
-- [*** Exception: Prelude.head: empty list
-- ghci> goldbachRange' [4,6..10]
-- [*** Exception: Prelude.head: empty list
-- ghci> goldbachRange' [6,10..17]
-- [(3,3),(3,7),(3,11)]
-- ghci> goldbachRange' [14,16..25]
-- [(3,11),(3,13),(5,13),(3,17),(3,19),(5,19)]

 --Висновок: Під час лабораторної роботи я мала змогу познайомитися та 
 --імплементувати нові функції мови Haskell, а саме: filter, map, null, isPrime та splitAt, 
 --а також дізналася як використовуються та навіщо потрібні такі символи як '.' та '$'.