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
-- goldbach :: [Int] -> Bool 
-- goldbach [] = error "The list is empty"
-- goldbach (x:xs)
--     | isEven x && x > 4 = goldbach xs
--     | isOdd x = goldbach xs
--     | otherwise = False
--     where t = True


--Тестування



--б) з застосуванням вбудованих функцiй


 --Тестування



 --Висновок: Під час лабораторної роботи я 