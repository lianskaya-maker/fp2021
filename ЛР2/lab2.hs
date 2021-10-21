--Лабораторна робота №2
--студентки групи КН-32 підгрупа 1
--Кавун Анни
--Варіант 12

--Мета:Набути досвiду визначення рекурсивних функцiй, використання механiзму
--зiставлення зi зразком i роботи з кортежами та списками.

--Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
--застосуванням вбудованих функцiй.

import Data.List ()
import System.IO


-- Завдання 1. Вставити у список через кожнi n елементiв вказане значення, напр.
--через n=2 значення ’z’: "1234590"⇒ "12z34z59z0".

--а) без застосування вбудованих функцій

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n xs@(_:xs')
   | n > 0     = myDrop (n-1) xs'
   | otherwise = xs

f1 :: [a] -> Int -> a -> [a]

f1 xs 0 y  = xs
f1 [] n y = []
f1 xs n y
 | myLength xs < n = xs
 | otherwise = n `take` xs ++ [y] ++ f1 (drop n xs) n y

 --Тестування

--ghci> f1 "1234567" 2 '@'
--"12@34@56@7"

--ghci> f1 "bnfldjgfkslv" 4 '|'
--"bnfl|djgf|kslv|"

--б) з застосуванням вбудованих функцiй
f2 :: [a] -> Int -> a -> [a]

f2 xs 0 y  = xs
f2 [] n y = []
f2 xs n y
 | length xs < n = xs
 | otherwise = take n xs ++ [y] ++ f2 (drop n xs) n y

 --Тестування

--ghci> f2 "1234567" 2 '@'
--"12@34@56@7"

--ghci> f2 "bnfldjgfkslv" 4 '|'
--"bnfl|djgf|kslv|"

--Завдання 2. Знайти перше просте число в указаному дiапазонi.

--а) без застосування вбудованих функцій

--{-# LANGUAGE ParallelListComp #-}
f3 :: [Int] -> Int

f3 (1:_) = 1
f3 [] = error "There is no prime number"
f3 (x:xs)
    | b = x
    | otherwise = f3 xs
    where b = and [ x `mod` y /= 0 | y <- [2..(x-1)]]

--Тестування

--ghci> f3 [1..10]
--1

--ghci> f3 [2,4..20]
--2

--ghci> f3 [4,8..40] 
-- There is no prime number
--CallStack (from HasCallStack):
--error, called at lab2.hs:57:9 in main:Main

--ghci> f3 [10, 107 ..1000] 
--107

--б) з застосуванням вбудованих функцiй
f4 :: [Int] -> Int

f4 (1:_) = 1
f4 [] = error "There is no prime number"
f4 xs
    | b = x
    | otherwise = f4 (drop 1 xs)
    where 
        b = and [ x `mod` y /= 0 | y <- [2..(x-1)]]
        x = head xs

 --Тестування

--ghci> f4 [1..10]
--1

--ghci> f4 [2,4..20]
--2

--ghci> f4 [4,8..40]
--Exception: There is no prime number
--CallStack (from HasCallStack):
--  error, called at lab2.hs:68:9 in main:Main

--ghci> f4 [10, 107 ..1000]
--107

 --Висновок: Під час лабораторної роботи я здобула практичні навички створення рекурсивних функцій в haskell, 
 --а також мала додаткову практику зі списками та вбудоваинми функціями haskell, що значно полегшують написання коду.
 --Окрім того, я створила декілька функцій з використанням guards, які розширюють можливості функції, та з використанням
 --складних булевих виразів.