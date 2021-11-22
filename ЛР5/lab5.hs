-- Реалiзувати та скомпiлювати одну з програм, розроблених у лабораторнiй роботi № 3 для Вашого варiанта 
-- з введенням даних: а) з клавiатури, б) з файлу та виведенням результатiв: в) на екран, г) у файл.

--Завдання за варіантом:
--Роздiлити список на двi частини при заданiй довжинi першої n,
-- напр. при n=3: "abcdefghik"⇒ ("abc "defghik")

import Data.List ()
import System.IO (IOMode (ReadMode), openFile, hIsEOF, hGetLine, hClose)
import Distribution.Compat.CharParsing (CharParsing(text))

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

divideListN :: [a] -> Int -> ([a], [a])
divideListN [] _ = ([],[])
divideListN all@(x:xs) n
    | n > 0 = (x:ys, zs)
    | otherwise = ([], all)
    where 
        (ys, zs) = divideListN xs (n-1)

main :: IO ()
main = do

-- Закоментуйте, будь ласка, один варіант виводу та один варіант вводу для коректної роботи програми

-- -- З клавіатури
    l <- getLine
    n <- readLn

-- -- На екран
    putStr "Result: "
    print $ divideListN l n 

-- З файлу
    handle <- openFile "input.txt" ReadMode 
    l <- hGetLine handle
    str <- hGetLine handle
    let n = read str :: Int
    hClose handle

-- В файл
    let res = divideListN l n
    let f = fst res
    let s = snd res
    let res_final = f ++ "\n" ++ s
    writeFile "output.txt" res_final
    putStr "Result is in the file output.txt :) "


-- Результат тестування зчитування з консолі та виведення в консоль

-- C:\Users\HP\Desktop\универ\3 курс\5 семестр\НП\ЛР5>lab5.exe
-- anscfj
-- 2
-- ("an","scfj")

-- C:\Users\HP\Desktop\универ\3 курс\5 семестр\НП\ЛР5>lab5.exe
-- annakavunCS-32
-- 4
-- ("anna","kavunCS-32")


-- Результат тестування зчитування з файлу та виведення в консоль

-- вміст файлу
-- just test text
-- 6 

-- C:\Users\HP\Desktop\универ\3 курс\5 семестр\НП\ЛР5>lab5.exe
-- ("just t","est text")

-- вміст файлу 
-- good evening, it's me
-- 14

-- C:\Users\HP\Desktop\универ\3 курс\5 семестр\НП\ЛР5>lab5.exe
-- ("good evening, ","it's me")


-- Результат тестування зчитування з файлу та запису в файл


-- вміст файлу "input.txt" 
-- good evening, it's me
-- 14

-- C:\Users\HP\Desktop\универ\3 курс\5 семестр\НП\ЛР5>lab5.exe
-- Result is in the file output.txt :)

-- вміст файлу "output.text"
-- good evening, 
-- it's me
