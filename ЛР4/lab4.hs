--Лабораторна робота №4
--студентки групи КН-32 підгрупа 1
--Кавун Анни
--Варіант 12

--Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

import Data.List ()
import System.IO

-- Записник. У записнику зберiгається iнформацiя про знайомих: телефон
-- (iм’я, телефон — один, або кiлька), нагадування про день народження (iм’я, дата
-- — день та мiсяць), зустрiчi (дата та час, мiсце, опис — тема зустрiчi, зауваження
-- про зустрiч — чи вiдбулась та iнше). Визначте функцiї для :
-- 12. пошуку iмен за першою лiтерою iменi;


type Name = String
type Phone  = String
type Notes  = String
type DayMonthYear = (Int,Int,Int) 
data Notebook = BirthdayCalendar Name DayMonthYear |  
               Phones  Name Phone |
               Meeting   Notes  DayMonthYear
               deriving Show


isAlreadyExists n l
  | n `elem` l = l
  | otherwise = n:l


searchNameOn:: [Notebook] -> Char -> [Name]
searchNameOn [] _ = []
searchNameOn ( Phones [] _ :xs) c = searchNameOn xs c
searchNameOn ( Phones all@(n:_) _ :xs) c 
    | n == c = isAlreadyExists all (searchNameOn xs c)
    | otherwise = searchNameOn xs c
searchNameOn ( BirthdayCalendar [] _ :xs) c = searchNameOn xs c
searchNameOn ( BirthdayCalendar all@(n:_) _ :xs) c 
    | n == c = isAlreadyExists all (searchNameOn xs c)
    | otherwise = searchNameOn xs c
searchNameOn (_:xs) c = searchNameOn xs c


--Тестування

-- notebook = [
--     Phones "Hleb" "066 623 42 34",
--     Meeting "Discussion" (28, 1, 1991),
--     BirthdayCalendar "Arseniy" (20, 2, 1998),
--     Phones "Maxim" "097 311 34 34",
--     BirthdayCalendar "Inga" (11, 8, 1996), 
--     BirthdayCalendar "Aleksandra" (14, 2, 2018),
--     Meeting "StandUp" (12, 9, 2005),
--     BirthdayCalendar "Nataliya" (23, 1, 1981),
--     Phones "Olga" "050 000 15 31",
--     Phones "Zinayida" "066 322 22 81",
--     BirthdayCalendar "Vyacheslav" (8, 3, 1946),
--     Phones "Ignat" "093 426 64 46",
--     Phones "Ihor" "067 523 41 80",
--     BirthdayCalendar "Volodymyr" (7, 7, 1977),
--     Meeting "Serious talk" (21, 8, 2009)
--            ]

-- ghci> searchNameOn notebook 'A'
-- ["Arseniy","Aleksandra"]

--Тестування

-- notebook = [
--     Phones "Ignat" "093 426 64 46",
--     Phones "Ihor" "067 523 41 80",
--     BirthdayCalendar "Volodymyr" (7, 7, 1977),
--     Meeting "Serious talk" (21, 8, 2009)
--            ]

-- ghci> searchNameOn notebook 'P'
-- []

--Тестування

-- notebook = [
--     BirthdayCalendar "Inga" (11, 8, 1996), 
--     BirthdayCalendar "Aleksandra" (14, 2, 2018),
--     Meeting "StandUp" (12, 9, 2005),
--     BirthdayCalendar "Nataliya" (23, 1, 1981),
--     Phones "Olga" "050 000 15 31",
--     Phones "Zinayida" "066 322 22 81",
--     BirthdayCalendar "Vyacheslav" (8, 3, 1946),
--     Phones "Ignat" "093 426 64 46",
--     Phones "Ihor" "067 523 41 80"
--            ]

-- ghci> searchNameOn notebook 'V'
-- ["Vyacheslav"]
-- ghci> searchNameOn notebook 'I'
-- ["Inga","Ignat","Ihor"]

 --Висновок: Під час лабораторної роботи я мала змогу познайомитися та 
 --імплементувати класи типів мови Haskell. Також ознайомилася з системою
 --типів та класів типів, визначила власні функції для нового типу. 

