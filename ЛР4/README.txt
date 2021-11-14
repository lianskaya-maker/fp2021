ЗАВДАННЯ. Записник. У записнику зберiгається iнформацiя про знайомих: телефон
(iм’я, телефон — один, або кiлька), нагадування про день народження (iм’я, дата
— день та мiсяць), зустрiчi (дата та час, мiсце, опис — тема зустрiчi, зауваження
про зустрiч — чи вiдбулась та iнше). Визначте функцiї для :
12. пошуку iмен за першою лiтерою iменi;

Створення нового типу даних:

type Name = String
type Phone  = String
type Notes  = String
type DayMonthYear = (Int,Int,Int) 
data Notebook = BirthdayCalendar Name DayMonthYear |  
               Phones  Name Phone |
               Meeting   Notes  DayMonthYear
               deriving Show

Функція для визначення чи є вже n елемент (в даному випадку ім'я) в списку l (список вже витягнутих імен за пошуком першої літери):
у випадку, якщо так, тоді в список він не додається повторно, інакше - додається на початок.

unical :: Eq a => a -> [a] -> [a]
unical n l
  | n `elem` l = l
  | otherwise = n:l

Основна функція для знаходження імені за першою літерою. Так як ім'я зустрічається двічі: в днях народження та телефонах, потрібно
визначити два можливих варіанти: знаходження імен за літерою в обох опціях та виключення повторую імен в кінцевому масиві.

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

В масиві notebook збережені дані типу Notebook, які ми перевірятимемо.
Основний масив виглядає так:

notebook = [
    Phones "Hleb" "066 623 42 34",
    Meeting "Discussion" (28, 1, 1991),
    BirthdayCalendar "Arseniy" (20, 2, 1998),
    Phones "Maxim" "097 311 34 34",
    BirthdayCalendar "Inga" (11, 8, 1996), 
    BirthdayCalendar "Aleksandra" (14, 2, 2018),
    Meeting "StandUp" (12, 9, 2005),
    BirthdayCalendar "Nataliya" (23, 1, 1981),
    Phones "Olga" "050 000 15 31",
    Phones "Zinayida" "066 322 22 81",
    BirthdayCalendar "Vyacheslav" (8, 3, 1946),
    Phones "Ignat" "093 426 64 46",
    Phones "Ihor" "067 523 41 80",
    BirthdayCalendar "Volodymyr" (7, 7, 1977),
    Meeting "Serious talk" (21, 8, 2009)
           ]

При тестуванні даний масив мав різні варіації.
