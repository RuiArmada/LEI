import Data.Char
import Data.List

-- Question 01 --

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' init end | init > end = []
                     | otherwise = init : enumFromTo' (init + 1) end

-- Question 02 --

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' init step end | init > end && step - init > 0 || init < end && step - init < 0 = []
                              | otherwise = init : enumFromThenTo' step (2 * step - init) end

-- Question 03 --

sum' :: [a] -> [a] -> [a]
sum' [] l = l
sum' l [] = l
sum' (h:t) l = h : sum' t l

-- Question 04 --

find' :: [a] -> Int -> a
find' (h:t) 0 = h
find' (h:t) n = find' t (n-1)

-- Question 05 --

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

-- Question 06 --

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (h:t) | n > 0 = h : take' (n - 1) t
              | otherwise = []

-- Question 07 --

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' n (h:t) | n < 0 = h:t
              | otherwise = drop' (n - 1) t

-- Question 08 --

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:y) = (h,x) : zip' t y

-- Question 09 -- 

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = x == h || elem' x t

-- Question 10 --

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x | n >= 0 = x : replicate' (n - 1) x
               | otherwise = []

-- Question 11 --

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' x (h:t) = h : x : intersperse' x t

-- Question 12 --
group' :: Eq a => [a] -> [[a]]
group' l = [l]
group' (x:y:t) | x == y = (x : head afterG ) : tail afterG
               | otherwise = [x] : group' (y:t)
               where
                   afterG = group' (y:t)

-- Question 13 --

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (h:t) = h ++ concat' t

-- Question 14 --

init' :: [a] -> [[a]]
init' [] = [[]]
init' l = init'(init l) ++ [l]

-- Question 15 --

tail' :: [a] -> [[a]]
tail' [] = [[]]
tail' l = l : tail'(tail l)

-- Question 16 --

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                          | otherwise = False

-- Question 17 --

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' xs ys | lxs == lys = isSuffixOf' ixs iys
                  | otherwise = False
                  where
                      lxs = last xs
                      lys = last ys
                      ixs = init xs
                      iys = init ys

-- Question 18 --

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] l = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

-- Question 19 --

aux :: Eq a => Int -> a -> [a] -> [Int]
aux _ _ [] = []
aux n x (h:t) | x == h = n : aux (n + 1) x t
              | otherwise = aux (n + 1) x t

elemIndex' :: Eq a => a -> [a] -> [Int]
elemIndex' = aux 0 -- Here we can simplify the Function by ommiting the "a" & "[a]" elements

-- Question 20 -- 

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : filter (/= h) (nub' t)

-- Question 21 --

delete' :: Eq a => a -> [a] -> [a]
delete' x [] = []
delete' x (h:t) | x == h = t
                | otherwise = h : delete' x t

-- Question 22 --

rem :: Eq a => [a] -> [a] -> [a]
rem l [] = l
rem [] l = []
rem l (h:t) = rem (delete' h l) t

-- Question 23 --

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t) | h `elem` l = union' l t
               | otherwise = union' (l ++ [h]) t

-- Question 24 --

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' l [] = []
intersect' (h:t) l | h `elem` t = h : intersect' t l
                   | otherwise = intersect' t l

-- Question 25 --

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x > h = h : insert' x t
                | otherwise = x : h : t

-- Question 26 --

unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ [' '] ++ unwords' t

-- Question 27 --

unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ ['\n'] ++ unlines' t

-- Question 28 --

aux2 :: Ord a => Int -> Int -> a -> [a] -> Int
aux2 i n x [] = i
aux2 i n x (h:t) | h > x = aux2 n (n + 1) h t
                 | otherwise = aux2 i (n + 1) x t

-- Question 29 --

hasRep :: Eq a => [a] -> Bool
hasRep [] = False
hasRep (h:t) = h `elem` t || hasRep t

-- Question 30 --

algorithm :: [Char] -> [Char]
algorithm [] = []
algorithm (h:t) | '1' <= h && h <= '9' = h : algorithm t
                | otherwise = algorithm t

-- Question 31 -- 

posImpar :: [a] -> [a]
posImpar _ = []
posImpar (x:y:t) = y : posImpar t

-- Question 32 --

posPar :: [a] -> [a]
posPar l = l
posPar (x:y:t) = x : posPar t

-- Question 33 --

isSorted :: Ord a => [a] -> Bool
isSorted _ = True
isSorted (x:y:t) | x <= y = isSorted (y:t)
                 | otherwise = False

-- Question 34 --

iSort :: Ord a => [a] -> [a]
iSort = foldr insert' []

-- Question 35 --

minus :: String -> String -> Bool
minus [] l = True
minus l [] = False
minus (x:xs) (y:ys) | x == y = minus xs ys
                    | x < y = True
                    | x > y = False

-- Question 36 --

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((y,n):t) | x == y = True
                     | otherwise = elemMSet x t

-- Question 37 --

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):t) = n + lengthMSet t 

-- Question 38 --

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x : converteMSet xs
converteMSet ((x,n):xs) = x : converteMSet ((x,n-1):xs)

-- Question 39 --

insertMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insertMSet x [] = [(x,1)]
insertMSet x ((y,n):t) | x == y = (y,n+1) : t
                       | otherwise = (y,n) : insertMSet x t

-- Question 40 -- 

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((y,n):t) | x == y && n == 1 = t 
                       | x == y = (y,n-1) : t
                       | otherwise = (y,n) : removeMSet x t 

-- Question 41 --

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insertMSet h (constroiMSet t)

-- Question 42 --

auxl :: [Either a b] -> [a]
auxl [] = []
auxl ((Left a):t) = a : auxl t

auxr :: [Either a b] -> [b]
auxr [] = []
auxr ((Right b):t) = b : auxr t 

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' l = (auxl l, auxr l)

-- Question 43 -- 

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just a : t) = a : catMaybes' t
catMaybes' (Nothing : t) = catMaybes' t

-- Question 44 --

data Movimento = Norte
               | Sul
               | Este
               | Oeste
               deriving Show 

pos :: (Int,Int) -> [Movimento] -> (Int,Int)
pos (x,y) [] = (x,y)
pos (x,y) (h:t) = case h of 
                        Norte -> pos (x , y+1) t
                        Sul   -> pos (x , y-1) t
                        Este  -> pos (x+1 , y) t
                        Oeste -> pos (x-1 , y) t

-- Question 45 --

path :: (Int,Int) -> (Int,Int) -> [Movimento]
path (x,y) (z,w) | x == z && y == w = []
                 | x > z = Oeste : path (x-1 , y) (z,w)
                 | x < z = Este : path (x+1 , y) (z,w)
                 | y > w = Sul : path (x , y-1) (z,w)
                 | y < w = Norte : path (x , y+1) (z,w) 

-- Question 46 --

vertical :: [Movimento] -> Bool
vertical [] = True 
vertical (Norte : t) = vertical t 
vertical (Sul : t) = vertical t 
vertical _ = False 

-- Question 47 --

data Posicao = Pos Int Int
                deriving Show 

quadDist :: Posicao -> Posicao -> Int
quadDist (Pos x y) (Pos z w) = (z - x)^2 + (w - y)^2

center' :: [Posicao] -> Posicao
center' [p] = p
center' (p1:p2:t) | quadDist p1 c <= quadDist p2 c = center' (p1:t)
                  | otherwise = center' (p2:t)
                  where
                      c = Pos 0 0

-- Question 48 --

dif :: Posicao -> Posicao -> Bool 
dif (Pos x y) (Pos z w) = z /= z || y /= w

neighbor :: Posicao -> [Posicao] -> [Posicao]
neighbor p [] = []
neighbor p (h:t) | quadDist p h <= 2 && dif p h = h : neighbor p t 
                 | otherwise = neighbor p h

-- Question 49 -- 

sameOrd :: [Posicao] -> Bool
sameOrd ((Pos x y):(Pos z w):t) = y == w && sameOrd((Pos x y):t) -- You can do this with the first and second functions of the Haskell Prelude
sameOrd _ = True

-- Question 50 --

data Semaforo = Verde
              | Amarelo
              | Vermelho
              deriving Show 

countRed :: [Semaforo] -> Int 
countRed [] = 0
countRed (h:t) = case h of 
                    Vermelho -> countRed t
                    _ -> 1 + countRed t 