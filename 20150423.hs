--trabalho9
--Questao 1
-- Slide 1

--[2,3] 2 itens [[2,3]] 1 item. [Int] [[Int]]
--[2,4,6,8]
--[2..2] = 2 
--[2,7..4] = 2
--[10,9..1] = [10,9,8,7,6,5,4,3,2,1]
--[10..1] = []

fibAux :: Int -> Int
fibAux 1 = 1
fibAux 2 = 1
fibAux n = fibAux(n-1) + fibAux(n-2)

fib :: Int -> [Int]
fib n = [ fibAux a | a <- [1..n] , even (fibAux a)]

sumDigits :: [Char] -> Int
sumDigits (a:[]) = strToInt a
sumDigits (a:as) = strToInt a + sumDigits as

strToInt :: Char -> Int
strToInt c
 | c == '1' = 1
 | c == '2' = 2
 | c == '3' = 3
 | c == '4' = 4
 | c == '5' = 5
 | c == '6' = 6
 | c == '7' = 7
 | c == '8' = 8
 | c == '9' = 9
 | c == '0' = 0

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (a:as) = (ordenar [y | y <- as, (sumDigits (show y)) < (sumDigits (show a))])
                       ++ [a] ++
                      (ordenar [y | y <- as, (sumDigits (show y)) >= (sumDigits (show a))])


--Slide 2
getWord :: String -> String
getWord [] = []
getWord (a:as)
 | a == ' ' = ""
 | otherwise = a : getWord as

dropWord :: String -> String
dropWord [] = []
dropWord (a:as)
 | a == ' ' = ""++as
 | otherwise = ' ' : dropWord (as)

dropSpace :: String -> String
dropSpace [] = []
dropSpace (a:as)
 | a == ' ' = dropSpace as
 | otherwise = (a:as)

type Word = String
type Line = [Word]

splitWords :: String -> [Word]
splitWords [] = []
splitWords (a:as)
 | a == ' ' = splitWords (dropSpace (a:as))
 | otherwise = getWord (a:as) : splitWords (dropWord (a:as))

getLinha :: Int -> [Word] -> Line
getLinha 0 [] = []
getLinha n (a:as) = getLineAux n (a:as) 0 0

getLineAux :: Int -> [Word] -> Int -> Int -> Line
getLineAux n list i s
 | s >= n = []
 | otherwise = pos : getLineAux n list (i+1) (s + (length pos))
 where pos = list !! i

dropLine :: Int -> [Word] -> [Word]
dropLine _ [] = []
dropLine n (a:as)
	|n == 0 = a:as
	|otherwise = (dropLine (n-1) as)


splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ls = ((getLinha tam ls):(splitLines (dropLine 1 ls)))
	where
		tam = 5

fill :: String -> [Line]
fill st = splitLines (splitWords st)

joinLines :: [Line] -> String
joinLines (a:[]) = (a !! 0)
joinLines (a:as) = (a !! 0)++" "++(joinLines as)


--Slide 3
agrupar :: Ord t => [[t]] -> [(t,Int)]
agrupar list = agruparAux (foldr (++) [] list) 0

agruparAux :: Ord t => [t] -> Int -> [(t,Int)]
agruparAux list index
 | index == length list = []
 | (repBefore index 0 list (list !! index)) && index <= ((length list)-1) = agruparAux list (index+1)
 | not (repBefore index 0 list (list !! index)) && index <= ((length list)-1) = ((list !! index), (reps list (list !! index))) : agruparAux list (index+1)

reps :: Ord t => [t] -> t -> Int
reps (a:[]) c = if a == c then 1 else 0
reps (a:as) c
 | a == c = 1 + reps as c
 | otherwise = reps as c

repBefore :: Ord t => Int -> Int -> [t] -> t -> Bool
repBefore _ i [] c = False
--repBefore _ _ (a:[]) c = a==c  
repBefore index i (a:as) c
 | index == i = False
 | a == c = True || repBefore index (i+1) as c
 | a /= c = False || repBefore index (i+1) as c


 --Slide 4

bfs :: Eq t => Tree t -> t -> Bool
bfs NilT n = False
bfs (Node no left right) n
 | no == n = True
 | otherwise = (||) (bfs left n) (bfs right n)

--Slide 5
member :: Eq t => t -> [t] -> Bool
member a list = foldr (||) False (map (== a) list)

union :: Ord t => [t] -> [t] -> [t]
union list1 list2 = foldr (juntar) [] ([list1]++[list2])

juntar :: (Ord t) => [t] -> [t] -> [t]
juntar [] l = l
juntar l [] = l
juntar (a:as) (b:bs)
	|(member a (b:bs)) == True = (b:(juntar as bs))
	|otherwise =(a:(juntar as (b:bs)))

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

inserirNo :: Ord t => t -> (Tree t) -> (Tree t)
inserirNo no NilT  = (Node no NilT NilT)
inserirNo no (Node raiz left right)
 | no < raiz = (Node raiz (inserirNo no left) right)
 | no >= raiz = (Node raiz left (inserirNo no right))


criarArvore :: Ord t => [t] -> (t -> Tree t -> Tree t) -> Tree t
criarArvore l f = foldr f NilT  l

mapp :: (t -> u) -> [t] -> [u]
mapp f l = [f a | a <- l]

folda :: (t -> u -> u) -> u -> [t] -> u
folda f b [] = b
folda f b (a:as) = f a (folda f b as)

mapFold :: (t -> u -> u) -> [u] -> [[t] -> u]
mapFold f l = mapp (folda  f) l

funcao :: [[t] -> u] -> [t] -> [u]
funcao [] _ = []
funcao (a:as) m = (a m) : (funcao as m)

--Slide 6

maxList :: [Int] -> Int
maxList = maximum

isomorficas :: Eq t => Tree t -> Tree t -> Bool
isomorficas NilT _ = False
isomorficas _ NilT = False
isomorficas (Node n l r) (Node n1 l1 r1)
 | l == NilT && l1 /= NilT || l1 == NilT && l /= NilT = False
 | r /= NilT && r1 == NilT || r == NilT && r1 /= NilT = False
 | otherwise = (isomorficas l l1) && (isomorficas r r1)

par :: (Eq t) => [t] -> [t] -> [(t,t)]
par [] _ = []
par _ [] = []
par (a:as) (b:bs) = (a, b) : par as bs

lista2Par :: (Eq t) => [t] -> ([t] -> [(t,t)])
lista2Par list = \x -> par list x

--Questao 2

data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)
data Tree2 t = NilT2 | Node2 t [Tree2 t] deriving (Eq, Show)

getAdjacent :: (Eq a) => [(a,a,Int)] -> a -> [a]
getAdjacent [] a = []
getAdjacent ((x,y,p):ls) a
	|x == a = (y:getAdjacent ls a)
	|y == a = (x:getAdjacent ls a)
	|otherwise =  getAdjacent ls a

adjacencias::(Eq a) => (Graph a) -> [a] -> a -> a -> (Tree2 a)
adjacencias (Graph l ls) pilha ini cheg 
	|elem ini pilha == True = NilT2
	|ini == cheg = (Node2 ini [NilT2]) 
	|otherwise = Node2 ini [(adjacencias (Graph l ls) (ini:pilha) x cheg )|x<-adjs]
		where
		adjs = getAdjacent ls ini

allPaths :: Tree2 a -> [[a]]
allPaths NilT2 = [[]]
allPaths (Node2 a ls) = [a:x | t <- ls, x <- allPaths t]

getDistance::(Eq a) =>[(a,a,Int)] -> a -> a -> Int
getDistance ((x,y,p):ls) a b 
	| (x == a) && (y == b) = p
	| (x == b) && (y == a) = p
	|otherwise = (getDistance ls a b)

allDistances :: (Eq a) =>[(a,a,Int)] -> [a] -> [Int]
allDistances ls (b:[]) = []
allDistances ls (a:b:rest) = ((getDistance ls a b):(allDistances ls (b:rest)))

distances ::(Eq a) => (Graph a) -> [[a]] -> a -> [([a],Int)]
distances (Graph l ls) paths cheg =  [(p,(foldr (+) 0 (allDistances ls p)))|p<-path]
	where path = filter (elem cheg) paths

menor :: [([a],Int)] -> ([a],Int) -> [a]
menor [] (l,dist) = l
menor ((lx,distx):ls) (l,dist)
	|distx < dist = menor ls (lx,distx)
	|otherwise = menor ls (l,dist)
	

shortest :: [([a],Int)] -> [a]
shortest [] = []
shortest ((l,dist):[]) = l
shortest ((l,dist):ls) = menor ls (l,dist)
	
geraFuncoMenorCaminho ::(Eq a) => (Graph a) -> (a->a->[a])
geraFuncoMenorCaminho graph = \x y ->shortest (distances graph (allPaths (adjacencias graph [] x y)) y) 
