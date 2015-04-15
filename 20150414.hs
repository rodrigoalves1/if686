--Trabalho 7
--Questão 1

compose ::  (Int -> Int) -> [(Int -> Int)] -> [( Int -> Int)]
compose f fn = [(composeAux f fx) | fx <- fn]

composeAux :: (Int -> Int) -> (Int -> Int) -> Int -> Int
composeAux f1 f2 a = f1 (f2 a)

--Questão 2
data Graph t = Graph [t] [(t, t, Int)] deriving (Show, Eq, Ord)

mapGraph :: (t -> t) -> (Graph t) -> (Graph t)
mapGraph _ (Graph [] [] ) =  (Graph [] [])
mapGraph f (Graph node edge) = (Graph no aresta)
 where no = [(f a)| a <- node]
       aresta = [((f b),(f b),c) | (a,b,c) <- edge]
	   
foldGraph :: (t -> t -> t) -> (Graph t) -> t -> t
foldGraph f (Graph [] _ ) def =  def
foldGraph f (Graph (a:as) l) def =  f a (foldGraph f (Graph as l) def)


--Questão 3

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

filterTree :: (Eq t) => (t->Bool) -> (Tree t) -> [(Tree t)]
filterTree _ NilT = []
filterTree func node = [x]++(allList func xs)
	where (x,xs) = filterTreeAux2 func node

allList ::(Eq t) => (t->Bool) -> [(Tree t)] -> [(Tree t)]
allList _ [] = []
allList func (a:as)
	|x /= NilT = (x:(allList func as))++(allList func xs)
	|otherwise = (allList func as)++(allList func xs)
	where (x,xs) = filterTreeAux2 func a

filterTreeAux2 :: (Eq t)=> (t->Bool) -> (Tree t) -> ((Tree t),[(Tree t)])
filterTreeAux2 _ NilT = (NilT, [])
filterTreeAux2 func (Node v n1 n2)
	| func v == False && n1 == NilT && n2 == NilT = (NilT, [])
	| func v == False && n1 == NilT = (NilT, [n2])
	| func v == False && n1 == NilT = (NilT, [n1])
	| func v == False = (NilT, [(n1),(n2)])
	| otherwise =  (Node v x y,xs++ys)
	where 
		(x,xs) = filterTreeAux2 func n1
		(y,ys) = filterTreeAux2 func n2



--Exercicios de aula

filterList :: [[Int]] -> Int -> [[Int]]
--filterList ln a = [ l | l <- ln , (foldr (+) 0 l) < a ] 
filterList list c = filter isSmaller list
 where isSmaller x = (foldr (+) 0 x) < c

filterr l n = filter (\x -> (foldr (+) 0 x) >= n) l
--(>= n) . (foldr (+) 0) x

inter :: [Int] -> [Int] -> [Int]
inter l1 l2 = filter isMemberL2 l1
 where isMemberL2 l = elem l l2

diff :: [Int] -> [Int] -> [Int]
diff l1 l2 = filter isNotMemberL2 l1
 where isNotMemberL2 l = not (elem l l2)

mapFilter :: (t -> Bool) -> [[t]] -> [[t]]
mapFilter f list = map (filter f) list




