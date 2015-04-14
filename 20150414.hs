--Trabalho 7
--Questão 1

compose :: (t -> t) -> [(t -> t)] -> t -> [( t -> t)]
compose f fn a = [(composeAux f fx a)|fx <- fn]

composeAux :: (t->t) -> (t->t) -> t -> t
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

filterTree :: (Ord t) => (t -> Bool) -> (Tree t) -> [(Tree t)]
filterTree _ NilT = []
