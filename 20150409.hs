import Data.Char 
--Trabalho
--Questão 1
       --       lista nós / lista arestas        
data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)

quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

quicksortTupla :: (Ord t) => [(t,t,Int)] -> [(t,t,Int)]
quicksortTupla [] = []
quicksortTupla (p:ls) = quicksortTupla([r|r<-ls, r<=p])++[p]++quicksortTupla([t|t<-ls, t>p])

compareList :: (Ord a) => [a] -> [a] -> Bool
compareList [] [] = True
compareList (a:as) (b:bs)
	|a == b = compareList as bs
	|otherwise = False

compareListTupla :: (Ord a) => [(a,a,Int)] -> [(a,a,Int)] -> Bool
compareListTupla [] [] = True
compareListTupla ((a1,a2,p1):as) ((b1,b2,p2):bs)
	| a1 == b1 && a2 == b2 && p1 == p2 = compareListTupla as bs
	|otherwise = False

equals :: (Ord a) => (Graph a) -> (Graph a) -> Bool
equals (Graph l1 la1) (Graph l2 la2)
	|compareList (quicksort l1) (quicksort l2) == True = compareListTupla (quicksort la1) (quicksort la2)
	|otherwise = False

--Questão 2

--data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)
--meuGrafo :: Graph Int
--[(2,[1,3,5]),(3,[2,4]),(4,[3,5,6]),(5,[1,2,4]),(6,[4])]
meuGrafo = Graph [1,2,3,4,5,6] [(1,2,1),(1,5,1),(2,1,1),(2,3,1),(2,5,1),(3,2,1),(3,4,1),(4,3,1),(4,5,1),(4,6,1),(5,1,1),(5,2,1),(5,4,1),(6,2,1)]
--recebe uma lsita de arestas de um grafo, um nó e retorna a lista de nós adjacentes a ele
getAdj :: (Ord a) => [(a,a,Int)] -> a -> [a]
getAdj []  b = []
getAdj ((a1,a2,p1):as) b
 | ((a1,a2,p1):as) == [] = []
 | b == a1 = a2 : getAdj as b
 | otherwise = getAdj as b

--recebe lista de nos marcados e lista de adjacencia de um nó e retorna [] se nao existe nó disponivel ou uma lista com um elemento caso exista
naoMarcado :: (Ord a) => [a] -> [a] -> [a]
naoMarcado [] (a:as) = [a]
naoMarcado marcado [] = []
naoMarcado marcado (a:as)
 | elem a marcado = naoMarcado marcado as
 | otherwise = [a]

 --data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)
--passar nos e adj ordenados
--esta funcao passa o primeiro no para fazer uma busca em profundidade, caso o no a seja elemento do resultado da busca retorna True, do contrario 
--retorna False
dfs :: (Ord a) => (Graph a) -> a -> Bool
--dfs (Graph nos adj) a = if elem a nos == False then False
dfs (Graph nos adj) a = elem a (dfsAux (Graph nos adj) [nos !! 0] (naoMarcado [nos !! 0] (getAdj adj (nos !! 0))) [nos !! 0]  )

----dfs2 meuGrafo [1] (naoMarcado [1] (getAdj meuGrafo 1)) [1]
--parametros: grafo,lista de nos marcados, no nao marcado a ser adicionado, pilha
dfsAux :: (Ord a) => (Graph a) -> [a] -> [a] -> [a] -> [a]
dfsAux _ marcado [] (a:[]) = reverse marcado
dfsAux (Graph nos adj) marcado (b:bs) [] = reverse marcado
dfsAux (Graph nos adj) marcado [] (a:as) = dfsAux (Graph nos adj) marcado (naoMarcado marcado (getAdj adj (head as))) as
dfsAux (Graph nos adj) marcado (b:bs) (a:as) = dfsAux (Graph nos adj) (b:marcado) (naoMarcado (b:marcado) (getAdj adj b)) (b:marcado)
 {-| (b:bs) == [] && as /= [] = dfsAux (Graph nos adj) marcado (naoMarcado marcado (getAdj adj (head as))) as -- se chegou numa folha ou num cara q ja tem todos os vizinhos marcados, da um pop na pilha e chama novamente a funcao
 | (b:bs) /= [] = dfsAux (Graph nos adj) (b:marcado) (naoMarcado (b:marcado) (getAdj adj b)) (b:marcado)  -- marca o cara, coloca ele na pilha e chama a funcao com os adjacentes a ele 
 | otherwise = []
 -}
--dfsAux meuGrafo [1] (naoMarcado [1] (getAdj [(1,2,1),(1,5,1),(2,1,1),(2,3,1),(2,5,1),(3,2,1),(3,4,1),(4,3,1),(4,5,1),(4,6,1),(5,1,1),(5,2,1),(5,4,1)] (1))) [1]


--Exercícios na sala de aula

mapp :: (t -> u) -> [t] -> [u]
mapp f [] = []
mapp f (a:as) = map f (a:as)

posicao :: Char -> Int
posicao c = (ord c) - 96

posicaoAlfabeto :: (t -> Int) -> [t] -> [Int]
posicaoAlfabeto f [] = []
posicaoAlfabeto f list = (map f list)
 
somaString :: [String] -> [Int]
somaString (a:as) = posicaoAlfabeto (+) [(foldr (+) 0 (posicaoAlfabeto (posicao) (a:as)))]

--map fold map
mappp :: (t -> u) -> [t] -> [u]
mappp f list = [ (f l) | l <- list] 

member :: Eq t => t -> [t] -> Bool
member a list = foldr (||) False (map (== a) list)


repetido :: Eq t => [t] -> t -> Int
repetido [] t = 0
repetido (a:as) b
 | a == b = 1 + repetido as b
 | otherwise = repetido as b
{-
union :: Eq t => [t] -> [t] -> [t]
union list1 list2 = foldr (++) [] (unir list1 list2)
 
 
unir :: Eq t => [t] -> [t] -> [t]
unir [] (b:bs)  = b : unir [] bs
unir (a:as) []  = a : unir [] as
unir [] []  = []
unir (a:as) (b:bs) 
 | a == b  = a : unir as bs 
 | a /= b  = a : b : unir as bs 
 | otherwise = unir as bs 
 -}
 
