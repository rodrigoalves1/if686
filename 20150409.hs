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


--passar nos e adj ordenados
--esta funcao passa o primeiro no para fazer uma busca em profundidade, caso o no a seja elemento do resultado da busca retorna True, do contrario 
--retorna False
dfs :: (Ord a) => (Graph a) -> a -> Bool
dfs (Graph nos adj) a = elem a (dfsAux (Graph nos adj) [nos !! 0] (naoMarcado [nos !! 0] (getAdj adj (nos !! 0))) [nos !! 0]  )


--parametros: grafo,lista de nos marcados, no nao marcado a ser adicionado, pilha
dfsAux :: (Ord a) => (Graph a) -> [a] -> [a] -> [a] -> [a]
dfsAux _ marcado [] (a:[]) = reverse marcado
dfsAux (Graph nos adj) marcado (b:bs) [] = reverse marcado
dfsAux (Graph nos adj) marcado [] (a:as) = dfsAux (Graph nos adj) marcado (naoMarcado marcado (getAdj adj (head as))) as
dfsAux (Graph nos adj) marcado (b:bs) (a:as) = dfsAux (Graph nos adj) (b:marcado) (naoMarcado (b:marcado) (getAdj adj b)) (b:marcado)


--Exercícios na sala de aula
