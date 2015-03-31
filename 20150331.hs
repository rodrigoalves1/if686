--Trabalho 4 31/03/2015
-- 1ª Questão

-- 2º questão
lookAndSay :: Int -> [Char]
lookAndSay 0 = []
lookAndSay 1 = "1"
lookAndSay n = lookAndSayAux (lookAndSay (n-1))

lookAndSayAux :: [Char] -> [Char]
lookAndSayAux str
 | str == [] = []
 | otherwise = (show rep) ++ [(str !! 0)] ++ (lookAndSayAux (drop rep str))
 where rep = repetido str (str !! 0)

repetido :: [Char] -> Char -> Int
repetido [] c = 0
repetido (a:as) c
 | a /= c = 0
 | otherwise = 1 + repetido as c

 --3ª QUESTÃO

type Arestas = [Int]
type No = (Int,Arestas)
type Grafo = [No]
type Stack = [No]

meuGrafo :: Grafo

meuGrafo = [(1,[2,5]),(2,[1,3,5]),(3,[2,4]),(4,[3,5,6]),(5,[1,2,4]),(6,[4])]

pilha :: Stack
pilha = []

search :: Grafo -> Int -> Int -> [Int]
search grf a b
 | a == b = [b]
 | verificaAdjacencia (getAdj grf a) b == True = a : [b]
 | otherwise = [0]

getAdj :: Grafo -> Int -> [Int]
getAdj (a:as) b
 | (a:as) == [] = []
 | b == fst a = snd a
 | otherwise = getAdj as b

naoMarcado :: [Int] -> [Int] -> Int
naoMarcado [] (a:as) = a
naoMarcado marcado [] = 0
naoMarcado marcado (a:as)
 | elem a marcado = naoMarcado marcado as
 | otherwise = a

verificaAdjacencia :: Arestas -> Int -> Bool
verificaAdjacencia [] b = False
verificaAdjacencia (a:as) b
 | (a:as) == [] = False
 | b == a = True
 | b /= a = verificaAdjacencia as b
 | otherwise = False

dfs :: Grafo -> [Int] -> [Int] -> [Int]
dfs grafo marcado [] = reverse marcado
dfs grafo marcado (a:as) 
 | elem a marcado = dfs grafo marcado as --pega o proximo da lista de adj nao marcado
 | otherwise = dfs grafo (a:marcado) (getAdj grafo a) --marca o cara e chama a funcao passando os filhos dele

dfs2 :: Grafo -> [Int] -> Int -> [Int] -> [Int]
dfs2 grafo1 marcado1 no [] = reverse marcado1
dfs2 grafo1 marcado1 no (a:as) 
 | no == 0 = dfs2 grafo1 marcado1 (naoMarcado marcado1 (getAdj grafo1 (head as)) as  -- se chegou numa folha ou num cara q ja tem todos os vizinhos marcados, da um pop na pilha e chama novamente a funcao
 | no /= 0 = (dfs2 grafo1 (no:marcado1) (naoMarcado marcado1 (getAdj grafo1 no)) (no:marcado1)) -- marca o cara, coloca ele na pilha e chama a funcao com os adjacentes a ele
 | otherwise = []


--dfs meuGrafo [1] (getAdj 1) [1] = dfs meuGrafo [1] [2,5]
-- dfs meuGrafo [2,1] getAdj 2 = dfs meuGrafo [2,1] [1,3,5]
-- dfs meuGrafo [2,1] [1,3,5] - 1 ta marcado daí: dfs meuGrafo [2,1] [3,5]
-- dfs meuGrafo [3,2,1] [2,4] = 2 ta marcado daí: dfs meuGrafo [3,2,1] [4]
-- dfs meuGrafo [4,3,2,1] [3,5,6] = 3 ta marcado dai dfs meuGrafo [5,6]
-- dfs meu grafo [5,4,3,2,1] [1,4] = 1 ta marcado dai dfs meu grafo [5,4,3,2,1] [4]
-- 4 ta marcado dai dfs meu grafo [5,4,3,2,1] []




--4ª Questão
