--Trabalho 4 31/03/2015

-- 1ª Questão
-- O polimorfismo de sobrecarga no Haskell permite somente que a pessoa defina uma função usando pattern matchings diferentes, mas nunca com numero de parâmetros diferentes ou tipos diferentes como no Java,
-- desta forma no Haskell erros são apontados em tempo de compilação e no Java erros em tempo de execução isso é uma vantagem. Porém torna o código menos flexível.

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
 | no == 0 = dfs2 grafo1 marcado1 (naoMarcado marcado1 (getAdj grafo1 (head as))) as -- se chegou numa folha ou num cara q ja tem todos os vizinhos marcados, da um pop na pilha e chama novamente a funcao
 | no /= 0 = dfs2 grafo1 (no:marcado1) (naoMarcado marcado1 (getAdj grafo1 no)) (no:marcado1)  -- marca o cara, coloca ele na pilha e chama a funcao com os adjacentes a ele
 | otherwise = []

--dfs2 meuGrafo [1] (naoMarcado [1] (getAdj meuGrafo 1)) [1]
--dfs meuGrafo [1] (getAdj 1) [1] = dfs meuGrafo [1] [2,5]
-- dfs meuGrafo [2,1] getAdj 2 = dfs meuGrafo [2,1] [1,3,5]
-- dfs meuGrafo [2,1] [1,3,5] - 1 ta marcado daí: dfs meuGrafo [2,1] [3,5]
-- dfs meuGrafo [3,2,1] [2,4] = 2 ta marcado daí: dfs meuGrafo [3,2,1] [4]
-- dfs meuGrafo [4,3,2,1] [3,5,6] = 3 ta marcado dai dfs meuGrafo [5,6]
-- dfs meu grafo [5,4,3,2,1] [1,4] = 1 ta marcado dai dfs meu grafo [5,4,3,2,1] [4]
-- 4 ta marcado dai dfs meu grafo [5,4,3,2,1] []

--Questão 4

--existem vários tipos de fazer o filtro de mediana, uma forma é preenchendo as bordas com os números das bordas para poder pegar a janela
--com os pixels das bordas, outra forma é não fazer a transformação com os pixels da broda para poder pegar a janela dentro do frame,
--a terceira maneira, a usada aqui é preencher as bordas com zero para poder pegar a janela dos pixels da borda e fazer a transformação.

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana [] _ = []
filtroMediana l 0 = l
filtroMediana l janelaTam = filtroMedianaLin l janelaTam 1


--gera a matriz com os valores transformados
filtroMedianaLin :: [[Int]] -> Int -> Int -> [[Int]]
filtroMedianaLin  l janelaTam posx 
	|posx > length l = []
	|otherwise = ((filtroMedianaCol l janelaTam posx 1):(filtroMedianaLin l janelaTam (posx+1)))

filtroMedianaCol :: [[Int]] -> Int -> Int -> Int ->[Int]
filtroMedianaCol l janelaTam posx posy 
	|posy > length l = []
	|otherwise = ((mediana (quicksort (emLinha (vizinhos l lMaior janelaTam posx posy 0)))):(filtroMedianaCol l janelaTam posx (posy+1)))
	where lMaior = fazerListaMaior l l janelaTam 0 0


--pegar os vizinhos na matrix grande e transformar numa lista
emLinha :: [[Int]] -> [Int]
emLinha [] = []
emLinha (a:as) = a++(emLinha as) 


--cria uma matriz com os vixinhos
vizinhos :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
vizinhos _ [] _ _ _ _ = []
vizinhos l (a:as) janelaTam posx posy posxMaior
	|posxMaior < (posx-tamBorda) = vizinhos l as janelaTam posx posy (posxMaior+1)
	|posxMaior > (posx+tamBorda) = []
	|otherwise = ((vizinhosAux l a janelaTam posy 0):(vizinhos l as janelaTam posx posy (posxMaior+1)))
	where 
		tamBorda = janelaTam`div`2
vizinhosAux :: [[Int]] -> [Int] -> Int -> Int -> Int -> [Int]
vizinhosAux _ [] _ _ _ = []
vizinhosAux l (a:as) janelaTam posy posyMaior
	|posyMaior < (posy-tamBorda) = vizinhosAux l as janelaTam posy (posyMaior+1)
	|posyMaior > (posy+tamBorda) = []
	|otherwise = (a:(vizinhosAux l as janelaTam posy (posyMaior+1))) 
	where 
		tamBorda = janelaTam`div`2


--criar uma matriz maior, com as laterais com valor = 0
fazerListaMaior :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> [[Int]]
fazerListaMaior l [] janelaTam posx posxMaior
	|posxMaior >= (tam + tamBorda + tamBorda - 1) = [(fazerListaMaiorAux l [] janelaTam posxMaior 0 0)]
	|otherwise = ((fazerListaMaiorAux l [] janelaTam posxMaior 0 0):(fazerListaMaior l [] janelaTam posx (posxMaior+1)))
	where 
		tam = length l
		tamBorda = janelaTam`div`2
fazerListaMaior l (a:as) janelaTam posx posxMaior
	|posxMaior >= (tam + tamBorda + tamBorda - 1) = [(fazerListaMaiorAux l [] janelaTam posxMaior 0 0)]
	|posxMaior < tamBorda || posxMaior >= (tam+tamBorda)|| ((a:as) == []) = ((fazerListaMaiorAux l [] janelaTam posxMaior 0 0):(fazerListaMaior l (a:as) janelaTam posx (posxMaior+1)))
	|otherwise = ((fazerListaMaiorAux l a janelaTam posxMaior 0 0):(fazerListaMaior l as janelaTam (posx+1) (posxMaior+1)))
	where 
		tam = length l
		tamBorda = janelaTam`div`2
fazerListaMaiorAux :: [[Int]] -> [Int] -> Int -> Int -> Int -> Int -> [Int]
fazerListaMaiorAux l ls janelaTam posxMaior posy posyMaior 
	|posyMaior >= (tam + tamBorda + tamBorda) = []
	|posxMaior < tamBorda || posyMaior < tamBorda = (0:fazerListaMaiorAux l ls janelaTam posxMaior posy (posyMaior+1))
	|posxMaior >= (tam + tamBorda) || posyMaior >= (tam + tamBorda)  = (0:fazerListaMaiorAux l ls janelaTam posxMaior posy (posyMaior+1))
	|otherwise = ((ls!!posy):fazerListaMaiorAux l ls janelaTam posxMaior (posy+1) (posyMaior+1))
	where 
		tam = length l
		tamBorda = janelaTam`div`2


--calcular a mediana de uma lista
mediana :: [Int] -> Int
mediana [] = 0
mediana ls = medianaCalc (quicksort ls)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

medianaCalc :: [Int] -> Int
medianaCalc [] = 0
medianaCalc ls
	|odd (length ls) == True = ls !! (((length ls)`div`2))
	|otherwise = ((ls !! ((length ls)`div`2))+(ls !! (((length ls)`div`2)-1)))`div`2


--Exercícios em sala de aula 
--                             (atual est in)
--Prelude>afd “111” [1, 2, 3] [(1, 1, “1”), (1, 3, “0”), (3, 2, “1”)] 1 [2]


runAfd :: String -> [Int] -> [(Int,Int,String)] -> Int -> [Int] -> Bool
runAfd [] estados delta inicio aceita = elem inicio aceita
runAfd str estados delta inicio aceita = runAfd (drop 1 str) estados delta (novoEstado inicio [str !! 0] delta) aceita
 
novoEstado :: Int -> String -> [(Int,Int,String)] -> Int
novoEstado atual input [] = 0
novoEstado atual input ((a,b,c):as)
 | atual == a && input == c = b
 | otherwise = novoEstado atual input as

 
--somatorioHexadecimal :: [String] -> String
--somatorioHexadecimal str = hexadecimal sum (strToInt str [])

strToInt :: [String] -> [Int] -> [Int]
strToInt [] result = 0 : result
strToInt (a:as) result
 | a == "1" = strToInt as (1 : result)
 | a == "2" = strToInt as (2 : result)
 | a == "3" = strToInt as (3 : result)
 | a == "4" = strToInt as (4 : result)
 | a == "5" = strToInt as (5 : result)
 | a == "6" = strToInt as (6 : result)
 | a == "7" = strToInt as (7 : result)
 | a == "8" = strToInt as (8 : result)
 | a == "9" = strToInt as (9 : result)
 | a == "0" = strToInt as (0 : result)
 | a == "A" = strToInt as (10 : result)
 | a == "B" = strToInt as (11 : result)
 | a == "C" = strToInt as (12 : result)
 | a == "D" = strToInt as (13 : result)
 | a == "E" = strToInt as (14 : result)
 | a == "F" = strToInt as (15 : result)
