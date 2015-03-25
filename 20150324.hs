--mergesort / heapsort
--REFERENTE AO TRABALHO 2
--mergesort
--A complexidade do firstHalf e do secondHalf é n/2, do size é n, do merge é n (sendo n o tamanho da maior lista), por sua vez o mergesort tem complexidade n log n 
size :: [Int] -> Int
size [] = 0
size (a:as) = 1 + size as

firstHalf :: [Int] -> Int -> [Int]
firstHalf l 0 = []
firstHalf (a:as) n = [a]++firstHalf as (n-1)

secondHalf :: [Int] -> Int -> [Int]
secondHalf l 0 = l
secondHalf (a:as) n = secondHalf as (n-1)

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (a:as) (b:bs)
	|a < b = (a:merge as (b:bs))
	|otherwise = (b:merge (a:as) bs)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort (firstHalf l ((size l)`div`2))) (mergesort (secondHalf l ((size l)`div`2)))


--heapsort
--o heapsort tem a complexidade O(n log n) no pior caso.

trocar :: Int -> Int -> [Int] -> [Int]
trocar i j xs 
 | i == j    = xs
 | i < j = firstElements xs i ++ ( get xs j) : firstElements (lastElements xs (i+1)) (j-i-1) ++ (get xs i) : lastElements xs (j+1)
 | otherwise = firstElements xs j ++ ( get xs i) : firstElements (lastElements xs (j+1)) (i-j-1) ++ (get xs j) : lastElements xs (i+1)
 
maior :: Int -> Int -> [Int] -> Int
maior i hs xs -- decide o maior entre o pai e seus dois filhos e retorna o indice do maior
 | ((2 * i + 1) < hs) && ((get xs (2 * i + 1)) > (get xs i)) = maiorAux (2 * i + 1) i hs xs -- se o filho 'pertence' a lista e ele é maior que o pai entao deve trocar 
 | otherwise = maiorAux i i hs xs -- se nao o pai é maior
        
 
maiorAux :: Int -> Int -> Int -> [Int] -> Int -- a partir da comparacao do filho da esquerda e o pai compara o maior deles com o filho da direita e retorna decisao final
maiorAux b i hs xs -- decide o maior entre o pai e seus dois filhos e retorna o indice do maior
 | ((2 * i + 2) < hs) && ((get xs (2 * i + 2)) > (get xs b)) = (2 * i + 2) -- se o filho 'pertence' a lista e ele é maior que o pai entao deve trocar 
 | otherwise = b

heapify :: Int -> Int -> [Int] -> [Int]
heapify i hs lista  
 | ((maior i hs lista) /= i) = heapify (maior i hs lista) hs (trocar (maior i hs lista) i lista)
 | otherwise = lista
        
 
buildheap :: Int -> [Int] -> [Int]
buildheap 0 lista = heapify 0 (size lista) lista --chegou na raiz, daí retorna a nova lista com o raiz do heap no final da lista
buildheap n lista = buildheap (n - 1) (heapify n (size lista) lista) -- recursivamente vai comparando o valor dos filhos

heapsort :: [Int] -> [Int]
heapsort lista = heapsortAux (size lista - 1) (buildheap (size lista `div` 2) lista) -- começa o algoritmo no meio da lista
              
heapsortAux :: Int -> [Int] -> [Int]
heapsortAux i lista
 | i /= 1 = heapsortAux (i - 1) (heapify 0 i (trocar 0 i lista)) -- 
 | otherwise = (heapify 0 i (trocar 0 i lista))
 

get :: [Int] -> Int -> Int
get (a:as) 0 = a
get (a:as) n = get as (n-1)

firstElements :: [Int] -> Int -> [Int]
firstElements list 0 = []
firstElements (a:as) n = [a] ++ firstElements as (n-1)

lastElements :: [Int] -> Int -> [Int]
lastElements list 0 = list
lastElements (a:as) n = lastElements as (n-1)


--CÓDIGO ABAIXO É REFERENTE A AULA!!!
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (a:as) = (quicksort [y | y <- as, y < a])
                       ++ [a] ++
                      (quicksort [y | y <- as, y >= a])

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (smaller, bigger) 
 where smaller = menor a b c
       bigger = maioor a b c
 
maioor :: Int -> Int -> Int -> Int
maioor a b c
 | a > b = maioorAux a c
 | otherwise = maioorAux b c
 
maioorAux :: Int -> Int -> Int
maioorAux a b
 | a > b = a
 | otherwise = b
 
menor :: Int -> Int -> Int -> Int
menor a b c
 | a < b = menorAux a c
 | otherwise = menorAux b c
 
menorAux :: Int -> Int -> Int
menorAux a b
 | a < b = a
 | otherwise = b

ordenaTripla :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTripla (a,b,c) = ordenaTriplaAux [a,b,c]

ordenaTriplaAux :: [Int] -> (Int,Int,Int)
ordenaTriplaAux list = (get lista 0,get lista 1, get lista 2)
 where lista = mergesort list

type Ponto = (Float,Float)
type Reta = (Ponto,Ponto)

firstPoint :: Reta -> Ponto
firstPoint (a,b) = a

secondPoint :: Reta -> Ponto
secondPoint (a,b) = b

vertical :: Reta -> Bool
vertical ((x1,x2),(x3,x4))
 | x1 == x3 = True
 | otherwise = False
 
pontoY :: Float -> Reta -> Float
pontoY x ((x1,y1),(x2,y2)) = ((y2-y1)*(x-x1))/(x2-x1) + (y1*(x2-x1))/(x2-x1)

--type String = [Char]
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Andre","Duna"),("Fernando","Jonathan Strange & Mr. Norrel"),("Fernando","A Game of Thrones")] -- livros emprestados
--livros ls pp = [l| (p,l) <- ls, pp == p]
livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros ((pe,l):as) p
 | pe == p = l : livros as p
 | otherwise = livros as p

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos ls li = [p | (p,l) <- ls, li == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado ls li = head [True | (p,l) <- ls, li == l]

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos ls pe = size [1 | (p,l) <- ls, pe == p]

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar ls p l = (p,l) : ls

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver ls pe li = [(p,l) | (p,l) <- ls , (pe,li) /=(p,l) ]
