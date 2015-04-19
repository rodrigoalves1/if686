--Trabalho 08
listPartitioner :: Ord a => [a] -> ([a]->[[a]])
listPartitioner list = (\x -> dividir (quicksort list) x)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (a:as) = (quicksort [y | y <- as, y < a])
                       ++ [a] ++
                      (quicksort [y | y <- as, y >= a])

dividir :: (Ord a) => [a] -> [a] -> [[a]] 
dividir (a:[]) list = [filter (<= a) list] ++[filter (> a) list]
dividir (a:as) list = (filter (<= a) list) : (dividir as (filter (> a) list))

-- Exercícios da aula
f :: t -> u -> v
f = \x y -> f y x

-- Dada uma lista de pares, devolver uma lista contendo apenas os primeiros elementos de cada par

a = \l -> [b | (b,c) <- l]

--Dados uma lista de listas de números e um número n, devolver uma lista contendo todas cujo comprimento seja maior que n

b = \l n -> [ b | b <- l , (length b) > n]

--Dada uma lista de listas, criar uma lista que contém todos os elementos das sub-listas da lista de entrada, mas removendo duplicação:
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (a:as) = (quicksort [y | y <- as, y < a])
                       ++ [a] ++
                      (quicksort [y | y <- as, y >= a])
-- [a | b <- l, a <- b]					  
c = \l -> remove (quicksort (foldr (++) [] l))

remove :: [Int] -> [Int]
remove l
 | l == [] = []
 | otherwise = [(l !! 0)] ++ (remove (drop rep l))
 where rep = repetido l (l !! 0)



repetido :: [Int] -> Int -> Int
repetido [] c = 0
repetido (a:as) c
 | a /= c = 0
 | otherwise = 1 + repetido as c
 
 --map.filter
mapp :: (t -> u) -> [t] -> [u]
mapp f l = [f a | a <- l]
 
filtro :: (t -> Bool) -> [t] -> [t]
filtro f l = [a | a <- l, f a]
 
mapFilter :: (t -> Bool) -> [[t]] -> [[t]]
mapFilter f l = mapp (filtro f) l
 --map.foldr
 --foldrr ::


--Somar uma constante x a todos os elementos de uma lista de números 
somaX :: Int -> Int -> Int 
somaX x y = x + y

somaConstante ::  Int -> [Int] -> [Int]
somaConstante x = map (somaX x)

--Dada uma lista de números, obter o maior da lista

maior :: [Int] -> Int
maior = max
