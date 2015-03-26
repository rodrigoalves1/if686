--trabalhos

--tabela hash
type Chave = Int
type Codigo = Int
type Tupla = (Chave,Codigo)
type HashTable = [Tupla]

search :: HashTable -> Chave -> Int -> Int
search l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = search l key 0
	| fst (l !! n) == key = n
	| otherwise = search l key (n+1)

freeSpace :: HashTable -> Chave-> Int -> Int
freeSpace l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = freeSpace l key 0
	| fst (l !! n) == (-1) = n
	| otherwise = freeSpace l key (n+1)

removeElement :: HashTable -> Int -> HashTable
removeElement l (-1) = l
removeElement [] n = []
removeElement (a:as) n
	|n == 0 = (((-1),(-1)):(removeElement as (n-1)))
	|otherwise = (a:(removeElement as (n-1)))

setElement :: HashTable -> Int -> Chave -> Codigo -> HashTable
setElement l (-1) key codigo = l
setElement [] n key codigo = []
setElement (a:as) n key codigo
	|n == 0 = ((key,codigo):(setElement as (n-1) key codigo))
	|otherwise = (a:(setElement as (n-1) key codigo))

get :: HashTable -> Chave -> Int
get [] key = (-1)
get hash key
	|fst (hash !! (key `mod` (length hash))) == key = (key `mod` (length hash))
	|otherwise = search hash key ((key `mod` (length hash))+1)

put :: HashTable -> Chave -> Codigo -> HashTable
put [] key codigo = []
put hash key codigo
	|fst (hash !! (key `mod` (length hash))) == (-1) = setElement hash (key `mod` (length hash)) key codigo
	|otherwise = setElement hash (freeSpace hash key ((key `mod` (length hash))+1)) key codigo

remove :: HashTable -> Chave -> HashTable
remove [] key = []
remove hash key = removeElement hash (get hash key)

hasKey :: HashTable -> Chave -> Bool
hasKey [] key = False
hasKey hash key
	|get hash key == (-1) = False
	|otherwise = True

--comparando conjunto

quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

elimRep :: (Ord t) => [t] -> [t]
elimRep [] = []
elimRep [a] = [a]
elimRep (a:a2:as)
	|a == a2 = (elimRep (a2:as))
	|otherwise = (a:elimRep (a2:as))

contem :: (Ord t) => [t] -> [t] -> Bool
contem [] [] = True
contem [] l = True
contem l [] = False
contem (a:as) (b:bs)
	|a /= b = contem (a:as) bs
	|otherwise = contem as bs

igual :: (Ord t) => [t] -> [t] -> Bool
igual [] [] = True
igual [] l = False
igual l [] = False
igual (a:as) (b:bs)
	|a /= b = False
	|otherwise = igual as bs

existe ::(Ord t) => t -> [t] -> Bool
existe n [] = False
existe n (a:as)
	|n == a = True
	|otherwise = existe n as
	
intercesiona :: (Ord t) => [t] -> [t] -> Bool
intercesiona [] [] = False
intercesiona [] l = False
intercesiona l [] = False
intercesiona (a:as) (b:bs)
	|a == b = True
	|existe a bs == True = True
	|existe b as == True = True 
	|otherwise = False
	
comparaConjuntos :: (Ord t) => [t] -> [t] -> String
comparaConjuntos a b
	|contem (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "B contem A"
	|contem (elimRep (quicksort b)) (elimRep (quicksort a)) == True = "A contem B"
	|igual (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "A igual B"
	|intercesiona (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "A intercesiona B"
	|otherwise = "Conjuntos disjuntos" 
