--Trabalho 11
import Data.Char (ord)
import Data.Char (toUpper)

--Questão 1

type Chave = Int
type Codigo = Int
type Tupla = (Chave,Codigo)
type HashTable = [Tupla]

search :: HashTable -> Chave -> Int -> Maybe Int
search l key n
	| n == (key `mod` (length l)) = Nothing
	| n == (length l) = (search l key 0)
	| fst (l !! n) == key = Just n
	| otherwise = (search l key (n+1))

freeSpace :: HashTable -> Chave-> Int -> Int
freeSpace l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = freeSpace l key 0
	| fst (l !! n) == (-1) = n
	| otherwise = freeSpace l key (n+1)

removeElement :: HashTable -> Maybe Int -> Maybe HashTable
removeElement l Nothing = Nothing
removeElement [] (Just n) = Just []
removeElement (a:as) (Just n)
	|n == 0 = Just (((-1),(-1)):x)
	|otherwise = Just (a:x)
	where Just x = (removeElement as (Just (n-1)))

setElement :: HashTable -> Int -> Chave -> Codigo -> HashTable
setElement l (-1) key codigo = l
setElement [] n key codigo = []
setElement (a:as) n key codigo
	|n == 0 = ((key,codigo):(setElement as (n-1) key codigo))
	|otherwise = (a:(setElement as (n-1) key codigo))

get :: HashTable -> Chave -> Maybe Int
get [] key = Nothing
get hash key
	|fst (hash !! (key `mod` (length hash))) == key = Just (key `mod` (length hash))
	|otherwise = search hash key ((key `mod` (length hash))+1)

put :: HashTable -> Chave -> Codigo -> Maybe HashTable
put [] key codigo = Nothing
put hash key codigo
	|fst (hash !! (key `mod` (length hash))) == (-1) = Just (setElement hash (key `mod` (length hash)) key codigo)
	|otherwise = Just (setElement hash (freeSpace hash key ((key `mod` (length hash))+1)) key codigo)

remove :: HashTable -> Chave -> Maybe HashTable
remove [] key = Nothing
remove hash key = (removeElement hash (get hash key))

hasKey :: HashTable -> Chave -> Bool
hasKey [] key = False
hasKey hash key
	|c == (-1) = False
	|otherwise = True
	where Just c = get hash key


main1 = (put [((-1),(-1)),((-1),(-1)),((-1),(-1)),((-1),(-1)),((-1),(-1))] 7 33) >>= (\a -> put a 4 67) >>= (\b -> remove b 7) >>= \c -> put c 2 11 >>= \d -> put d 3 22 >>= \e -> remove e 4 >>= \c -> put c 2 33 >>= \d -> put d 3 43 >>= \e -> remove e 3 >>= \f -> put f 8 66 >>= \g -> put g 5 37 >>= \h -> remove h 2 >>= \i -> put i 1 54 >>= \j -> put j 45 32 >>= \k ->remove k 4



--Questão 2

filtragem :: String -> String -> IO (Maybe String)
filtragem [] orig	 = return (Just orig)
filtragem (a:as) orig
 | not (a `elem` ['A'..'Z']) && not (a `elem` ['a'..'z']) && a /= ' ' = return Nothing
 | otherwise = filtragem as orig

toUper :: Maybe String -> IO (Maybe String)
toUper Nothing = return Nothing
toUper (Just []) = return (Just [])
toUper (Just str) = return (toUperAux str)

toUperAux :: String -> Maybe String
toUperAux [] = Just []
toUperAux (a:as) =  Just ((toUpper a) : b)
 where Just b = toUperAux as

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

splitWords ::  String -> [Word]
splitWords [] = []
splitWords (a:as)
 | a == ' ' = splitWords (dropSpace (a:as))
 | otherwise = getWord (a:as) : splitWords (dropWord (a:as))

split :: Maybe String -> IO [String]
split Nothing = return []
split (Just []) = return []
split (Just str) = return (splitWords str)

main :: IO()
main = putStrLn "Digite uma palavra" >>  getLine >>= (\str -> filtragem str str) >>= (\a -> toUper a) >>= (\b -> split b) >>= (\c -> mapM_ putStrLn c) 
--mudança pro git achar q tem algo diferente
