--Trabalho 11
import Data.Char (ord)
import Data.Char (toUpper)

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
