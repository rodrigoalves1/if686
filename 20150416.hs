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
