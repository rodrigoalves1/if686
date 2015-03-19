vendas :: Int -> Int
vendas n = 3

teste :: Int -> Int -> Int
teste n s
 | vendas n == s = 1 
 | otherwise = 0

func :: Int -> Int -> Int
func s n 
 | (n == 0) && (vendas 0) == s = 1
 | (n == 0) && (vendas 0) /= s = 0
 | otherwise func s (n - 1) + teste n s
		
		
		
