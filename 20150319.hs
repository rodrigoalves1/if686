double :: [Int] -> [Int]

double list
 | list == [] = list
 | otherwise = (head list) * 2 : double (tail list) 
 
member :: [Int] -> Int -> Bool
 
member list n
 | list == [] = False
 | head list == n = True
 | otherwise = member (tail list) n
 
 
 
digits :: String -> String

digits str
 | str == [] = [] 
 | head str == '0' = '0' : digits (tail str)
 | head str == '1' = '1' : digits (tail str)
 | head str == '2' = '2' : digits (tail str)
 | head str == '3' = '3' : digits (tail str)
 | head str == '4' = '4' : digits (tail str)
 | head str == '5' = '5' : digits (tail str)
 | head str == '6' = '6' : digits (tail str)
 | head str == '7' = '7' : digits (tail str)
 | head str == '8' = '8' : digits (tail str)
 | head str == '9' = '9' : digits (tail str)
 | otherwise = digits (tail str)
 
sumPairs :: [Int] -> [Int] -> [Int]
 
sumPairs list1 list2
 | list1 == [] = list2
 | list2 == [] = list1 
 | otherwise =  ((head list1) + (head list2)) : sumPairs (tail list1) (tail list2)
