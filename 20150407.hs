data Mes = Janeiro | Fevereiro | Outubro
isAniversarioCastor :: Mes -> Bool
isAniversarioCastor Outubro = True
isAniversarioCastor _ = False

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = (2 * pi * r)
area (Rectangle m n) = m * n

data Dia = Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado | Domingo

{-
isWeekend :: Dia -> Bool
isWeekend d = if d == (Sabado) || d == ( Domingo) then True else False
-}
horario :: [Dia]
horario = [Segunda 6 ["Redes","TAI","Ingles"],Terca 4 ["Compiladores","PLC"],Quarta 4 ["TAI","Redes"],Quinta 2 ["PLC"],Sexta 2 ["PLC"]]

isWeekend :: Dia -> Bool
isWeekend Sabado = True
isWeekend Domingo = True
isWeekend _ = False

hasPLC :: Dia -> Bool
hasPLC Sabado = False
hasPLC Domingo = False
hasPLC (Segunda _ []) = False
hasPLC (Segunda h (a:as))
 | a == "PLC" = True
 | otherwise = hasPLC (Segunda h as)
hasPLC (Terca _ []) = False
hasPLC (Terca h (a:as))
 | a == "PLC" = True
 | otherwise = hasPLC (Terca h as)
hasPLC (Quarta _ []) = False
hasPLC (Quarta h (a:as))
 | a == "PLC" = True
 | otherwise = hasPLC (Quarta h as)
hasPLC (Quinta _ []) = False
hasPLC (Quinta h (a:as))
 | a == "PLC" = True
 | otherwise = hasPLC (Quinta h as)
hasPLC (Sexta _ []) = False
hasPLC (Sexta h (a:as))
 | a == "PLC" = True
 | otherwise = hasPLC (Sexta h as)
 
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
 
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


showExpr :: Expr -> String
showExpr (Lit n) =   show n 
showExpr (Add e1 e2) =  "("++showExpr e1++")" ++" + "++ "("++showExpr e2++")"
showExpr (Sub e1 e2) =  "("++showExpr e1++")" ++" - "++ "("++showExpr e2++")"

data List t = Nil | Cons t (List t)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq,Show)
-- toList (Cons 2 (Cons 1(Nil)))

toList :: List t -> [t]
toList Nil = []
toList (Cons t x) = (t:toList x)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)




