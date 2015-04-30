data Failable t = Error String | Value t 

instance Monad Failable where
(>>=) (Value x) f = f x
(>>=) (Error x) _ = (Error x)
return x = (Value x)

data Fila u = NilF | Fila (Int,[u])

criarFila :: Show t => Int -> t -> Failable (t, Fila t)
criarFila 0 _ = Error "Fila deve conter 1 elemento"
criarFila i u = Value (u,Fila (i,[u]))
