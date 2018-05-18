--1
descompacta :: [(a, b)] -> ([a], [b])
descompacta xs = foldr (\(a, b) (xs, ys) -> (a:xs, b:ys)) ([],[]) xs 

--2
preco = [("Leite", 2.0), ("Manteiga", 2.5), ("Batata", 4.0), ("Brocolis", 2.0), ("Cenoura", 2.2)]

pegarPreco _ [] = 0
pegarPreco i ((item, d): xs) | i == item = d
			     | otherwise = pegarPreco i xs

type Item = String
total :: [(Item, Double)] -> [Item] -> Double
total xs ys = foldr (\i a -> a + (pegarPreco i xs)) 0 ys

--3
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) | x == y = isPrefix xs ys  
		       | otherwise = False

subLista :: Eq a => [a] -> [a] -> Bool
subLista [] _ = True
subLista xs [] = False
subLista xs (y:ys) | isPrefix xs (y:ys) = True
		   | otherwise = subLista xs ys

--4
data Prop = And Prop Prop | Or Prop Prop | Not Prop | Val Bool
prop1 = (And (Val True) (Or (Val False) (Val True)))
prop2 = (Not (Or (Val True) (Val False)))

eval :: Prop -> Bool
eval (Val b) = b
eval (And a b) = (eval a) && (eval b) 
eval (Not p) = not (eval p)
eval (Or a b) = (eval a) || (eval b)

--5
data (Arvore a) = Folha a | Ramo (Arvore a) (Arvore a)
arv :: Arvore Int
arv = (Ramo (Ramo (Folha 6) (Ramo (Ramo (Folha 5) (Folha 4)) (Folha 3))) (Ramo (Folha 1) (Folha 2)))


foldTree :: (a->b) -> (b -> b -> b) -> Tree a -> b
--foldTree (Folha a) = 

data ArvBin a = Vazia | No a (ArvBin a) (ArvBin a)
deriving (Eq , Show)
arv1 = No 4 (No 3 Vazia Vazia) (No 5 (No 6 Vazia Vazia) (No 4 Vazia Vazia))
arv2 = No 7 (No 4 (No 5 Vazia Vazia) Vazia) (No 3 (No 2 Vazia Vazia) Vazia)
