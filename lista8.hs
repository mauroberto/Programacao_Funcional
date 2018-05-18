--lista5

--questao12
potenciaIterada :: Int -> (a -> a) -> a -> a
potenciaIterada n f x = last (take (n+1) (iterate f x))

--questao14

--a
seguinte :: Integer -> Integer
seguinte n | n == 1 = 1
           | mod n 2 == 0 = div n 2
           | otherwise = (n * 3) + 1

--b
collatz :: Integer -> [Integer]
collatz n = takeWhile (/=1) (iterate seguinte n) ++ [1]

--AP 2
--1
data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

a0 = No 5 Vazia Vazia
a1 = No 1 Vazia Vazia
a2 = No 2 a1 Vazia
a3 = No 3 a2 a0

a4 = No 9 Vazia Vazia
a5 = No 28 Vazia Vazia
a6 = No 50 Vazia Vazia
a7 = No 18 Vazia Vazia
a8 = No 29 a5 Vazia
a9 = No 40 Vazia a6
a10 = No 30 a8 a9
a11 = No 19 a7 Vazia
a12 = No 20 a11 a10
a13 = No 10 a4 a12


--a
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No a esq dir) = [a]
nivel n (No a esq dir) = (nivel (n-1) esq) ++ (nivel (n-1) dir)

--b
inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir) | x < y =  (No y (inserir x esq) dir)
			 | x > y =  (No y esq (inserir x dir))
			 | otherwise = (No y esq dir)

--c
mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) | x == y = dir
			   | otherwise = (No y Vazia (remover x dir))

remover x (No y esq Vazia) | x == y = esq
			   | otherwise = (No y (remover x esq) Vazia) 

remover x (No y esq dir) | x == y = (No (mais_esq dir) esq (remover (mais_esq dir) dir))
			 | x > y = (No y esq (remover x dir))
			 | otherwise = (No y (remover x esq) dir)


--d
listar :: Ord a => Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = (listar esq) ++ [x] ++ (listar dir)

--2
data Prop = Var Char
	| Neg Prop
	| Conj Prop Prop
	| Disj Prop Prop

propToArray :: Prop -> [Char]
propToArray (Var a) = [a]
propToArray (Neg a) = propToArray a
propToArray (Conj a b) = (propToArray a) ++ (propToArray b)
propToArray (Disj a b) = (propToArray a) ++ (propToArray b)


removeElemento n [] = []
removeElemento n (x:xs) | x == n = removeElemento n xs
			| otherwise = x:(removeElemento n xs) 	

contarAux [] = []
contarAux (x:xs) = [(x, (length (x:xs)) - (length z))] ++ contarAux z
		where z = removeElemento x (x:xs)

contar :: Prop -> [(Char, Int)]
contar p = contarAux vars
	where vars = propToArray p

	
--lista7
--10

--11
data Complex = Complex { 
	real :: Float,
	img :: Float
}

soma :: Complex -> Complex -> Complex
soma c1 c2 = Complex ((real c1) + (real c2)) ((img c1) + (img c2))

multi :: Complex -> Complex -> Complex
multi c1 c2 = Complex (((real c1) * (real c2)) - ((img c1) * (img c2))) (((real c1) * (img c2)) + ((real c2) * (img c1)))

neg :: Complex -> Complex
neg c1 = Complex ((real c1) * (-1)) ((img c1) * (-1))

absolute :: Complex -> Complex
absolute c1 = Complex (abs (real c1)) (abs (img c1))

--sinal :: Complex -> Int
sinal (Complex real1 img1) = 1

complexFromInt :: Integer -> Complex
complexFromInt n = Complex (fromInteger n) (0)

instance Num Complex where
	z1 + z2 = soma z1 z2
	z1 * z2 = multi z1 z2
	negate z = neg z
	abs z = absolute z
	signum z = sinal z
	fromInteger z = complexFromInt z

instance Eq Complex where
	c1 == c2 = (img c1) == (img c2) && (real c1) == (real c2)

--instance Fractional Complex
	
	
instance Show Complex where
	show c1 = (show (real c1))++" "++(show (img c1))++"i"




