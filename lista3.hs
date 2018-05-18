--questao1
--a
remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y then ys else y:(remove x ys)

--b
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = [ x:y | y <- partes xs] ++ partes xs

--c
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

--d
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--e
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--questao2
--a 
-- remove o primeiro valor igual a x da lista

--b
-- retorna todos os subconjuntos da lista passada

--c
-- rotaciona a lista n vezes para a esquerda

--d
-- recebe uma tupla de duas posições e inverte as posições

--e
-- recebe uma função f e aplica-a duas vezes a um valor x

--questao3
--a
prodIntervalo n m = product [n..m]

--b
somaIntervalo n m = sum [n..m]

--c
metades xs = splitAt (div (length xs) 2) xs

--questao4
--a
potencia x n = x^n

--b
elemento _ [] = False
elemento x (y:ys) = x == y || elemento x ys

--c 
seleciona 0 (x:xs) = x
seleciona n (x:xs) = seleciona (n-1) xs

--d
refinada [] = []
refinada [x] = [x*2/2]
refinada (x:y:xs) = x:((x+y)/2):refinada (y:xs)

--e
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | y < x = y:(merge (x:xs) ys) 
		    | otherwise = x:(merge xs (y:ys))

--f
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (fst (metades xs))) (mergesort (snd (metades xs)))

--g
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

--h
subconjunto _ [] = False
subconjunto [] _ = True
subconjunto (x:xs) ys = (elemento x ys) && subconjunto xs ys

--i
union xs [] = xs
union [] ys = ys
union xs (y:ys) | notElem y xs = union (xs ++ [y]) ys
	      	| otherwise = union xs ys

--j
diferencia xs [] = xs
diferencia [] ys = []
diferencia (x:xs) ys | notElem x ys = x:(diferencia xs ys)
		     | otherwise = diferencia xs ys

--k
frequencia _ [] = 0
frequencia x (y:ys) | y == x = 1 + (frequencia x ys)
		    | otherwise = frequencia x ys

--l
unico x xs | (frequencia x xs) == 1 = True --MODIFICAR--
	   | otherwise = False

--m

absoluto x | x >= 0 = x
           | otherwise = -x
maiorSalto [x] = 0 
maiorSalto (x:y:xs) | absoluto (x-y) > (maiorSalto (y:xs)) = absoluto (x-y)
		    | otherwise = maiorSalto (y:xs)

--n
horner [] z = 0
horner (c:cs) z = c+z*(horner cs z)

--o
fac n = fac' 1 n

fac' ac n | n == 1 = ac
	  | otherwise = fac' (ac*n) (n-1) 


reverso xs = reverse' (length xs) xs

reverse' _ [] = []
reverse' n (x:xs) | n == 0 = [x]
	      	  | otherwise = (reverse' (n-1) xs) ++ [x]

--p
remove2 _ [] = []
remove2 x (y:ys) | x == y = remove2 x ys
                 | otherwise = (y:(remove2 x ys))

--q
unique [] = []
unique (x:xs) = x: unique (remove x xs)


--r
inserir x [] = [x]
inserir x [y] | x > y = [y] ++ [x]
              | otherwise = x:[y]

inserir x (a:b:ys) | x <= a = x:a:b:ys
                   | x > a && x <=b = a:x:b:ys
                   | otherwise = a: inserir x (b:ys)


inserir2 x [] = [x]
inserir2 x (y:ys) | x < y = x:y:ys
                  | otherwise = y: (inserir2 x ys)

--s 
permutations [] = [[]]
permutations xs = [i:j | i<-xs, j<-permutations (remove i xs)]
