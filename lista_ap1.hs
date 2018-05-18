--questao1
--a
final n xs = drop ((length xs)-n) xs

--b
rota n xs = drop n xs ++ take n xs

--questao2
--a
interseccao xs ys = [x | x<-xs, elem x ys]

--b
diferencia xs ys = [x | x<-xs, notElem x ys]

--questao3
--a
duplicar [] = []
duplicar (x:xs) | elem x vogais = x:x:duplicar xs
		| otherwise = x:duplicar xs
		where vogais = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'] 

--b
aplica [] n = []
aplica (x:xs) n = x n: aplica xs n 

--questao4
--a
paridade [] = False
paridade (x:xs) | x = not (paridade xs)
		| otherwise = paridade xs

--b
paridadeFold xs = foldr (\x a -> if x then not a else a) False xs

--questao5
media xs = media' 0  0 xs

media' n s [] = s/n;
media' n s (x:xs) = media' (n+1) (s+x) xs

--questao6
diamonds n = [[i*x | i<-[1..x]] | x<-[1..n]] ++ [[i*x | i<-[1..x]] | x<-[(n-1),(n-2)..1]]

--questao7
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:y:xs) = x:n:(intersperse n (y:xs))


--questao8
group [] = []
group (x:xs) = (x, length (takeWhile (==x) (x:xs))): group (dropWhile (==x) (x:xs)) 

--questao9
--a
scanSum [] = []
scanSum [x] = [x]
scanSum (x:xs) = x:[i+x|i<-(scanSum xs)]

--a
prefixos [] = []
prefixos xs = prefixos (take ((length xs)-1) xs) ++ [xs]


--b
scanSum2 [] = []
scanSum2 xs = [sum x | x<-(prefixos xs)]

--c

scanSum3 [] = []
scanSum3 [x] = [x]
scanSum3 (x:xs) | (length xs) > 1 = x:(scanSum3 ((x+(head xs)):(drop 1 xs)))
	      	| otherwise = x:[(x+(head xs))]



