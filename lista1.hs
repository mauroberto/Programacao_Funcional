--questao16
gamma [] = ""
gamma [c] = [c]
gamma (c:u) = c:'.': gamma u

--questao22
--i
twice f x = f (f x)
--h
palindromo xs = reverse xs == xs

--questao24

triangulo a b c = a < b+c && b < a+c && c < b+a

--questao25
inserir x [] = [x]
inserir x (y:xs)  | x < y = x:y:xs
		  | otherwise = y:(inserir x xs) 


--questao26
--a
contar p [] = 0
contar p (x:xs) | p x = 1+(contar p xs)
		| otherwise = (contar p xs)

--b
contar2 p xs = length (filter p xs)
