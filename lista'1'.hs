--questao2
replica 0 _ = []
replica a b = (b:(replica  (a-1) b))

--questao3
elemento a [] = False
elemento a (x:xs) = a == x || elemento a xs

--questao4
isSorted [] = True
isSorted [a] = True
isSorted (x:y:xs) = x <= y && isSorted(y:xs)

--questao5
palindromo [] = True
palindromo [a] = True
palindromo (x:xs) = x == (last xs) && palindromo (init xs)

--questao6
rotEsq 0 x = x
rotEsq _ [] = []
rotEsq n (x:xs) = rotEsq (n-1) (xs++[x])

--questao7
