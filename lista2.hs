--questao1
--a
interior xs = init (tail xs)

--b
final n xs = drop ((length xs)-n) xs

--c
segmento n m xs = drop (n-1) (take m xs)

--d
extremos n xs = take n xs ++ drop ((length xs)-n) xs

--e
dotprod xs ys = sum (zipWith (\a b-> a*b) xs ys)

--f
elemento x xs = any (==x) xs

--questao2
--a
somaQuadrados n = sum [x^2 | x <- [1..n]] 

--b
replica n x = [x | y<-[0..(n-1)]]

--c
linha n = [x | x<-[(s+1)..s+n]] where s = sum[z | z<-[1..(n-1)]]

--d
triangulo n = [linha x | x<-[1..n]]

--e
fatores n = [x | x<-[1..n], mod n x == 0]

--f
perfeitos n = [x | x<-[1..(n-1)], (sum (fatores x) -x) == x]

--g
fatoresPrimos n = [(fator, exp) | fator <- [x | x<-(fatores n), length [z | z<-[1..x], mod x z == 0] == 2], exp <- [floor (sqrt (fromIntegral n)), floor (sqrt (fromIntegral n))-1 .. 1], (mod n (fator^exp) == 0) && (mod n (fator^(exp+1)) /= 0)]

--h
intersecao xs ys = [x | x <- xs, elemento x ys]

--k
aplica xs n = [(x n) | x <- xs]

--l
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2 == z^2]

--m
densa xs = [(x,y) | (x,y) <- (zip  (reverse[0..((length xs)-1)]) xs), y /= 0]

--n
neglist xs = length [x | x<-xs, x<0]

--o
gensquares low high = [x^2 | x<-[low..high], mod x 2 == 0]

--questao3
--a
listaNumeradores = [4*((-1)^(x+1)) | x<-[1..]]

--b
listaDenominadores = [x | x<-[1,3..]]

--c
combinando = zipWith (/) listaNumeradores listaDenominadores

--d
calcPi1 n = sum (take n combinando)

--a
binom n k = floor((product [1..n]) / ((product [1..k]) * (product [1..(n-k)])))

--b
pascal 0 = [[1]]
pascal n = (pascal (n-1)++[[binom n z | z<-[0..n]]])

--c
pascal2 = pascal (2^10)
