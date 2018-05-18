raiz q = iterate (\x -> 0.5*(x+q/x)) q

absoluto xs ep = head [x | (x,y) <- zip xs (tail xs), abs (x-y) < ep]

relativo xs ep = head [x | (x,y) <- zip xs (tail xs), (abs (x-y) / x) < ep]

fib = 0:1:zipWith (+) fib (tail fib)

primos = crivo [2 ..]

crivo (x:xs) = x:crivo[y | y<-xs, mod y x /= 0]

fatorial 0 = 1
fatorial n = n*(fatorial (n-1))

binomio n k = (fatorial n)/((fatorial k) * (fatorial (n-k)))

pascal = [[binomio n k | k <- [0..n]] | n<-[0..]]

pascal2 = [[ if k == 0 || k == n then 1 else (pascal2 !! (n-1) !! (k-1)) + (pascal2 !! (n-1) !! k)| k<-[0..n] ]| n<-[0..]] 
