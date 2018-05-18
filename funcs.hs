tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

pegar _ [] = []
pegar 0 _ = []
pegar n (x:xs) = x:(pegar (n-1) xs)

apagar _ [] = []
apagar 0 xs = xs
apagar n (x:xs) = apagar (n-1) xs
