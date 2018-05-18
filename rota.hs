rotaEsq n xs = (drop n xs) ++ (take n xs)
rotaDir n xs = (drop (length xs-n) xs) ++ (take (length xs-n) xs)

