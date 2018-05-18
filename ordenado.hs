ordenado :: [Int] -> Bool
ordenado [] = True
ordenado [x] = True
ordenado (x:y:xs) = (x<=y) && (ordenado(y:xs))
