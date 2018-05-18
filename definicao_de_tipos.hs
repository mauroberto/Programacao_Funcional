data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Eq, Show)

arv1 = Vazia
arv2 = No 2 arv1 arv1
arv3 = No 4 arv2 arv1
arv4 = No 5 arv3 arv2

tamanhoArv Vazia = 0
tamanhoArv (No n esq dir) = 1 + tamanhoArv esq + tamanhoArv dir

alturaArv Vazia = 0
alturaArv (No n esq dir) = 1 + max (alturaArv esq) (alturaArv dir)

sumArv Vazia = 0
sumArv (No n esq dir) = n + (sumArv esq) + (sumArv dir)


nivel n Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

mapArv f Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

reflectArv Vazia = Vazia
reflectArv (No x esq dir) = No x (reflectArv dir) (reflectArv esq)

