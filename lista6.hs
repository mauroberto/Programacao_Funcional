import Data.List

--questao1

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x | x > 0 = Just (log x)
	  | otherwise = Nothing


--questao2

--a
type Ponto = (Float, Float)

dist :: Ponto -> Ponto -> Float
dist p1 p2 = sqrt (((fst(p1)-fst(p2))^2) + ((snd(p1)-snd(p2))^2)) 


--b
comprimento :: [Ponto] -> Float
comprimento [] = 0.0
comprimento [x] = 0.0
comprimento (x:y:xs) = (dist x y) + comprimento (y:xs)

--questao3

data Ponto2 = Pt Float Float
type Raio = Float
type Regiao = Ponto2 -> Bool

c1 = (circ (Pt 0 0) 10)
r2 = (retang (Pt 15 15) (Pt 20 20))
r3 = (retang (Pt 18 18) (Pt 22 22))
r4 = (retang (Pt 18 18) (Pt 20 20))
 
--a
retang :: Ponto2 -> Ponto2 -> Regiao
retang (Pt x1 y1) (Pt x2 y2) = (\(Pt x y)-> if(x1 <= x && y1 <= y && x2 >= x && y2 >= y) then True else False)

dist2 :: Ponto2 -> Ponto2 -> Float
dist2 (Pt x1 y1) (Pt x2 y2) = sqrt(((x1 - x2)^2) + ((y1 - y2)^2))

circ :: Ponto2 -> Raio -> Regiao
circ p1 r = (\p2 -> if((dist2 p1 p2) > r) then False else True)

--b
uniao :: Regiao -> Regiao -> Regiao
uniao r1 r2 = (\p -> (r1 p) || (r2 p)) 

intersecao :: Regiao -> Regiao -> Regiao
intersecao r1 r2 = (\p -> (r1 p) && (r2 p)) 

complemento :: Regiao -> Regiao
complemento r = (\p -> not (r p))

--questao7
data MConj a = Vazio | No a Int (MConj a) (MConj a) deriving (Show)

mc1 = No 'A' 2 Vazio (No 'B' 1 Vazio Vazio)
mc2 = No 3 2 Vazio (No 4 1 Vazio Vazio)
mc3 = No 'C' 3 Vazio Vazio

--a
ocorre :: Ord a => a -> MConj a -> Int
ocorre _ Vazio = 0
ocorre x (No a n b c) = if a == x then n + (ocorre x b) + (ocorre x c) else (ocorre x b) + (ocorre x c)

--b
inserir :: Ord a => MConj a -> MConj a -> MConj a
inserir Vazio a = a
inserir a Vazio = a
inserir (No a n b c) (No a2 n2 b2 c2) | a < a2 = (No a n b (inserir c (No a2 n2 b2 c2)))
				      | a > a2 = (No a n (inserir b (No a2 n2 b2 b2)) c)
				      | otherwise = (No a (n+n2) b c)

--c
listar :: MConj a -> [a]
listar Vazio = []
listar (No a n b c) = if n > 0 then a:(listar (No a (n-1) b c)) else (listar b) ++ (listar c)

--d
tamanho :: MConj a -> Int
tamanho Vazio = 0
tamanho (No a n b c) = n + (tamanho b) + (tamanho c)

--e
--sumMConj :: Num a => MConj a -> a
sumMConj Vazio = 0
sumMConj (No a n b c) = (fromInteger(a) * n) + (sumMConj b) + (sumMConj c)


--questao8
data Mobile  = Haste Mobile Int Mobile Int | Objeto Int deriving (Eq, Ord)
m1 = Haste (Objeto 1) 6 (Objeto 3) 2
m2 = Haste (Objeto 2) 4 (Objeto 4) 2
m3 = Haste (Objeto 1) 1 (Objeto 1) 1
m4 = Haste (m3) 3 (m2) 1
m5 = Haste (m4) 2 (m1) 6

peso :: Mobile -> Int
peso (Objeto x) = x
peso (Haste x1 d1 x2 d2) = (peso x1) + (peso x2)


equilibrio :: Mobile -> Bool
equilibrio (Objeto x) = True
equilibrio (Haste x1 d1 x2 d2) = if (((peso x1)*d1) == ((peso x2)*d2)) then (equilibrio x1) && (equilibrio x2) else False

--questao13

data Prop = Const Bool | Var Char | Neg Prop  | Conj Prop Prop  | Disj Prop Prop  | Impl Prop Prop deriving (Eq , Show) 
prop1 = Impl (Var 'P') (Var 'Q')
prop2 = Impl (Neg (Var 'P')) (Var 'Q')
prop3 = Impl prop1 prop2

variaveis :: Prop -> [Char]
variaveis (Const x) = []
variaveis (Var x) = [x]
variaveis (Neg p) = variaveis p
variaveis (Conj p q) = nub (variaveis p ++ variaveis q)
variaveis (Disj p q) = nub (variaveis p ++ variaveis q)
variaveis (Impl p q) = nub (variaveis p ++ variaveis q)

showProp :: Prop -> String
showProp (Const x) = if(x)then "T" else "F"
showProp (Var x) = [x]
showProp (Neg p) = "(~"++(showProp p)++")"
showProp (Conj p q) = "( "++(showProp p)++" && "++(showProp q)++" )"
showProp (Disj p q) = "( "++(showProp p)++" || "++(showProp q)++" )"
showProp (Impl p q) = "( "++(showProp p)++" -> "++(showProp q)++" )"
