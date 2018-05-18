data Polinomio a = PolZero | ConsPol a Int (Polinomio a) deriving (Eq)

pol1 = ConsPol 4 2 ( ConsPol 5 1 (ConsPol 4 0 PolZero) )
pol3 = ConsPol 4 3 ( ConsPol 5 2 (ConsPol 3 0 PolZero) )
pol2 = ConsPol 4 2 ( ConsPol 3 1 (ConsPol 4 0 PolZero) )

eval PolZero n = 0
eval (ConsPol a b p) n = a*(n^b) + (eval p n)

instance Show a => Show (Polinomio a) where
	show (PolZero) = ""
	show (ConsPol a b PolZero) = (show a) ++ "x" ++ "^" ++ (show b)
	show (ConsPol a b (ConsPol a1 b1 p1)) = (show a) ++ "x" ++ "^" ++ (show b) ++ " + " ++ (show (ConsPol a1 b1 p1))
	
soma PolZero p2 = p2
soma p1 PolZero = p1
soma (ConsPol a1 b1 p1) (ConsPol a2 b2 p2) | b1 == b2 = (ConsPol (a1+a2) b1 (soma p1 p2))
					   | b1 > b2 = (ConsPol a1 b1 (soma p1 (ConsPol a2 b2 p2)))
					   | otherwise = (ConsPol a2 b2 (soma  (ConsPol a1 b1 p1) p2))

