--2
--module Main (main) where
	--main = do {n <- readLn; elefantes n}


	import System.Random(randomRIO)
	elefantes :: Int -> IO() 
	elefantes n = if n <= 2 then 
			putStrLn "" 
		      else do 
			elefantes (n-1) 
			putStrLn ("Se "++(show (n-1))++" elefantes incomodam muita gente")
			putStrLn ((show (n))++" elefantes incomodam muito mais!")

	--3

	--main = do {
	--	n <- readLn;
	--	contents <- lerESomar n;
	--	putStrLn $ show contents
	--}

	lerESomar n = do
		if n == 0 then return 0
		else do
			a <- readLn
			somaResto <- lerESomar (n-1)
			return (a + somaResto)
	


	--6
	--main = do {n <- readLn; putStrLn $ show (factorial n)}

	factorial::Int -> Int
	factorial 0 = 1
	factorial n = n*(factorial (n-1))

	--7
	--main = do{n <- readLn; isPrime n}

	isPrime n = if (length [x | x<-[1..n], mod n x == 0]) == 2 then putStrLn "Sim" else putStrLn "Não" 

	--jogo
	main = do{
		x <-randomRIO(1, 100) :: IO Int;
		n <- jogo 1 x; 
		putStrLn ("acertou em "++show n ++" tentativas")
	}

	jogo t n = do{
		v <- readLn;
		putStrLn ("Tentativa "++(show v));
		if v == n then
			return (t)
		else do
			if v > n then putStrLn "Alto!"
			else putStrLn "Baixo!"
			jogo (t+1) n
	}

	--monadas
	--1

	type Passaros = Int
	type Barra = (Passaros, Passaros)

	pousoEsq :: Passaros -> Barra -> Maybe Barra
	pousoEsq n (esq, dir)
		| abs ((esq + n) - dir) < 4 = Just (esq + n, dir)
		| otherwise 		    = Nothing


	pousoDir :: Passaros -> Barra -> Maybe Barra
	pousoDir n (esq, dir)
		| abs ((dir + n) - esq) < 4 = Just (esq, dir + n)
		| otherwise 	            = Nothing


	data Pos = Esq | Dir deriving (Eq, Show)
	type Pouso = (Pos, Passaros)

	b1 = (0,0) :: Barra
	p1 = (Esq, 2) :: Pouso
	p2 = (Dir, 4) :: Pouso
	sq = [p1, p2]

	rotina :: Barra -> [Pouso] -> Maybe Barra
	rotina b [] = Just b
	rotina b ((d, p): xs) =		
		if d == Dir then do
			b <-pousoDir p b
			rotina b xs
		else do
			b <- pousoEsq p b
			rotina b xs

		

	--3
	(!?) :: [a] -> Int -> Maybe a
	(!?) [] _ = Nothing
	(!?) xs n = if n < 0 || n >= (length xs) then Nothing
		    else Just (xs !! n)

	--4

	swapAux x y x1 y1 xs =
		[if (i == x) then y1 else if (i == y) then x1 else xs !! i | i<-[0..((length xs)-1)]]
	
	swap :: Int -> Int -> [a] -> Maybe [a]
	swap x y xs = do
		x1 <- xs !? x
		y1 <- xs !? y
		Just (swapAux x y x1 y1 xs)
	--5
	getElts :: [Int] -> [a] -> Maybe [a]
	getElts xs ys = mapM (ys !?) xs

	

