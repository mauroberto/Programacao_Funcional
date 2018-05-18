
--2
module Stack (Stack, push, pop, height, top, empty, isEmpty) where

data Stack a = Empty | Top a (Stack a)

push :: a -> Stack a -> Stack a
push a Empty = Top a (Empty)
push a s = Top a (s)

pop :: Stack a -> Maybe (Stack a)
pop Empty = Nothing
pop (Top a s) = Just s


height :: Stack a -> Int
height Empty = 0
height (Top a s) = 1 + (height s)

top :: Stack a -> Maybe a
top Empty = Nothing
top (Top a s) = Just a

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty s = False

instance Show a => Show (Stack a) where
	show (Empty) = "Pilha de altura 0"
	show (Top a s) = "Pilha de altura "++(show (height (Top a s)))++" e topo " ++(show a)

--3
module Queue(Queue, startQueue, endQueue, pushQueue, popQueue, isEmptyQueue, lenQueue, whileNotEmpty) where

data Queue a = Empty | Start a (Queue a)

startQueue :: Queue a -> Maybe a
startQueue Empty = Nothing
startQueue (Start a q) = Just a

endQueue :: Queue a -> Maybe a
endQueue Empty = Nothing
endQueue (Start a Empty) = Just a
endQueue (Start a q) = (endQueue q)

pushQueue :: a -> Queue a -> Queue a
pushQueue a Empty = Start a Empty
pushQueue a (Start b q) = Start b (pushQueue a q)

popQueue :: Queue a -> Queue a
popQueue Empty = Empty
popQueue (Start a q) = q

isEmptyQueue :: Queue a -> Bool
isEmptyQueue Empty = True
isEmptyQueue q = False

lenQueue :: Queue a -> Int
lenQueue Empty = 0
lenQueue (Start a q) = 1 + (lenQueue q)

whileNotEmpty :: (a -> b) -> Queue a -> [b]
whileNotEmpty f Empty = []
whileNotEmpty f (Start a q) = (f a):(whileNotEmpty f q) 

instance Show a => Show (Queue a) where
	show Empty = "Fila Vazia"
	show q = "Fila de tamanho "++show(lenQueue q)++", com o primeiro elemento "++show(startQueue(q))++" e último elemento "++show(endQueue q)


--10

--11
data Complex = Complex { 
	real :: Float,
	img :: Float
}

soma :: Complex -> Complex -> Complex
soma c1 c2 = Complex ((real c1) + (real c2)) ((img c1) + (img c2))

multi :: Complex -> Complex -> Complex
multi c1 c2 = Complex (((real c1) * (real c2)) - ((img c1) * (img c2))) (((real c1) * (img c2)) + ((real c2) * (img c1)))

neg :: Complex -> Complex
neg c1 = Complex ((real c1) * (-1)) ((img c1) * (-1))

absolute :: Complex -> Complex
absolute c1 = Complex (abs (real c1)) (abs (img c1))

--sinal :: Complex -> Int
sinal (Complex real1 img1) = 1

complexFromInt :: Integer -> Complex
complexFromInt n = Complex (fromInteger n) (0)

instance Num Complex where
	z1 + z2 = soma z1 z2
	z1 * z2 = multi z1 z2
	negate z = neg z
	abs z = absolute z
	signum z = sinal z
	fromInteger z = complexFromInt z

instance Eq Complex where
	c1 == c2 = (img c1) == (img c2) && (real c1) == (real c2)

--instance Fractional Complex
	
	
instance Show Complex where
	show c1 = (show (real c1))++" "++(show (img c1))++"i"
