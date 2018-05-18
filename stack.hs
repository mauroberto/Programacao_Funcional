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
