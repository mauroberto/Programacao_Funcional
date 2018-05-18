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
