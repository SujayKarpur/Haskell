data Color = Red | Black 
data Side = Left | Right 

data Tree a = Empty | Node Color a (Tree a) (Tree a) 

contains :: (Ord, Eq a) => Tree a -> a -> Bool 
contains Empty _ = False 
contains (Node _ x l r) val 
                         |x == val = True 
                         |val > x = contains r val 
                         |otherwise = contains l val 


search :: (Ord, Eq a) => Tree a -> a -> Tree a 
search Empty _ = Empty 
search (Node c x l r) val 
                         |x == val = Node c x l r
                         |val > x = contains r val 
                         |otherwise = contains l val 


insert :: (Ord, Eq a) => Tree a -> a -> Tree a 
delete :: (Ord, Eq a) => Tree a -> a -> Tree a 
display :: (Ord, Eq a) => Tree a -> Nothing 
rotate :: (Ord, Eq a) => Tree a -> Tree a -> Side -> Tree a  
