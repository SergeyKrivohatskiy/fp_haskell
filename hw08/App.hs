-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node a arr) = Node (f a) (fmapArr f arr) where
        fmapArr f [] = []
        fmapArr f (x:xs) = (fmap f x: fmapArr f xs)


instance Applicative Tree where
    pure x = Node x (repeat (pure x))
    (<*>) (Node ab abArr) (Node a aArr) = Node (ab a) (processArr abArr aArr) where
        processArr [] _ = []
        processArr (x:xs) (y:ys) = ((<*>) x y: processArr xs ys)
