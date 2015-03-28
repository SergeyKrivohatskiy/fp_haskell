module Tree where

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] } deriving Eq

-- show и read должны работать как описано в тестах в Main.hs
instance Show a => Show (Tree a) where
    show (Node v children) = (show v) ++ (show_children children) where
        show_children [] = ""
        show_children (x:xs) = ":{" ++ (show x) ++ (show_other xs) ++ "}"
        show_other [] = ""
        show_other (x:xs) = "," ++ (show x) ++ (show_other xs)

starts_with str [] = True
starts_with [] (_:_) = False
starts_with (x:xs) (y:ys) = x == y && starts_with xs ys

ends_with str1 str2 = starts_with (reverse str1) (reverse str2)

remove str 0 = str
remove (x:xs) i = remove xs (i - 1)

remove_back str i = reverse (remove (reverse str) i)

read_children unused str = case (readsPrec unused str) of
    [] -> Nothing
    [(child, remaind)] -> if remaind `starts_with` ","
        then case (read_children unused (remove remaind 1)) of
            Just (children, remaind_remind) -> Just ((child: children), remaind_remind)
            Nothing -> Nothing
        else if remaind `starts_with` "}"
                then Just ([child], remove remaind 1)
                else Nothing
instance Read a => Read (Tree a) where
    readsPrec unused str = case readsPrec unused str of 
        [] -> []
        ((v, remaind):xs) -> (if remaind `starts_with` ":{"
            then case (read_children unused (remove remaind 2)) of
                Just (children, remaind_remind) -> [(Node v children, remaind_remind)]
                Nothing -> [(Node v [], remaind)]
            else [(Node v [], remaind)])

instance Functor Tree where
    fmap f (Node v children) = (Node (f v) (fmap_children f children)) where
        fmap_children f [] = []
        fmap_children f (x:xs) = ((fmap f x):(fmap_children f xs))
