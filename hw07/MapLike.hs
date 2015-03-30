-- Зачем это всё?
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.
class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k, v)] -> m k v
    fromList [] = empty
    fromList ((k, v):xs) = insert k v (fromList xs)

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

newtype ListMap k v = ListMap { toList :: [(k,v)] }

newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike M.Map where
    empty = M.empty
    lookup = M.lookup
    insert = M.insert
    delete = M.delete
    fromList = M.fromList

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap []) = Nothing
    lookup k (ListMap ((x, y):xs)) | x == k = Just y
                                      | otherwise = lookup k (ListMap xs)
    insert k v (ListMap []) = ListMap [(k, v)]
    insert k v (ListMap ((x, y):xs)) | x == k = ListMap ((k, v):xs)
                                     | otherwise = ListMap ((x, y):toList (insert k v (ListMap xs)))
    delete k (ListMap []) = ListMap []
    delete k (ListMap ((x, y):xs)) | x == k = ListMap xs
                                   | otherwise = ListMap ((x, y):toList (delete k (ListMap xs)))
    fromList = ListMap



instance MapLike ArrMap where
    empty = ArrMap (const Nothing)
    lookup k (ArrMap f) = f k
    insert k v (ArrMap f) = ArrMap (\x -> if x == k then Just v else f x)
    delete k (ArrMap f) = ArrMap (\x -> if x == k then Nothing else f x)



-- 3. Написать instace Functor для ListMap k и ArrMap k.

instance Functor (ArrMap k) where
    fmap f (ArrMap map_f) = ArrMap (\k -> fmap f (map_f k))
{-
        new_f f map_f k = case map_f k of -- Да это же fmap!
            Nothing -> Nothing
            Just v -> Just (f v)
-}

instance Functor (ListMap k) where
    fmap f (ListMap []) = empty
    fmap f (ListMap ((x, y):xs)) = ListMap ((x, f y):toList (fmap f (ListMap xs)))
