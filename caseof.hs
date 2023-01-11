catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:t) = x : catMaybes t
catMaybes (Nothing:t) = catMaybes t


catMaybes' :: [Maybe a] -> [a]
catMaybes'[] = []
catMaybes' (h:t) = case h of Just x -> x : catMaybes' t
                             Nothing -> catMaybes' t

catMaybes'' :: [Maybe a] -> [a]
catMaybes'' l = 
        case l of [] -> []
                  (h:t) -> case h of Just x -> x : catMaybes'' t
                                     Nothing -> catMaybes''t

catMaybes_os :: [Maybe a] -> [a]
catMaybes_os l = foldr (\x acc -> case x of Just y -> y : acc; Nothing -> acc) [] l