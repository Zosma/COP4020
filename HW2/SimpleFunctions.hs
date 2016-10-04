module SimpleFunctions where

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterfirst p [] = []
filterfirst p [x] = if p x then [x] else []
filterFirst p xs = if p x then [x] ++ (filterFirst p (tail xs)) else tail xs
     where x = head xs

-- b)
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p [] = []
filterLast p [x] = if p x then [x] else []
filterLast p xs = if p x then (filterLast p (init xs)) ++ [x] else init xs
     where x = last xs

-- c)
split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split [x,y] = ([x],[y])
split (x:y:xs) = ([x]++ (fst (split xs)),[y] ++ (snd (split xs)))

-- d)
interleave :: ([a],[a]) -> [a]
interleave ([],[]) = []
interleave ((x:xs),[]) = [x] ++ (interleave (xs,[]))
interleave ([],(y:ys)) = [y] ++ (interleave ([],ys))
interleave ((x:xs),(y:ys)) = [x] ++ [y] ++ (interleave (xs,ys))

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]
merge ([],[]) = []
merge ((x:xs),[]) = [x] ++ (merge (xs,[]))
merge ([],(y:ys)) = [y] ++ (merge ([],ys))
merge ((x:xs),(y:ys)) = if x < y then [x] ++ (merge (xs,[y] ++ ys)) else [y] ++ (merge ([x] ++ xs,ys))

-- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x,y] = merge (split [x,y])
mergeSort (x:y:xs) = merge ((merge (split [x,y])),(mergeSort xs))
