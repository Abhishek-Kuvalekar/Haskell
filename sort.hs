-- Implementation of insertion sort
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  |(x <= y) = x:(y:ys)
  |otherwise = y:(insert x ys)
--


-- Implementation of merge sort
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (x:xs) = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    firstHalf = take (length (x : xs) `div` 2) (x:xs)
    secondHalf = drop (length (x : xs) `div` 2) (x:xs)

merge :: [Int] -> [Int] -> [Int]
merge [] m = m
merge l [] = l
merge (x:xs) (y:ys)
  |(x <= y) = x : (merge xs (y:ys))
  |otherwise = y: (merge (x:xs) ys)
--


-- Implementation of quick sort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort firstHalf) ++ [pivot] ++ (quickSort secondHalf)
  where
    pivot = x
    firstHalf = [y | y <- xs, y <= pivot]
    secondHalf = [z | z <- xs, z > pivot]
--
