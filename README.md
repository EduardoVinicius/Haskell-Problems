# Haskell-Problems
Some Haskell Problems

-- 1
myLast :: [a] -> a
myLast [] = error "It's an empty list!"
myLast [x] = x
myLast (h:t) = myLast t

-- 2
myButLast :: [a] -> a
myButLast [] = error "It's an empty list!"
myButLast [x, y] = x
myButLast (h:t) = myButLast t

-- 3
-- I can also use !!
elementAt :: [a] -> Int -> a  
elementAt [] _ = error "It's an empty list!"
elementAt _ 0 = error "Indexes start at 1!"
elementAt (h:t) n | n > (length (h:t)) = error "The list's not that big!"
                  | (n == 1) = h
                  | otherwise = elementAt t (n-1)

-- 4
myLength :: [a] -> Int 
myLength [] = 0
myLength (h:t) = 1 + (myLength t)

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = (myReverse t) ++ [h]


-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome li | (li == (myReverse li)) = True
                | otherwise = False
