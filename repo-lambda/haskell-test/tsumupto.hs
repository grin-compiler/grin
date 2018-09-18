
main = print (xsum 0 (upto 1 10000000))

upto :: Int -> Int -> [Int]
upto m n = if m > n then [] else m : upto (m+1) n

xsum :: Int -> [Int] -> Int
xsum n []     = n
xsum n (x:xs) = xsum (n+x) xs
