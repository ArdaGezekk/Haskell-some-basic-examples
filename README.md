# Haskell-some-basic-examples
There are some usefull functtion, 18 questions and its answers about haskell, the functional programming language.


l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]

f1 a b c = a + b + c

f2 name = "Hello " ++ name

f3 n = 2^n

f4 0 = 0
f4 1 = 1
f4 x = f4 (x-1) + f4 (x-2)

factorial 0 = 1
factorial a = a * factorial (a - 1)

f5 n r = factorial( n ) / ( factorial( r ) * factorial( n - r ) )

f6  n r = factorial( n ) / factorial( r )

f7 radius height =  pi * radius * radius * height

f8 radius  = ( 4 / 3 ) * pi * radius * radius * radius

f9 base height =  ( 1 / 3 ) * base * base * height

f10 radius height = pi * ( 1 / 3 ) * radius * radius * height

f11 :: [a] -> Integer
f11 []     =  0
f11 (x:xs) =  1 + f11 xs

f12 :: Ord a => [a] -> Int
f12 (x:xs) = f12' xs x 0
f12' :: Ord a => [a] -> a -> Int -> Int
f12' [] _ count = count
f12' (x:xs) prev count = f12' xs x newcount
  where newcount = if x<prev then count+1 else count

f13 :: Eq a => [a] -> a -> Int
f13 [] f = 0
f13 ys f = length list
    where list = [list | list <- ys, list == f]

f14 = y - x
liste = [2, 5, 3, 8, 4, 3]

tek = [ x | x <- liste, x `mod` 2 == 1]
cift = [ y | y <- liste, y `mod` 2 == 0]
x = sum$tek
y = sum$cift



f15 :: (Ord a) => [a] -> (a, Int)
f15 l =
  let f15 :: (Ord a) => [a] -> Int -> (a, Int)
      f15 [x] xi = (x, xi)
      f15 (x:xs) xi
        | x > t     = (x, xi)
        | otherwise = (t, ti)
        where
          (t, ti) = f15 xs (xi + 1)
  in f15 l 0

f16 :: [Integer]
f16 = 2: 3: chk (tail f17) [5,7..]
 where
  chk (p:ps) xs = h ++ chk ps [x | x <- t, x `rem` p /= 0]
                  where (h,~(_:t)) = span (< p*p) xs

f17 0 y = y
f17 x y = f17 (y `mod` x) x
fuc x y | x < 0     = fuc (-x) y
          | y < 0     = fuc x (-y)
          | y < x     = f17 y x
          | otherwise = f17 x y

f18 :: Integral a => a -> [Int]
f18 n = f18' n 0 []

f18' :: Integral a => a -> Int -> [Int] -> [Int]
f18' 0 _ acc = acc
f18' n pos acc = f18' newN newPos newAcc
  where
    newN = ( n `quot` 2 )
    newAcc = if odd n then (acc ++ [pos]) else acc
    newPos = 1+pos


main=do

 print $f0

 print(f1 2 5 7)

 print(f2 "Arda")

 print(f3 5,0)

 print(f4 12,1,0)

 print(f5 10 3)

 print(f6 10 3)

 print(f7 5 20)

 print(f8 10)

 print(f9 10 6)

 print(f10 5 9)

 print (f11 l)

 print (f12 l)

 print(f13 [1,3,1,3,3,3,2,3,4,3] 3)
 print(f13 "Hello Ali" 'l')

 print (f14)

 print(f15 l)

 print("f17 infinite ")
 --print(f17)

 print(f17 24 18)

 print(f18 53)


