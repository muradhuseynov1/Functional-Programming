// Given a list of integers, write a function that goes through the element, and for every element i, 
// generates the i-th Fibonnaci number, and only check it's even or odd, only keeps the even ones.
// Example [0,1,2] -> [2] because the 0th fib is 1, the 1st fib is 1 and the 2nd dib is 2, 
// but only 2 is even, so it's in the final list.

module progresstask3
import StdEnv

fib1 :: Int -> Int
fib1 n = fibAux n 1 1
fibAux 0 a b = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

generateFibEven :: [Int] -> [Int]
generateFibEven [] = []
generateFibEven [x:xs]
| isEven (fib1 x) = [fib1 x] ++ generateFibEven xs
= generateFibEven xs

//Start = generateFibEven [0,1,2] // [2]
//Start = generateFibEven [3,7,11] //[144]
//Start = generateFibEven [4..10] //[8,34]