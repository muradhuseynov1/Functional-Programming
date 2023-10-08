module homework2
import StdEnv

//1. Give a list of numbers, multiplying all even numbers by 2 and all odd numbers by 3

multiply :: [Int] -> [Int]
multiply [] = []
multiply [x : xs]
| x rem 2 == 0 = [2*x] ++ multiply xs
| x rem 2 <> 0 = [3*x] ++ multiply xs
//Start = multiply [14, 22, 45, 56] // [28, 44, 135, 112]
//Start = multiply [13, 27, 44] // [39, 81, 88]
//Start = multiply [] // []


// 2. Given a list of integers, find the prime numbers and compute the sum of them.
// Return 0 for empty lists or if there are no primes.

is_Prime :: Int -> Bool
is_Prime x 
| x == 0 || x == 1 = False
| x < 0 = abort "Negative numbers are not prime." 
| x == 2 = True 
= and [x rem y <> 0 \\ y <- [2..x-1]]

sum_of_prime :: [Int] -> Int
sum_of_prime [] = 0
sum_of_prime [x : xs] 
| is_Prime x == True = x + sum_of_prime xs 
= sum_of_prime xs
//Start = sum_of_prime [14, 22, 45, 56] // 0
//Start = sum_of_prime [13, 27] // 13
//Start = sum_of_prime [13, 3, 76, 17] // 33
//Start = sum_of_prime [] // 0


/*
3. Given two lists of integers of the same length, check if the elements in two lists with the same index are of the same property (both even or both odd).
Return True for empty lists
*/

evod :: [Int] [Int] -> [Int]
evod [] [] = []
evod [x : xs] [y : ys]
| x rem 2 == 0 && y rem 2 == 0 = [x] ++ evod xs ys
| x rem 2 <> 0 && y rem 2 <> 0 = [x] ++ evod xs ys
| x rem 2 <> 0 || y rem 2 <> 0 = evod xs ys

same :: [Int] [Int] -> Bool
same [] [] = True
same b c
| length (evod b c) <> length b = False
| length (evod b c) == length b = True
//Start = same [1,2,3] [2,4,6] // False
//Start = same [1,2,3,4] [3,8,5,12] // True
//Start = same [] [] // True