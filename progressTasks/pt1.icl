// Write a recursive(!) function for the sum of cubes of the first 'n' positive values.
// 1*1*1+2*2*2+3*3*3+ ... +n*n*n 
// Assume that 'n' is non-negative.

module progressTask1
import StdEnv

sumofcubes :: Int -> Int
sumofcubes x
| x < 0 = abort "x must be non-negative"
| x == 0 = 0
= x^3 + sumofcubes (x-1)

Start = sumofcubes 4 // 100
