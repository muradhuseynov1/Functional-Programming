module progressTask1
import StdEnv

sumofcubes :: Int -> Int
sumofcubes x
| x < 0 = abort "x must be non-negative"
| x == 0 = 0
= x^3 + sumofcubes (x-1)

Start = sumofcubes 4 // 100