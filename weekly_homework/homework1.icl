module homework1
import StdEnv

// Write a function that will take a digit (Int)
// and return the respective word for it (String).
// For example input of 1 should output One; input of 0 should output Zero; input of 5 should output Five.
// Anything that is not the digit (0-9) should output "Not a digit"

digit_to_string :: Int -> String
digit_to_string x
| x < 0 || x > 9 = abort "Not a digit"
| x == 0 = "Zero"
| x == 1 = "One"
| x == 2 = "Two"
| x == 3 = "Three"
| x == 4 = "Four"
| x == 5 = "Five"
| x == 6 = "Six"
| x == 7 = "Seven"
| x == 8 = "Eight"
| x == 9 = "Nine"
//Start = digit_to_string 4 //"Four"
//Start = digit_to_string 0 //"Zero"
//Start = digit_to_string 5 //"Five"
//Start = digit_to_string 10 //"Not a digit"
//Start = digit_to_string -1 //"Not a digit"
//Start = digit_to_string 42 //"Not a digit"





// Write a function that takes Int and checks if this number is prime or not.
// handle the case of negative numbers (negative numbers are not primes).
// 0 and 1 are not prime numbers.

divisorsOfnum :: Int Int -> [Int]
divisorsOfnum a b
| b == 0 = [] // stop condition: divisor cannot be 0
| a rem b == 0 = [b] ++ divisorsOfnum a (b-1) // in order to count all of the divisors (including the number itself), a should be be equal to b (a==b)
= divisorsOfnum a (b-1)
//Start = divisorsOfnum 100 100 // b starts from 100 to 0 and decreases by 1, meanwhile it find the divisors.

is_prime :: Int -> Bool
is_prime c
| c < 0 = abort "negative numbers are not prime!"
| c == 0 || c == 1 = False
| c == 2 = True // the only even prime number
| length [divisorsOfnum c (c-1)] > 2 = False // prime number has 2 divisors (1 and the number itself), if >2, it is not a prime number.
= True

//Start = is_prime 5 // True
//Start = is_prime 0 // False
//Start = is_prime 1 // False
//Start = is_prime 2 // True
//Start = is_prime 2017 // True




// Write a function that takes Int argument and checks if this number is a palindrome.
// Palindrome is a number that is the same when we read from left to right or from right to left.

numDigitsToList :: Int -> [Int]
numDigitsToList 0 = []
numDigitsToList a = numDigitsToList (a/10) ++ [a rem 10] 
// for example a=565; 565/10=56 and 565 rem 10 = 5 -> [56]++[5] = [56,5] 
// then 56/10=5 and 56 rem 10 = 6 -> [5]++[6]++[5]=[5,6,5]  
is_palindrome :: Int -> Bool
is_palindrome x
| x >= 0 && x < 10 = True // digits [0..10] are palindromes 
| (x rem 10) == (x/10) = True // condition for 2-digit number; x=55 -> 55/10=5 and 55 rem 10 = 5 -> [5]++[5]=[5,5]
| numDigitsToList x == reverse (numDigitsToList x) = True // if the list = the reverse of it, means the number is a palindrome
= False

//Start = is_palindrome 0 // True
//Start = is_palindrome 55 // True
//Start = is_palindrome 49594 // True
//Start = is_palindrome 1337 // False 