module HomeWork02
import StdEnv


// Write a function that takes an Int 'n' and
// generates a list of fibonacci numbers,
// which are less than or equal to 'n'. 
// https://en.wikipedia.org/wiki/Fibonacci_number


///A very bad approach 
Fib :: Int -> Int
Fib 0 = 0
Fib 1 = 1
Fib n = Fib (n-1) + Fib (n-2)

fibo :: Int -> [Int]
fibo 1 = [0,1,1]
fibo n 
| n < 0 = [-1]
= takeWhile (\x -> x <= n) [Fib n \\ n <-[0..n]]

//Start = fibo 0 // [0]
//Start = fibo -1 // []
//Start = fibo 15 // [0,1,1,2,3,5,8,13]
//Start = fibo 1 // [0,1,1]
//Start = fibo 55 // [0,1,1,2,3,5,8,13,21,34,55]




// Write a function that takes Int 'n' and
// checks if 'n' is a prime number or not.
// Please handle the case of negative numbers.
// Note: 0 and 1 are not prime numbers.

/// We can do it without using list comrehension, but This is the most beautiful one you can find out there.
isPrime :: Int -> Bool
isPrime num 
| num < 2 = False 
= and[num rem x <> 0\\x<-[2..num-1]]

//Start = isPrime 5 // True
//Start = isPrime 0 // False
//Start = isPrime 1 // False
//Start = isPrime 28736 // False
//Start = isPrime 8 //Flase

// Write a function that takes Int 'n' and
// checks if 'n' is a palindrome or not.
// A palindrome is a number which reads
// identicaly both forwards and backwards.


//Put it in the list, check whether the reverse(list) == list and if it is the number we have is a plaindrome
makeList :: Int -> [Int]
makeList 0 = []
makeList n 
| n < 10 = [n]
= [n rem 10 : makeList(n/10)] 

isPalindrome :: Int -> Bool
isPalindrome num 
| num < 0 = False
= makeList num == reverse (makeList num) 


//Start = isPalindrome 0 // True
//Start = isPalindrome 55 // True
//Start = isPalindrome 49594 // True
//Start = isPalindrome 1337 // False
//Start = isPalindrome -57975 // False