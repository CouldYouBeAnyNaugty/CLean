module HomeWork03
import StdEnv

//Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order. 


//if num rem 2 == 0, We find our first prime => 2
//then (num/2) rem 2 == 0, factor is the same 2
//then ((num/2)/2) rem <> = 0, check it with three and so on and so forth until you reach num itself

/*

__2__|___36_____  -> 36 rem 2 == 0 .... devisor remain the same for the next step
__2__|___18_____  -> 18 rem 2 == 0 .... devisor remain the same for the next step
__3__|____9______ -> 9 rem 2 == 0 .... increase devisor -> 9 rem 3 == 0 and then do the same for the upcomming steps 
__3__|____3______
_____|____1_____



_2_|_46____
_23|_23____   -> here all elements [2..22] are not devisors of 23, so we incremented from 2 to 23, and we found 23.
___|_1_____



This way we are deviding and filtering out the prime factors both at the same time.
*/  

factorFunction :: Int Int -> [Int]
factorFunction n x 
| n == x = [n]
| n rem x == 0 = removeDup(sort([x] ++ factorFunction(n/x) x))
= removeDup(sort(factorFunction n (x+1)))

callfact :: Int -> [Int]
callfact x = factorFunction x 2 //This is the call to factorFunction 

primeFactors :: Int -> [Int]
primeFactors x 
| x <= 1 = []
= callfact x   
//Start = primeFactors 0 // []
//Start = primeFactors -5 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 24 // [2,3]




// Write a function that takes a list of numbers and
// breaks it into two lists, which contain even and 
// odd elements from the original list.
// For example: [3,5,6,8,7,9] -> [ [3,5,7,9], [6,8] ]

splitList :: [Int] -> [[Int]]
splitList list = [filter (isOdd) list] ++ [filter (isEven) list]
//Start = splitList [56,3,87,5,234,5,0,-4] //[[3,87,5,5],[56,234,0,-4]]
//Start = splitList [1] //[[1],[]]
//Start = splitList [420] //[[],[420]]
//Start = splitList []//[[],[]]








// Write a function that takes a list of integers and gives their least common multiple.


lcmList :: [Int] -> Int
lcmList [] = 0
lcmList [a] = a
lcmList [a,b] = lcm a b
lcmList [a,b:c] = lcm (lcm a b) (lcmList c)  
//Start = lcmList [1, 10, 400453, 58359, 5389538] // 89966928901863090
//Start = lcmList [] // 0
//Start = lcmList [5] // 5
//Start = lcmList [4,5] // 20
//Start = lcmList [1, 2, 3] // 6