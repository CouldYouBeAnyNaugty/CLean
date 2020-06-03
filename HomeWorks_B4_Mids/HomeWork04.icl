module HomeWork04
import StdEnv 

//For every sublist, eliminates its elements
//Until the current element is a prime number
//After that, multiply each number by 5
//And remove all elements that end with 0. I.e. divisible by 10.

isPrime :: Int -> Bool
isPrime n 
| n < 2 = False
= and[n rem x <> 0 \\x <-[2..toInt (sqrt(toReal n))]]


f1 :: [[Int]] -> [[Int]]
f1 [a:b] = [map (\e -> e*5) (filter (\m -> isOdd m)(dropWhile (not o isPrime) x))\\x<-[a:b]] 


//Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[15],[35,25,15],[15,25,35,45],[],[]]
//Start = f1 [[1], [4], [2]] // [[],[],[]]
//Start = f1 [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[25,35,45],[35,45],[],[25]]



/*______________________________________________________________________________________________________________________________________*/


//Write function that returns length of a list
//You must use foldr or foldl



///Using recursion
lengthWithRec :: [Int] -> Int 
lengthWithRec [] = 0
lengthWithRec [a:b] = 1 + lengthWithRec b



///Using Foldl
//NOTE: in lamda expression the right hand side element is from the list
//for the first time the element outside the list would be zero, next it's one, next it's two and so on.. and so forth 

/*
   0 [first, second, third, foruth, fifth..]
   1 [second, third, fourth, fifth]
   2 [third, fourth, fifth]
   3 [forth, fifth]
   4 [fifth]
   5 []
*/
//then we return the 5
//NOTE: This is not the actual way how foldl works, but it's kinda a mind-picture

f2 :: [Int] -> Int
f2 list = foldl (\elementOutSideTheList elementfromTheList -> elementOutSideTheList + 1) 0 list


///Using foldr 
//NOTE: in lambda expression the left hand side element is from the list
//Element outside the list is zero in this case
f21 :: [Int] -> Int
f21 list = foldr (\elementFromTheList elementOutSideTheList -> elementOutSideTheList + 1) 0 list 

//Start = f2 [] // 0
//Start = f2 [1,2,3] // 3
//Start = f2 [1] // 1
//Start = f21 (take 100 [1..]) // 100





/*________________________________________________________________________________________________________________________________________*/





// Define function "reverse" using foldr
//Using Recursion 
revRec :: [Int] -> [Int]
revRec [] = []
revRec [a:b] = revRec b ++ [a]

//Using foldr
f3 :: [Int] -> [Int]
f3 list = foldr (\fromList outSideList -> outSideList ++ [fromList]) [] list //outside list is already an empty list so you can't do [OutSideList] ++ ... That's making it listOflists 

//Using foldl
f31 :: [Int] -> [Int]
f31 list = foldl (\outSideList fromList -> [fromList] ++ outSideList) [] list //Order of the events chages from left to right and from right to left. One start from the first element and the other start from the last element 

//Start = f3 [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = f3 [] // []
//Start = f3 [1] // [1]


/*__________________________________________________________________________________________________________________________________________*/