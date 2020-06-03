module All_B4_Mid_Exams 
import StdEnv 



//These are all the progress task that I could get from a friend. 

//Define a function which finds the maximum of two numbers
maximum :: Int Int -> Int
maximum x y 
| x > y = x
= y
//Start = maximum 2 10


/*____________________________________________________________________________________________________________________________*/






/*
	Given a list of Tuples of 3 of Integers, keep only the Pythagorean triples.
	A Pythagorean triple consists of three positive integers a, b, and c, such that a^2 + b^2 = c^2.
*/
pyth :: [(Int,Int,Int)] -> [(Int,Int,Int)]
pyth list = [(a,b,c) \\ (a,b,c)<- list | a^2 + b^2 == c^2]


//Start = pyth [(1,2,3),(3,4,5),(6,7,8)]//[(3,4,5)]
//Start = pyth [(6,8,10),(5,12,13),(1,1,1)] //[(6,8,10),(5,12,13)]
//Start = pyth [(1,1,1),(2,3,4),(9,10,20)] //[]

/*____________________________________________________________________________________________________________________________________________*/


/*
Given a list of numbers keep only the odd numbers bigger than 10 using higher-order functions.
*/
big10 :: [Int] -> [Int]
big10 list = filter (\x -> isOdd x && x>10) list

//Start = big10 [1..100] //[11,13..99]



/*_______________________________________________________________________________________________________________________________________________*/

//Write a function which takes a [Int] and checks 
//if the first two integers are equal to the last two
//Important note: if the list has less than 2 elements return False
firstLast::[Int]->Bool
firstLast list 
| length list < 2 = False
= take 2 list == take 2 (reverse list)
//Start = firstLast [1..10]//False
//Start = firstLast [1,4,3,4,3,1,4]//False
//Start = firstLast [1,2,1]//True
//Start = firstLast [1,1]//True
//Start = firstLast [1]//False



/*_________________________________________________________________________________________________________________________________________________*/




//Given a list of integers.
//Write a code which will substitute every integer greater than 5 with the character 'g' and every integer less or equal to 5 with 's'
subst :: [Int] -> [Char]
subst list = ['s'\\x<-list | x<=5] ++ ['g'\\x<- list | x>5]
//Start=subst [1..10] //['s','s','s','s','s','g','g','g','g','g']
//Start = subst [-100..100]



/*____________________________________________________________________________________________________________________________________________________*/


/*Given a list of Int.
Write a function which will calculate the sum of the squares of 
only the even numbers from that list*/
sumPowerEven :: [Int] -> Int
sumPowerEven list = sum[x^2\\x<-list | isEven(x^2)]

//Start = sumPowerEven [1..10]
//Start=sumPowerEven [1..5]//20 =2^2+4^2
//Start=sumPowerEven [2,2,2,2]//16
//Start=sumPowerEven [1,3,5]//0
//Start=sumPowerEven []//0


