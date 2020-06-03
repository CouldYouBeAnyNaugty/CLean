module ZERO
import StdEnv


///should take at most 25 minutes to compute every single of these otherwise you are more dumb than I am xD xD xD


// 1. Is x a power of 10?
iprod :: Int -> Bool 
iprod n = or[n==(10^x)\\x <- [0..n]]
//Start = iprod 100 // False


// 2. Write a function to compute 1+2+3+4+5+6+...+n = (n*(n+1))/2
//we can use list comprehension for this question but mathematical formula rocks!!!!!
isum :: Int -> Int
isum n = (n*(n+1))/2

//Start = isum 4 // 10
//Start = isum 100 //5050


// 3. Compute the number of sublists in a list
nrlist :: [[Int]] -> Int
nrlist list = foldr (\x y = 1 + y) 0 list

//Start = nrlist [[1, 2, 3], [3, 4], [5, 7, 8, 9]] //3
//Start = nrlist [[x]\\x<-[1..100]] //100


//4. Compute the number of all the elements in a list
nrelist :: [[Int]] -> Int
nrelist list = length (flatten list) 

//Start = nrelist [[1, 2, 3], [3, 4], [5, 7, 8, 9]] ///9
//Start = nrelist [[x]\\x<-[1..100]] //100

//5. Keep the first n and the last n elements of a list
cut :: [Int] Int -> [Int]
cut list n = (take n list) ++ reverse(take n (reverse list))

//Start = cut [1, 2, 3, 4, 5, 6, 7, 8, 9] 3 //[1,2,3,7,8,9]


//6. Delete the 0 element from a list

delzero :: [Int] -> [Int]
delzero [] = []
delzero [a:b]
| a==0 = delzero b
= [a: delzero b]

//Start = delzero [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] 


// 7. compute  1*2 + 2*3 + ... + n*n+1 
sums :: Int -> Int
sums num = sum[x*y\\x<-[1..num] & y<-[2..num+1]]



//Start = sums1 2 //8
//Start = sums1 5 //70


// 8. filter the even elements of a list
g :: [Int] -> [Int]
g list = filter isEven list 
//Start = g [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] ///[0,0,0,6,0,0,0,8,0,0,0,0]
 


// 9. compute the half of the elements of a list using map
halfs :: [Int] -> [Int]
halfs list = map (\x -> x/2) list

//Start = halfs [1, 2, 3, 4, 5, 6, 7, 8, 9] ///[0,1,1,2,2,3,3,4,4]
 
 

// 10. delete the last element of the sublists
dellast :: [[Int]] -> [[Int]] 
dellast list = [init x\\ x<- list ]

//Start = dellast [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]