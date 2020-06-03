module ONE
import StdEnv


//Task understanding takes time!
//Problem solving takes mind

// 1. write a function duplicates which checks if there are neighbour duplicates in a list
duplic :: [Int] -> Bool
duplic [] = False
duplic [a] = False
duplic [a,b:c]
| a == b = True 
= duplic [b:c]

//Start = duplic [1, 1] //Ture
//Start = duplic [2] //False
//Start = duplic [1, 2, 3, 4, 5, 6, 7, 8, 9] //False
//Start = duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] //True


// 2. write a function that removes neighbour duplicates in a list
duplicrem :: [Int] -> [Int] 
duplicrem [] = []
duplicrem [a] = [a]
duplicrem [a,b:c]
| a == b = duplicrem(dropWhile (\x -> x == a) c) 
= [a: duplicrem [b:c]]

//Start = duplicrem [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] //[1,0,5,6,7,5,8,0,5]


// 3. transform the sub-sub lists into one list of sublists
f :: [[[Int]]] -> [[Int]]
f listOflistOfList = flatten listOflistOfList

//Start = f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]] 
// result : [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]


// 4.  generate the following list [(1,1),(2,2),(3,3),(4,4),(5,5)]
l1 :: [(Int, Int)]
l1 = [(x,y)\\x<-take 5 [1..] & y <- take 5 [1..]]

//Start = l1 //[(1,1),(2,2),(3,3),(4,4),(5,5)]


//5. generate [(1,2,3),(2,4,6),(3,6,9),(4,8,12),(5,10,15)]
l2 :: [(Int, Int, Int)]
l2 = take 5 [(x,2*x,3*x)\\x<-[1..]]

//Start = l2