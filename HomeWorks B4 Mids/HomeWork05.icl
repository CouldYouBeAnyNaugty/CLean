module HomeWork05
import StdEnv

// find the sum of all odd squares that are smaller than 10,000
f1 :: Int
f1 = sum[x*y\\x<-take 100 [1..] & y<-take 100 [1..] | isOdd(x*y)] /// coz 100 * 100 = 10,000 
 //Start = f1 // 166650



/*________________________________________________________________________________________________________________________________*/


// Given list of integers, find number of
// different sums of continuous subsequences
// Example: [1,2,3,4] has 10 continuous subsequences
// [1], [2], [1,2], [3], [2,3], [1,2,3], [4], [3,4], [2,3,4], [1,2,3,4]
// And there sums are [1,2,3,3,5,6,4,7,9,10], from which we need to remove
// duplicates and we get [1,2,3,4,5,6,7,9,10] - Hence we ave 9 different sums
// https://stackoverflow.com/questions/3988575/what-does-this-definition-of-contiguous-subsequences-mean


///Recursion is so freaking cool

ContSeqGenerator :: [Int] -> [Int]
ContSeqGenerator [a] = [a]
ContSeqGenerator list = [sum list] ++  ContSeqGenerator (init list)
 
conSeq :: [Int] -> [Int]
conSeq [] = []
conSeq list = removeDup(ContSeqGenerator list ++ conSeq (tl list))

f2 :: [Int] -> Int
f2 list = length(conSeq list)





/*___________________________________________________________________________________________________________________________________*/

//Start = f2 [1,2,3,4] // 9
//Start = f2 [] // 0
//Start = f2 [3] // 1
//Start = f2 [1,-3,2,-4,-3,1,7,6,2,8,9] // 34
//Start = f2 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // 166



// Given the list of integers. Find the longest
// continues subsequence which does not have
// duplicates. If there are several answers
// return rightmost one.
// https://stackoverflow.com/questions/3988575/what-does-this-definition-of-contiguous-subsequences-mean



exists :: Int [Int] -> Bool
exists n [] = False
exists n [x:xs] = n == x || exists n xs 

existsMain :: [Int] -> Bool
existsMain [] = False 
existsMain [n] = False 
existsMain [x:xs] = exists x xs || existsMain xs

ExistDups :: [[Int]] -> [[Int]]
ExistDups [] = []
ExistDups [firstList: restLists]
| existsMain firstList = ExistDups restLists 
= [firstList : ExistDups restLists]

//abcd is nothing but maximum selection
abcd :: [[Int]] -> [Int]
abcd [] = []
abcd [x] = x
abcd [x:xs]
| lnX > h = abcd ([x] ++ tail)
= abcd ([hd(xs)] ++ tail)
where 
	lnX = length x
	h = length (hd(xs))
	tail = tl(xs)

ContinuSeqGenerator :: [Int] -> [[Int]]
ContinuSeqGenerator [a] = [[a]]
ContinuSeqGenerator list = [list] ++  ContinuSeqGenerator (init list)
 
conSequen :: [Int] -> [[Int]]
conSequen [] = [[]]
conSequen list = ExistDups(ContinuSeqGenerator list  ++ conSequen (tl list))


f3 :: [Int] -> [Int]
f3 list = abcd (conSequen list)

//Start = f3([1,1,2,4,5,3,2,6,3]) // [4,5,3,2,6])
// Start = f3 [] // []
// Start = f3 [3] // [3]
// Start = f3 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]





/*______________________________________[Last one is gonna kick your ass to grasp on]________________________________________________________________*/
//I am the worst Programmar ever