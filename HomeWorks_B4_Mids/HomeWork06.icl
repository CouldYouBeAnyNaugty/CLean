module HomeWork06
import StdEnv

/*
Given a list of pairs of name of the person and his/her favourite food.
Make function which returns list of pairs of food and a list of people who likes it.
Note : order doesn't matter
*/


//OH, Boy!!! This was hard.

favFood :: [(String, String)] -> [(String,[String])]
favFood [] = []
favFood [(name,f):rests] = [(Fruite,[name]++[x\\(x,y) <- rests | y == Fruite])] ++ favFood[(x,y)\\(x,y)<- rests | y <>Fruite] 
where 
	Fruite = snd(hd [(name,f):rests])
//Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "apple")] // [("apple", ["Zuka", "Ahmed"]),("orange",["Beka"]),("pineapple",["Emad"])]
Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "pineapple")] // [("apple", ["Zuka"]),("orange",["Beka"]),("pineapple",["Emad","Ahmed"])]















/*
Having a list of tuples, each tuple represent a person in that form (name, age, gender)
Write a function to produce a list of two elements. the older man's name and the older woman's name
i.e : [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] -> ["Hossam", "Nani"]
Note : You can assume that the input for the gender will be "male", "female".
*/










//This is not a very good way to solve this question. :) 
AllMales :: [(String, Int, String)] -> [(String, Int, String)]
AllMales list = minimumSelection[(a,b,c)\\(a,b,c)<-list | c == "male"]
AllFemales :: [(String, Int, String)] -> [(String, Int, String)]
AllFemales list = minimumSelection[(a,b,c)\\(a,b,c)<-list | c == "female"]


minimumSelection :: [(String, Int, String)] -> [(String, Int, String)]
minimumSelection [] = []
minimumSelection [a] = [a]
minimumSelection [(a,b,c),(x,y,z):rest]
| b <= y = minimumSelection [(a,b,c):rest]
= minimumSelection [(x,y,z):rest]

findYounger :: [(String, Int, String)] -> [String]
findYounger list =  [fst3(hd(AllMales list))] ++ [fst3(hd(AllFemales list))]
  





//Start = AllMales [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")]

//Start = findYounger [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] // ["Hossam", "Nani"]
//Start = findYounger [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")] // ["Evan", "Tringa"]
//Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]

/*
Decide if a number is triangular number and write the count of levels of triangle. 
Triangular number is a number that can form a triangle.
The output should be in a tuple.
Note : if it is false the count should be -1. 
examples:
1       3         6          10          15
                                          *	
                               *          * *
                  *            * *        * * *
        *         * *          * * *      * * * *
*       * *       * * *        * * * *    * * * * *
Note : 0 is not a triangular number
*/

//This is quite affective one :D
reduction :: Int Int -> (Bool, Int)
reduction 0 x = (True, x-1)
reduction n x 
| n < 0 = (False, -1)
= reduction (n-x) (x+1) 

isTringularNum :: Int -> (Bool, Int)
isTringularNum n 
| n <= 0 = (False, -1)
= reduction n 1
 

//Start = isTringularNum -1 // (False,-1)
//Start = isTringularNum 0 // (False,-1)
//Start = isTringularNum 1 // (True,1)
//Start = isTringularNum 5 // (False,-1)
//Start = isTringularNum 10 // (True,4)
//Start = isTringularNum 666 // (True,36)