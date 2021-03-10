module helloworld
import StdEnv


//Overloading built-in functions in Clean
//We have + for Ints, Reals, and so on 
//We can also define it for lists of Ints
//Start = 2 + 2 // 4
//Start = 1.1 + 2.6 // 3.7
/*
instance + [Int] //instance of + on list of Ints 
    where 
        (+) [x] [y] = [x+y]
        (+) [x:xs] [y:ys]  = [x+y] ++ (+) xs ys
*/


//Start = [1, 2, 4, 5, 10] + [1, 2,3 , 4,5] //[2,4,7,9,15] -> the problem here is thu, the lengths must be equal
/*
instance + [Real] // replace Real with Int and we have defined + for list of reals
    where
        (+) [x] [y] = [x+y]
        (+) l1 l2 
        |  (length l1 == length l2)  || (length l1 > length l2 && not(isEmpty l2)) || (length l1 < length l2 && not(isEmpty l1)) = [hd l1 + hd l2] ++ (+) (drop 1 l1) (drop 1 l2)
        = l1 ++ l2
*/


//[1 2 3 3 4], [ 1 2 3 4] -> [1 + 1] ++ [2 + 2] ++ [3+3] ++ [3+4] ++ [] + [4]


//Start = [1.1, 2.3, 3.2, 4.0,5.0, 5.1] + [1.1]

/**Task: Overload + operator for list of Strings such that the first list has the first names of employees and the second list has the last names of employees. Your task is to concatenate all the names from the second list to the first list. */
/** 
instance + [String]
    where 
        (+) [x] [y] = [x +++ " " +++ y]
        (+) l1 l2 
        |  (length l1 == length l2)  || (length l1 > length l2 && not(isEmpty l2)) || (length l1 < length l2 && not(isEmpty l1)) = [hd l1 +++ " " +++ hd l2] ++ (+) (drop 1 l1) (drop 1 l2)
        = l1 ++ l2
*/
//Start = ["Ali", "Zsofi", "Rehan" , "Tom" , "Benidict"] + ["Ahmed", "Bolsz"] // ["Ali Ahmed","Zsofi Bolsz","Rehan","Tom","Benidict"]
//Start = ["Ahmed", "Bolsz"] + ["Ali", "Zsofi", "Rehan" , "Tom" , "Benidict"] //["Ahmed Ali","Bolsz Zsofi","Rehan","Tom","Benidict"]



///MORE GENERIC | a 
/*
instance + (a, b) |  + a & + b // + must be defined on a and b respectively 
    where
        //(+) :: (a,b) (a,b) -> (a, b) // type declaration which is not concrete - won't work
        (+) (x, y) (m, n) = (x+m, y+n) // actual instanciation

*/
//Start = (1,2) + (1, 2)

/*
instance + [a] | + a // + must be defined on a 
    where
        (+) [x] [y] = [x+y] //concrete realization
        (+) l1 l2 
        |  (length l1 == length l2)  || (length l1 > length l2 && not(isEmpty l2)) || (length l1 < length l2 && not(isEmpty l1)) = [hd l1 + hd l2] ++ (+) (drop 1 l1) (drop 1 l2)
        = l1 ++ l2
*/

//Start = [1.1, 2.3, 3.2, 4.0,5.0, 5.1] + [1.1] //[2.2,2.3,3.2,4,5,5.1]
//Start = [1, 2, 3, 4, 5] + [1, 2,3,4] ///[2,4,6,8,5]

//Start = ["s", "n"] + ["n", "x", "x"] //"+" no instance available of type {#Char}


///ALREADY DEFINED IN STDENV
/** 
instance == [a] | == a // == must be defined on a 
    where   
        (==) [x] [x] = true
        (==) l1 l2
        | length l1 <> length l2 = False
        = hd l1 == hd l2 && (==) (drop 1 l1) (drop 1 l2)


 */       
//Start = [1.1, 2.3, 3.2, 4.0,5.0, 5.1] == [1.1] //[2.2,2.3,3.2,4,5,5.1]
//Start = [1, 2, 3, 4, 5] + [1, 2,3,4] ///[2,4,6,8,5]



//Type definiation of Rational Numbers 

:: Q = {
            numerator :: Int, 
            denominator :: Int 
       }



makeRational :: Int Int -> Q
makeRational x y = Simplify{numerator= x, denominator= y}

Simplify :: Q -> Q
Simplify {numerator=num, denominator=denom} //pattern matching
| denom == 0 = abort "denominator is Zero"
| denom < 0 = {numerator = -1 * num/ GCD, denominator = -1 * denom/GCD} //dividing by gcd to simplify
= {numerator = num/GCD, denominator = denom/GCD}
    where 
        GCD = gcd num denom ///gcd from StdEnv       

/**These functions are not as intuitive as a +, -, < operator*/
sum :: Q Q -> Q 
sum x y = makeRational(x.numerator * y.denominator + y.numerator * x.denominator) (x.denominator * y.denominator)

subtract :: Q Q -> Q
subtract x y = makeRational(x.numerator * y.denominator - y.numerator * x.denominator) (x.denominator * y.denominator)

isSmallerThan :: Q Q -> Bool
isSmallerThan x y = (x.numerator * y.denominator < y.numerator * x.denominator)

isEqual :: Q Q -> Bool 
isEqual x y = x.numerator * y.denominator == y.numerator * x.denominator

reciprocal :: Q -> Q 
reciprocal x = makeRational x.denominator x.numerator

instance + Q
    where 
        + x y = makeRational(x.numerator * y.denominator + y.numerator * x.denominator) (x.denominator * y.denominator)

instance - Q 
    where 
        - x y = makeRational(x.numerator * y.denominator - y.numerator * x.denominator) (x.denominator * y.denominator)

instance * Q 
    where 
        * x y = makeRational (x.numerator * y.numerator) (y.numerator * y.denominator)
instance / Q
    where
        / x y = x * reciprocal(y)

instance < Q 
    where 
        < x y = (x.numerator * y.denominator < y.numerator * x.denominator)

instance == Q
    where 
        == x y = x.numerator * y.denominator == y.numerator * x.denominator

instance fromInt Q //if you want to cast any integer to Rational
    where 
        fromInt int = makeRational int 1 //denominator should be 1

instance zero Q
    where 
        zero = fromInt 0 //should cast 0 to rational first

instance one Q
    where 
        one = fromInt 0 //should cast 1 to rational first

instance toString Q
    where 
        toString x 
        | x.denominator == 1 = toString x.numerator //this toString is not called recursively, but a toString instance on Integers
        = toString x.numerator +++ "/" +++ toString x.denominator



//Start = toString(makeRational 3 2 / makeRational 1 2) //"0"
//Start = reciprocal (makeRational 2 3)

//Start = toString (makeRational 1 2) //"1/2"

//Start = (makeRational 1 2) < (makeRational 1 2) // False
//Start = (makeRational 1 2) == (makeRational 1 2) // True


//Start = [toString q \\ q <- [zero, makeRational 1 2 .. makeRational 3 2]] //["0","1/2","1","3/2"]

//Start = toString((makeRational 1 2) * (makeRational 1 2))

