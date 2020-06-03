module HomeWork01
import StdEnv


/*
Write a function that, given a grade as a Real,
will return the respective grade as an Int.
The grade cutoffs are as follows:
1: 0% - 50%
2: 50% - 60%
3: 61% - 70%
4: 71% - 85%
5: 86% - 100%
Return -1 for any invalid input (such as a negative grade).
*/

myGrade :: Real -> Int
myGrade num 
| num < 0.0 = -1
| num >= 0.0 && num < 51.0 = 1
| num >= 50.0 && num < 61.0 = 2
| num >= 61.0 && num < 71.0 = 3
| num >= 71.0 && num < 86.0 = 4
| num >= 86.0 && num <= 100.0 = 5

//Start = myGrade 25.5 //1
//Start = myGrade 90.24 //5
//Start = myGrade -3.42 //-1




/*
Write a function that will take a number (Int)
and return the respective month (String).

For example: input of 1 should output January.
Return "Invalid month" (or something similar)
for any invalid input (such as negative numbers or
numbers bigger than 12.)
*/
monthString :: Int -> String
monthString n
| n <  1 || n > 12 = "HEY, WE HAVE ONLY 12 months, ARE YOU STUPID??!!"
| n == 1 = "January"
| n == 2 = "February"
| n == 3 = "March"
| n == 4 = "April"
| n == 5 = "May"
| n == 6 = "June"
| n == 7 = "July"
| n == 8 = "August"
| n == 9 = "September"
| n == 10 = "October"
| n == 11 = "November"
| n == 12 = "December"

//Start = monthString 4 //"April"
//Start = monthString 10 //"October"
//Start = monthString -1 //"Not a valid month"
//Start = monthString 42 //"Not a valid month"






/*
Write a function that will calculate the total
of your dining bill after including gratuity.
Take the subtotal as a Real and the gratuity percentage
as a Real.
For example:
If the subtotal is 10.00, and gratuity is 0.15, then
the total is 10.00 + (0.15 * 10.00) = 11.50
*/

myBill :: Real Real -> Real
myBill sub grat = sub + (grat * sub)

//Start = myBill 10.00 0.15 //11.50
//Start = myBill 9001.00 0.08 //9721.08