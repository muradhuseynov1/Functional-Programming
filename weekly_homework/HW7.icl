module HW7
import StdEnv

/* 1. Write a function that takes an array of integers and gives back a tuple that contains:
(the integer in the array, a boolean value)
the boolean value tells if when cutting the integer in half it consists of
the same number, e,g, 2020 -> 20 20 so it keeps it but 2008 -> 20 08 it doesn't.
*/
numDigitsList :: Int -> [Int]
numDigitsList num
| num < 0 = abort "must be a non-negative integer"
| num < 10 = [num]
= numDigitsList (num/10) ++ [num rem 10]

cutCheck :: Int -> Bool
cutCheck a = a rem 10^b == a / 10^b
where
    b = length (numDigitsList a)/2

arrayToList :: {t} -> [t]
arrayToList arr = [x \\ x <-: arr]

listToArray :: [t] -> {t}
listToArray lst = {y \\ y <- lst}

toTuple :: {Int} -> {(Int,Bool)}
toTuple arr1 = listToArray [(z, cutCheck z) \\ z <- (arrayToList arr1)]
	
//Start = toTuple {} // {}
//Start = toTuple {100, 2020, 1919} // {(100,False),(2020,True),(1919,True)}
//Start = toTuple {312, 1001, 1010} // {(312,False),(1001,False),(1010,True)}

/* 2. Define a Person record which contains name and height two fields,
with type of String and Real respectively. Write a function which takes a person
and a certain height, if the person is taller than 1.70, subtract their height by
1%.

*/

::Person1 = { name1 :: String, tall :: Real}
John::Person1
John={name1 = "John", tall= 1.78}
Mike::Person1
Mike={name1 = "Mike", tall= 1.58}
Lily::Person1
Lily={name1 = "Lily", tall= 1.85}

ChangeHeight :: Person1 -> Person1
ChangeHeight p
| p.tall > 1.70 = {p&tall = p.tall - (p.tall * 0.01)}
= {p&tall = p.tall}

//Start = ChangeHeight John // (Person1 "John" 1.7622)
//Start = ChangeHeight Mike // (Person1 "Mike" 1.58)
//Start = ChangeHeight Lily // (Person1 "Lily" 1.8315)

::Person={name::String, mass::Real, height::Real, bmi::Real}
Rose::Person
Rose={name="Rose", mass=147.71, height=1.72, bmi=0.0}
Jack::Person
Jack={name="Jack", mass=158.73, height=1.93, bmi=0.0}
Emilia::Person
Emilia={name="Emilia", mass=121.25, height=1.60, bmi=0.0}
Leo::Person
Leo={name="Leo", mass=85.98, height=1.75, bmi=0.0}
Grace::Person
Grace={name="Grace", mass=112.43, height=1.65, bmi=0.0}
Harry::Person
Harry={name="Harry", mass=169.76, height=1.80, bmi=0.0}

/* 3.
Given an array of Persons, write a function that calculates the BMI of each Person
BMI: body mass index = m / h^2
m = mass (in kilograms)
h = height (in meters)
note: the mass given in the records are in pounds, you need to convert before using the formula
hint: 1 pound = 0.453592kg
*/

calcBMI :: {Person} -> {Person}
calcBMI person = {{name = x.name, mass = toReal(toInt(x.mass * 0.453592)), height = toReal(toInt(x.height * 100.0)), bmi = (toReal(toInt(x.mass * 0.453592)) / x.height^2.0)} \\ x <-: person}

//Start = calcBMI {Rose,Jack,Emilia} // {(Person "Rose" 67 172 22.6473769605192),(Person "Jack" 72 193 19.3293779698784),(Person "Emilia" 55 160 21.484375)}
//Start = calcBMI {Leo,Grace,Harry} // {(Person "Leo" 39 175 12.734693877551),(Person "Grace" 51 165 18.732782369146),(Person "Harry" 77 180 23.7654320987654)}
//Start = calcBMI {} // {}