module hw9
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

minRoot :: Tree Int
minRoot = (Node 4 (Node 10 (Node 11 Leaf Leaf)(Node 16 Leaf Leaf)) (Node 22 (Node 15 Leaf Leaf) Leaf))

minMostLeftLeaf :: Tree Int
minMostLeftLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

minMostRightLeaf :: Tree Int
minMostRightLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf (Node 1 Leaf Leaf)) ))

minNode :: Tree Int
minNode = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node -12 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

/*
1. Given a Tree of integers, find the minimum value of all the nodes.
*/

treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = [x] ++ treeToList l ++ treeToList r

minTree :: (Tree Int) -> Int
minTree (Node x l r) = minList (treeToList (Node x l r))

//Start = minTree minRoot // 4
//Start = minTree minMostLeftLeaf //2
//Start = minTree minMostRightLeaf //1
//Start = minTree minNode // -12

:: Major = CS | Math | Physics
:: Course = {name::String, major :: Major, credits:: Int}

Programming::Course
Programming = {name="Programming",major=CS, credits =5}
Analysis::Course
Analysis = {name="Analysis",major=Math, credits =4}
Relativity::Course
Relativity = {name="Relativity",major=Physics,credits=6}
Functional::Course
Functional = {name="Functional",major=CS,credits=5}
Basic::Course
Basic = {name="Basic",major=Math,credits=3}
Thermo_Dynamics::Course
Thermo_Dynamics = {name="Thermo_Dynamics",major=Physics,credits=4}
Astronomy::Course
Astronomy = {name="Astronomy",major=Physics,credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods",major=Math,credits=4}
Compilers::Course
Compilers = {name="Compilers",major=CS,credits=4}

/*
2. Given a list of Courses that a student has taken,
find the credits they earned if
for each CS major course, he gets 3 more credits
*/

instance == Major
where 
   == CS CS = True
   == Math Math = True
   == Physics Physics = True
   == _ _ = False
   
instance == Course
where
   == a b = a.major == b.major

aux :: Course -> Int
aux crs
| crs.major == CS = crs.credits + 3
= crs.credits 

creds :: [Course] -> Int
creds [] = 0
creds crs = sum ([aux x \\ x <- crs])

//Start = creds [Compilers, Astronomy, Basic] // 16
//Start = creds [Thermo_Dynamics, Relativity, Numerical_Methods] // 14
//Start = creds [Compilers, Functional, Programming] //23
//Start = creds [] // 0

//:: Tree a = Node a (Tree a) (Tree a ) | Leaf

treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf) (Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf) (Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treec = (Node Analysis Leaf (Node Programming Leaf (Node Astronomy Leaf (Node Basic Leaf (Node Compilers Leaf (Node Thermo_Dynamics Leaf (Node Numerical_Methods Leaf (Node Functional Leaf Leaf))))))))
/*
3. Given a tree of Courses, give back all the CS Courses
whose both children are either Physics courses or Leaf-s.
*/

cs :: (Tree Course) -> Bool
cs Leaf = False
cs (Node x left right) = x.major == CS

phy :: (Tree Course) -> Bool
phy Leaf = True
phy (Node x left right) = x.major == Physics

getphysics :: (Tree Course) -> [Course]
getphysics Leaf = []
getphysics (Node x l r)
| cs (Node x l r) == True && (phy l == True && phy r == True) = [x] ++ getphysics l ++ getphysics r
= getphysics l ++ getphysics r

/*getphysics :: (Tree Course) -> [Course]
getphysics Leaf = []
getphysics (Node x l r) = [a \\ a <- (treeToList (Node x l r)) | a.major == CS] */

//Start = getphysics treea // [(Course "Programming" CS 5),(Course "Compilers" CS 4)]
//Start = getphysics treeb // [(Course "Functional" CS 5),(Course "Programming" CS 5)]
//Start = getphysics treec // [(Course "Programming" CS 5),(Course "Compilers" CS 4),(Course "Functional" CS 5)]