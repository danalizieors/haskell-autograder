import Text.Printf

import Impl


-- colored printing functionality
data Color = Normal | Red | Green

codeFor Normal = 0
codeFor Red = 31
codeFor Green = 32

stringFor color = "\x1b[" ++ show (codeFor color) ++ "m"

paint color string
    | coloredPrint = stringFor color ++ string ++ stringFor Normal
    | otherwise = string


-- methods and data structures used for verifying the solutions and displaying the results
data Problem = Problem {
    title :: String,
    description :: String,
    score :: Double,
    tests :: [Test]
} deriving (Show)

data Test = Test {
    name :: String,
    function :: String,
    expected :: String,
    result :: String
} deriving (Show)


line = "--------------------------------------------------------------------------------"

displayExam exam =
    let solved = calculateSolved exam
        given = calculateGiven exam
        scored = calculateScored exam
        maximum = calculateMaximum exam
        result = displayResult solved given scored maximum
    in intro ++ displayProblems exam ++ outro result

intro = let title = "--== 14th week Haskell test ==--"
        in printf "\n\n%s\n                      %s\n%s\n\n\n" line title line

outro result = printf "%s\n    Results: %s\n%s\n\n\n" line result line

displayResult solved given scored maximum =
    displayScore (resultColor scored maximum) solved given scored maximum

displayProblems problems = mapConcat displayProblem problems

displayProblem problem@(Problem title description score tests) =
    let solved = solveTests tests
        given = length tests
        scored = scoreProblem problem
        maximum = score
        label = displayLabel solved given scored maximum 
        
        header = printf "%s    %s\n\n" label title
        body = printf "%s\n\n%s\n\n" description (displayTests tests)
    in header ++ body

displayLabel solved given scored maximum =
    displayScore (labelColor scored maximum) solved given scored maximum

displayTests :: [Test] -> String
displayTests tests = mapConcat displayTest tests

displayTest test@(Test name function expected result) =
    let correct = solveTest test
        label = if correct then paint Green "PASS" else paint Red "FAIL"
        arrow = (paint Green "=>")
        corrected = (paint Green expected)
        mistake = if correct then "" else printf "| %s" (paint Red result)

        header = printf "    %s  %s\n" label name
        body = printf "          %s %s %s %s\n\n" function arrow corrected mistake
    in header ++ body

displayScore color solved given scored maximum =
    paint color $ printf "[%d/%d] | [%.2f/%.2f]" solved given scored maximum

labelColor solved given
    | solved == given = Green
    | otherwise = Red

resultColor scored maximum
    | maximum / 2 <= scored = Green
    | otherwise = Red

calculateSolved problems = sum $ map (solveTests . tests) problems
calculateGiven problems = sum $ map (length . tests) problems
calculateScored problems = sum $ map scoreProblem problems
calculateMaximum problems = sum $ map score problems

scoreProblem (Problem _ _ score tests) =
    let solved = fromIntegral $ solveTests tests
        given = fromIntegral $ length tests
    in score * solved / given
           
solveTests = length . filter solveTest
solveTest (Test _ _ expected result) = expected == result

mapConcat f = foldr (++) [] . map f


main = putStr $ displayExam $ exam


-- define the problems and the soultions
exam =
    [
        Problem {
            title = "Camping basics",
            description = "In this exercise you will implement basic operations on a dictionary. The dictionary is represented as a list of tuples, each tuple containing a key and a value, for example: '[(key1, value1), (key2, value2), (key3, value3)]'.",
            score = 30,
            tests = [
                Test {
                    name = "Add an entry - empty dictionary",
                    function = "addEntry ('a', 1) []",
                    result = show $ addEntry ('a', 1) [],
                    expected = "[('a',1)]"
                },
                Test {
                    name = "Add an entry - one entry",
                    function = "addEntry ('a', 1) [('b', 2)]",
                    result = show $ addEntry ('a', 1) [('b', 2)],
                    expected = "[('a',1),('b',2)]"
                },
                Test {
                    name = "Add an entry - many entries",
                    function = "addEntry ('a', 1) [('b', 2), ('c', 3), ('d', 4)]",
                    result = show $ addEntry ('a', 1) [('b', 2), ('c', 3), ('d', 4)],
                    expected = "[('a',1),('b',2),('c',3),('d',4)]"
                },
                Test {
                    name = "Remove newest entry - empty dictionary",
                    function = "removeNewestEntry []",
                    result = show $ removeNewestEntry [],
                    expected = "[]"
                },
                Test {
                    name = "Remove newest entry - one entry",
                    function = "removeNewestEntry [('a',1)]",
                    result = show $ removeNewestEntry [('a',1)],
                    expected = "[]"
                },
                Test {
                    name = "Remove newest entry - many entries",
                    function = "removeNewestEntry [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ removeNewestEntry [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[('b',2),('c',3),('d',4)]"
                },
                Test {
                    name = "Remove oldest entry - empty dictionary",
                    function = "removeOldestEntry []",
                    result = show $ removeOldestEntry [],
                    expected = "[]"
                },
                Test {
                    name = "Remove oldest entry - one entry",
                    function = "removeOldestEntry [('a',1)]",
                    result = show $ removeOldestEntry [('a',1)],
                    expected = "[]"
                },
                Test {
                    name = "Remove oldest entry - many entries",
                    function = "removeOldestEntry [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ removeOldestEntry [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[('a',1),('b',2),('c',3)]"
                },
                Test {
                    name = "Get all keys - empty dictionary",
                    function = "getAllKeys []",
                    result = show $ getAllKeys [],
                    expected = show ""
                },
                Test {
                    name = "Get all keys - one entry",
                    function = "getAllKeys [('a',1)]",
                    result = show $ getAllKeys [('a',1)],
                    expected = show "a"
                },
                Test {
                    name = "Get all keys - many entries",
                    function = "getAllKeys [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ getAllKeys [('a',1),('b',2),('c',3),('d',4)],
                    expected = show "abcd"
                },
                Test {
                    name = "Get all values - empty dictionary",
                    function = "getAllValues []",
                    result = show $ getAllValues [],
                    expected = "[]"
                },
                Test {
                    name = "Get all values - one entry",
                    function = "getAllValues [('a',1)]",
                    result = show $ getAllValues [('a',1)],
                    expected = "[1]"
                },
                Test {
                    name = "Get all values - many entries",
                    function = "getAllValues [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ getAllValues [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[1,2,3,4]"
                }
            ]
        },
        Problem {
            title = "Navigating in the woods",
            description = "More exercises featuring operations on dictionaries. You should be concerned about duplicated entries (entries having the same keys) only when implementing the last function.",
            score = 40,
            tests = [
                Test {
                    name = "Remove entry with key - empty dictionary",
                    function = "removeEntry 'a' []",
                    result = show $ removeEntry 'a' [],
                    expected = "[]"
                },
                Test {
                    name = "Remove entry with key - key not found",
                    function = "removeEntry 'f' [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ removeEntry 'f' [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[('a',1),('b',2),('c',3),('d',4)]"
                },
                Test {
                    name = "Remove entry with key - key found",
                    function = "removeEntry 'c' [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ removeEntry 'c' [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[('a',1),('b',2),('d',4)]"
                },
                Test {
                    name = "Search entry with key - empty dictionary",
                    function = "searchEntry 'a' []",
                    result = show $ searchEntry 'a' [],
                    expected = "Nothing"
                },
                Test {
                    name = "Search entry with key - key not found",
                    function = "searchEntry 'f' [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ searchEntry 'f' [('a',1),('b',2),('c',3),('d',4)],
                    expected = "Nothing"
                },
                Test {
                    name = "Search entry with key - key found",
                    function = "searchEntry 'c' [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ searchEntry 'c' [('a',1),('b',2),('c',3),('d',4)],
                    expected = "Just 3"
                },
                Test {
                    name = "Remove entries with duplicate keys - empty dictionary",
                    function = "optimize []",
                    result = show $ optimize [],
                    expected = "[]"
                },
                Test {
                    name = "Remove entries with duplicate keys - no duplicate keys",
                    function = "optimize [('a',1),('b',2),('c',3),('d',4)]",
                    result = show $ optimize [('a',1),('b',2),('c',3),('d',4)],
                    expected = "[('a',1),('b',2),('c',3),('d',4)]"
                },
                Test {
                    name = "Remove entries with duplicate keys - duplicate entries",
                    function = "optimize [('a',1),('a',1),('b',2),('a',1),('b',2),('c',3),('d',4),('a',1),('d',4),('d',4)]",
                    result = show $ optimize [('a',1),('a',1),('b',2),('a',1),('b',2),('c',3),('d',4),('a',1),('d',4),('d',4)],
                    expected = "[('a',1),('b',2),('c',3),('d',4)]"
                },
                Test {
                    name = "Remove entries with duplicate keys - duplicate keys",
                    function = "optimize [('a',1),('a',3),('b',2),('a',2),('b',1),('c',3),('d',4),('a',2),('d',3),('d',3)]",
                    result = show $ optimize [('a',1),('a',3),('b',2),('a',2),('b',1),('c',3),('d',4),('a',2),('d',3),('d',3)],
                    expected = "[('a',1),('b',2),('c',3),('d',4)]"
                }
            ]
        },
        Problem {
            title = "Two maps, we are lost",
            description = "In this exercise you will work with two dictionaries at once. Each dictionary can be considered as well-formed, they do not contain entries with the same key. However by working with two dictionaries at once you may find conflicting entries, entries which have the same key. Each implemented function will manage these conflicts differently. First, you will write the function subtract, which will remove all the conflicting entries from the first dictionary. Next, you will implement the merge function, which will add the left dictionary to the not conflicting entries of the second dictionary (keeps the conflicting entries from the first dictionary). Finally, you will implement merge with, which will use a function given as a parameter to decide the kept value for conflicting entries.",
            score = 30,
            tests = [
                Test {
                    name = "Subtract the second dictionary from the first - common entities",
                    function = "subtractDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)]",
                    result = show $ subtractDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)],
                    expected = "[('a',1),('b',2),('c',3)]"
                },
                Test {
                    name = "Subtract the second dictionary from the first - common keys",
                    function = "subtractDictionary [('a',1),('b',2),('c',3),('e',5),('d',4)] [('d',6),('e',7),('f',8),('g',9)]",
                    result = show $ subtractDictionary [('a',1),('b',2),('c',3),('e',5),('d',4)] [('d',6),('e',7),('f',8),('g',9)],
                    expected = "[('a',1),('b',2),('c',3)]"
                },
                Test {
                    name = "Merge two dictionaries, keep conflicting entities from first - common entities",
                    function = "mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)]",
                    result = show $ mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)],
                    expected = "[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7)]"
                },
                Test {
                    name = "Merge two dictionaries, keep conflicting entities from first - common keys",
                    function = "mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('e',7),('d',6),('f',8),('g',9)]",
                    result = show $ mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('e',7),('d',6),('f',8),('g',9)],
                    expected = "[('a',1),('b',2),('c',3),('d',4),('e',5),('f',8),('g',9)]"
                },
                Test {
                    name = "Merge two dictionaries, solve conflicts with a function - keep greater",
                    function = "mergeDictionaryWith max [('a',1),('b',2),('c',3),('d',13),('e',12)] [('d',14),('e',11),('f',8),('g',9)]",
                    result = show $ mergeDictionaryWith max [('a',1),('b',2),('c',3),('d',13),('e',12)] [('d',14),('e',11),('f',8),('g',9)],
                    expected = "[('a',1),('b',2),('c',3),('d',14),('e',12),('f',8),('g',9)]"
                },
                Test {
                    name = "Merge two dictionaries, solve conflicts with a function - keep smaller",
                    function = "mergeDictionaryWith min [('a',1),('b',2),('c',3),('e',13),('d',12)] [('d',14),('e',11),('f',8),('g',9)]",
                    result = show $ mergeDictionaryWith min [('a',1),('b',2),('c',3),('e',13),('d',12)] [('d',14),('e',11),('f',8),('g',9)],
                    expected = "[('a',1),('b',2),('c',3),('e',11),('d',12),('f',8),('g',9)]"
                }
            ]
        },
        Problem {
            title = "Being asked by a kind traveler",
            description = "There is always time to chat with an NPC just to get another worthless side quest on your map.",
            score = 0,
            tests = [
                Test {
                    name = "Hey you! Where are you going?",
                    function = "Your answer",
                    result = yourAnswer,
                    expected = yourAnswer
                }
            ]
        }
    ]
