# haskell-autograder
An automatic grader for the Haskell lab test. Beyond the fact that it freed me of the burden of manually correcting code and potentially making mistakes, it prooved itself useful by interactively leading the students to correct solutions.

The test, including the grader framework, is defined in the `Test.hs` file, and it can be executed with the `runhaskell Test.hs` command. The students can write their solutions in the `Impl.hs` file.

The program runs entirely in the command line. In Linux terminals it has colored output, if someone wants to use it on Windows the coloring of the output can be turned off by setting a flag defined in the `Impl.hs` file.
![Screenshot](https://user-images.githubusercontent.com/20115656/51351735-660bde00-1ab4-11e9-8f4b-ba17182ec57c.png)

The exercises provided in the test are not hard-coded in the grader framework, all the data regarding the problems and the solutions can be found under the comment `-- define the problems and the soultions` in `Test.hs` file. One can change the exercises to suit their or the students' needs without understanding the grader framework itself, by only understanding the data structure used to define the test. The aforementioned structure won't be discussed, because it is intuitive and can be understood with a bit of tinkering.
