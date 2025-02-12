# An Introduction to Debugging in Haskell

## Introduction

There is a general purpose debugging guide available [here](https://wiki.haskell.org/Debugging), but it is outdated and lacks information on how to navigate a modern debugger like gdb.
This guide will cover two main techniques for debugging a program in Haskell.
The first method involves inserting print statements using [Debug.Trace](https://hackage.haskell.org/package/base-4.21.0.0/docs/Debug-Trace.html), a built-in package that is great for debugging smaller problems.
The second will focus on tracing through the code using the [GHCi debugger](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger), which is very closely related in function to gdb, the debugger used in C in courses like CSC209 and CSC369.
**Note:** If any issues arise or new commands, tools, or features become useful, please update this guide accordingly!

## Debug.Trace

This library is a quick and effective way to print values of objects in your Haskell Program.
We will look at `trace` and `traceM`.
If you want to explore more with this library, please refer to the [documentation](https://hackage.haskell.org/package/base-4.21.0.0/docs/Debug-Trace.html).

`trace :: String -> a -> a`, takes in two inputs: the message and an expression.
An effective way to utilize `trace` is with the `let` keyword.
For example, in `Svg.Parser.dotProduct`:

```haskell
-- | Computes the dot product of two vectors.
dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 =
    let result = zipWith (*) v1 v2
    in trace ("Intermediate Products: " ++ show result) (sum result)

ghci> dotProduct [1, 2, 3] [3, 6, 7]
Intermediate Products: [3.0, 12.0, 21.0]
36.0
```

`traceM :: Applicative f => String -> f ()` is similar to `trace` except the only input is the message, and it returns within any [Applicative context](https://learnyouahaskell.github.io/functors-applicative-functors-and-monoids.html#applicative-functors).
This is quite handy to use in do-notation.
For example, in `Database.CourseQueries.queryCourse`:

```haskell
-- | Queries the database for all information about @course@, and returns a JSON
-- object representing the course.
queryCourse :: T.Text -> IO Value
queryCourse str = do
    courseJSON <- returnCourse str
    traceM $ "The course: " ++ show courseJSON
    return $ toJSON courseJSON

ghci> queryCourse "CSC324"
The course: Just (Course {breadth = Just "The Physical and Mathematical "...)
Object (fromList [("allMeetingTimes",Array []),("breadth",String "The " ...)
```

**Warning:** One important consideration is how trace is used.
Haskell has lazy evaluation, so if Debug.Trace's functions are used in a place that does not need to be executed, the output will not be visible.
Here is an example in `Svg.Parser.dotProduct` that illustrates such a case:

```haskell
-- | Computes the dot product of two vectors.
dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 =
    let result = zipWith (*) v1 v2
        _ = trace ("Intermediate Products: " ++ show result) --| This string will not be printed
    in (sum result)
```

## GHCi Debugger

### Setup

Run `stack ghci` to start GHCi.
This is the [Haskell tool stack](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#) and there are more features than just this debugger.
Start by loading the relevant modules using `:l Module`.

### Useful Commands

| **Category**            | **Command**                  | **Description**                                                         |
| ----------------------- | ---------------------------- | ----------------------------------------------------------------------- |
| **Setting Up GHCi**     | `:l Module`                  | Load `Module` into GHCi.                                                |
|                         |                              |                                                                         |
| **Breakpoints**         | `:break ModuleA.my_func`     | Set a breakpoint at function `my_func` in `ModuleA`.                    |
|                         | `:break ModuleA line_number` | Set a breakpoint at a specific line in `ModuleA`.                       |
|                         | `:show breaks`               | Display all active breakpoints.                                         |
|                         | `:enable breakpoint_number`  | Enable a specific breakpoint.                                           |
|                         | `:disable breakpoint_number` | Disable a specific breakpoint.                                          |
|                         | `:enable *`                  | Enable all breakpoints.                                                 |
|                         | `:disable *`                 | Disable all breakpoints                                                 |
|                         | `:delete breakpoint_number`  | Delete a specific breakpoint.                                           |
|                         | `:delete *`                  | Delete all breakpoints.                                                 |
|                         |                              |                                                                         |
| **Step Debugging**      | `:step`                      | Step through the program until the next breakpoint.                     |
|                         | `:steplocal`                 | Step through the program, limiting breakpoints to the current function. |
|                         | `:stepmodule`                | Step through the program, limiting breakpoints to the current module.   |
|                         | `:force expression`          | Forces the given expression to be evaluated                             |
|                         | `main`                       | Run the program until the first breakpoint is hit.                      |
|                         |                              |                                                                         |
| **Variable Inspection** | `:info var_name`             | Show the type of `var_name`.                                            |
|                         | `:print var_name`            | Display the current value of `var_name` if it has typeclass `Show`.     |

### Important Notes

- You can only run main once per session.
  If `main` is run a second time, the debugger environment will not be available.
  To restart debugging, exit GHCi and restart `stack ghci`.

- If you call a function in GHCi with breakpoints set, you won’t see actual values—only the algebraic data type placeholders (\_).
  This happens due to Haskell’s lazy evaluation.
  When execution stops at a breakpoint, the function hasn’t been evaluated yet, so arguments remain unevaluated.
  To see variable values, use `:force variable_name`.
  This forces evaluation and displays the computed result.
  You can still see the values of your variables by using `:force variables`.
  GHCi binds the current expression to `_result`, which holds the unevaluated computation.
  Using `:force _result` will fully evaluate the function but skip all remaining breakpoints.

- For additional tools please check out the [official documentation](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger).

### Example

Here we use a toy project with modules `Main` and `Functions.Extra1`:

Main.hs:

```haskell
import Functions.Extra1(add2, sumList)

main :: IO ()
main = print (add2 1 2 + sumList [3, 4, 5, 6])
```

Functions.Extra1.hs:

```haskell
module Functions.Extra1 where

add2 ::(Num a) => a -> a -> a
add2 x y = x + y

sumList :: (Num a) => [a] -> a
sumList []     = 0
sumList (x:xs) = x + sumList xs
```

Now we start off by setting up the environment:

```
CourseographyDeveloper % stack ghci
ghci> :l Main
[1 of 2] Compiling Functions.Extra1 ( Functions/Extra1.hs, interpreted )
[2 of 2] Compiling Main             ( Main.hs, interpreted )
```

Next we can set some breakpoints:

```
ghci> :break Functions.Extra1.add2
Breakpoint 0 activated at Functions/Extra1.hs:6:12-16
```

Now run through the entire program:

```
ghci> main
Stopped in Functions.Extra1.add2, Functions/Extra1.hs:6:12-16
_result :: Integer = _
x :: Integer = 1
y :: Integer = 2
[Functions/Extra1.hs:6:12-16] ghci> :step
21
```

Unfortunately, we can't trace through the entire program unless we restart the debugger:

```
ghci> main
21
```

Now we can illustrate calling `:force _result`:

```
[Functions/Extra1.hs:6:12-16] ghci> :break Functions.Extra1.sumList
Breakpoint 1 activated at Functions/Extra1.hs:9:14
Breakpoint 2 activated at Functions/Extra1.hs:10:18-31
[Functions/Extra1.hs:6:12-16] ghci> :delete 1
[Functions/Extra1.hs:6:12-16] ghci> sumList [1, 2, 3, 4]
Stopped in Functions.Extra1.sumList, Functions/Extra1.hs:10:18-31
_result :: a = _
x :: a = _
xs :: [a] = [_,_,_]
... [Functions/Extra1.hs:10:18-31] ghci> :force _result
*** Ignoring breakpoint [Functions/Extra1.hs:10:18-31]
*** Ignoring breakpoint [Functions/Extra1.hs:10:18-31]
*** Ignoring breakpoint [Functions/Extra1.hs:10:18-31]
*** Ignoring breakpoint [Functions/Extra1.hs:10:18-31]
_result = 10
```

Instead, we can look at intermediate values:

```
ghci> sumList [1, 2, 3, 4]
Stopped in Functions.Extra1.sumList, Functions/Extra1.hs:10:18-31
_result :: a = _
x :: a = _
xs :: [a] = [_,_,_]
[Functions/Extra1.hs:10:18-31] ghci> :force x
x = 1
```
