import Data.Char
import Test.QuickCheck

----------- Question 1 -----------

max1 :: Int -> Int -> Int -> Int
max1 x y z | (x >= y && x >= z) = x
           | (y >= x && y >= z) = y
           | otherwise = z

max2 :: Int -> Int -> Int -> Int
max2 x y z = maxrec [x,y,z]

maxrec :: [Int] -> Int
maxrec [x] = x
maxrec (x:xs) | x > maxrec xs = x
              | otherwise = maxrec xs

checkcorrectness x y z = max1 x y z == max2 x y z

-- *Main> quickCheck checkcorrectness
-- +++ OK, passed 100 tests.

----------- Question 2 -----------

--base: £0.002/cm^2, topping: £0.60 each, selling price: total*1.6
luigi :: Float -> Float -> Float
luigi n m =
      let radius = m/2
      in twoDP(((n*0.6) + (0.002 * pi * radius * radius)) * 1.6)

twoDP :: Float -> Float
twoDP x = fromIntegral(floor(x * 100))/ 100

-- Bambini: luigi(6)(15) = £6.32
-- Famiglia: luigi(2)(32) = £4.49
-- Pizza Famiglia is cheaper

----------- Question 3 -----------

counta :: [Char] -> Int
counta xs = length[x | x <- xs, isDigit(x)] 

countb :: [Char] -> ([Char] -> Int) -> Int
countb xs f = f(xs)

countc :: [Char] -> Int
countc [] = 0
countc (x:xs) | isDigit(x) = 1 + countc(xs)
              | otherwise = countc(xs)

----------- Question 5 -----------

won :: [Int] -> Bool
won (xs) | sum(xs) == 0 = True
         | otherwise = False

validMove :: [Int] -> Int -> Int -> Bool
validMove(ps)(p)(c) | length ps < p || p < 1 = False
                    | c < 1 = False
                    | ps!!(p - 1) >= c = True
                    | otherwise = False  

takeAway :: [Int] -> Int -> Int -> [Int]
takeAway ps p c = take (p-1) (ps) ++ [(ps!!(p-1)) - c] ++ drop (p) (ps)

getMove1 :: [Int] -> IO(Int,Int)
getMove1(x:xs) = do putStrLn "Enter a pile number"
                    p <- getLine
                    putStrLn "Enter the number of coins you wish to take"
                    c <- getLine
                    return (read p, read c)

getMove2 :: [Int] -> IO(Int,Int)
getMove2(x:xs) = do (p,c) <- getMove1(x:xs)
                    if validMove(x:xs) p c then return (p,c)
                    else do putStrLn ""
                            putStrLn "INVALID MOVE"
                            putStrLn "" 
                            getMove2(x:xs)

displayGame :: [Int] -> IO()
displayGame ps | ps /= [] = displayGame' ps 1
               | otherwise = putStrLn ""

displayGame' :: [Int] -> Int -> IO()
displayGame' (p:ps) x = do putStr (show x)
                           putStr ": "
                           putStrLn(replicate p '*')
                           if ps /= []
                           then displayGame' (ps) (x+1)
                           else putStr ""

nim :: IO()
nim = do putStrLn""
         putStrLn "Enter the name of Player 1"
         player1 <- getLine
         putStrLn "Enter the name of Player 2"
         player2 <- getLine
         putStrLn "Enter a pile configuration (e.g [5,4,3,6])"
         piles <- getLine
         putStrLn ""
         nimaux(player1, player2)(read piles)
 

nimaux :: (String, String) -> [Int] -> IO()
nimaux (p1,p2) ps = do displayGame(ps)
                       putStrLn""
                       putStrLn("Player to move: " ++ p1)
                       (p,c) <- getMove2(ps)
                       let newps = takeAway(ps)(p)(c)
                       putStrLn""
                       if won(newps) then putStrLn(p1 ++ " wins!")
                       else do displayGame(newps)
                               putStrLn ""
                               putStrLn("Player to move: " ++ p2)
                               (p,c) <- getMove2(newps)
                               let newps2 = takeAway(newps)(p)(c)
                               putStrLn""
                               if won(newps2) then putStrLn(p2 ++ " wins!")
                               else nimaux(p1,p2)(newps2)

-- *Main> nim

-- Enter the name of Player 1
-- Bob
-- Enter the name of Player 2
-- Sally
-- Enter a pile configuration (e.g [5,4,3,6])
-- [5,4,3,6]

-- 1: *****
-- 2: ****
-- 3: ***
-- 4: ******

-- Player to move: Bob
-- Enter a pile number
-- 2
-- Enter the number of coins you wish to take
-- 1

-- 1: *****
-- 2: ***
-- 3: ***
-- 4: ******

-- Player to move: Sally
-- Enter a pile number
-- 4
-- Enter the number of coins you wish to take
-- 6

-- 1: *****
-- 2: ***
-- 3: ***
-- 4:

-- Player to move: Bob
-- Enter a pile number
-- 1
-- Enter the number of coins you wish to take
-- 99

-- INVALID MOVE

-- Enter a pile number

----------- Question 6 -----------

stratB :: [Int] -> (Int, Int)
stratB(xs) =  stratBaux(xs)(1)

stratBaux :: [Int] -> Int -> (Int, Int)
stratBaux(x:xs)(index) | x == 0 = stratBaux(xs)(index + 1)
                       | otherwise = (index, x)

nimAI :: ([Int] -> (Int, Int)) -> IO()
nimAI f = do putStrLn ""
             putStrLn "Enter a pile configuration (e.g [5,4,3,6])"
             piles <- getLine
             putStrLn ""
             nimAIaux f (read piles)

nimAIaux :: ([Int] -> (Int, Int)) -> [Int] -> IO()
nimAIaux f (ps) = do displayGame(ps)
                     putStrLn ""
                     (p,c) <- getMove2(ps)
                     let newps = takeAway(ps)(p)(c)
                     putStrLn""
                     if won(newps) then putStrLn("You win!")
                     else do displayGame(newps)
                             putStrLn ""
                             let (p,c) = f(newps)
                             putStrLn("The AI takes " ++ show(c) ++ " coin(s) from pile " ++ show(p))
                             let newps2 = takeAway(newps)(p)(c)
                             putStrLn""
                             if won(newps2) then putStrLn("The AI wins!")
                             else nimAIaux f (newps2)

-- stratI calculates the 'nim sum' of the current pile configuration
-- and tries to find a pile with that exact value to remove from the game.
-- If this value is removed the player is left in an unbalanced (losing) position.
-- If this value can't be found the basic strategy is used instead.

stratI :: [Int] -> (Int, Int)
stratI(x:xs) = do if sum(nimSum(x:xs)) == 0 then stratB(x:xs)
                  else do let n = binToInt(nimSum(x:xs))
                          let (p,c) = stratIaux(x:xs)(n)(1)
                          if validMove(x:xs)(p)(c) then (p,c)
                          else stratB(x:xs)

stratIaux :: [Int] -> Int -> Int ->(Int,Int)
stratIaux [] _ _ = (-1,-1)
stratIaux(x:xs)(n)(count) | x == n = (count, x)
                          | otherwise = stratIaux(xs)(n)(count + 1)


-- *Main> nimAI stratI

-- Enter a pile configuration (e.g [5,4,3,6])
-- [5,4,3,6]

-- 1: *****
-- 2: ****
-- 3: ***
-- 4: ******

-- Enter a pile number
-- 2
-- Enter the number of coins you wish to take
-- 1

-- 1: *****
-- 2: ***
-- 3: ***
-- 4: ******

-- The AI takes 3 coin(s) from pile 2

-- 1: *****
-- 2:
-- 3: ***
-- 4: ******

-- Enter a pile number
-- 1
-- Enter the number of coins you wish to take
-- 1

-- 1: ****
-- 2:
-- 3: ***
-- 4: ******

-- The AI takes 4 coin(s) from pile 1

-- 1:
-- 2:
-- 3: ***
-- 4: ******

-- Enter a pile number

binToInt :: [Int] -> Int
binToInt(x:xs) = binToIntAux(reverse(x:xs))(0)

binToIntAux :: [Int] -> Int -> Int
binToIntAux [] _ = 0
binToIntAux(x:xs)(n) | x == 1 && n == 0 = 1 + binToIntAux(xs)(2)
                     | x == 0 && n == 0 = 0 + binToIntAux(xs)(2)
                     | x == 0 = 0 + binToIntAux(xs)(n*2)
                     | otherwise = n + binToIntAux(xs)(n*2)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n | n `mod` 2 == 0 = toBinary(n `div` 2) ++ [0]
           | otherwise = toBinary(n `div` 2) ++ [1]


nimSum :: [Int] -> [Int]
nimSum(x:xs) = reverse(nimSumaux(map reverse (map toBinary(x:xs))) 0)

nimSumaux :: [[Int]] -> Int -> [Int]
nimSumaux(x:xs)(index) | index < maxLengthList(x:xs) - 1 = nimSumAt(x:xs)(index) : nimSumaux(x:xs)(index + 1)
                       | otherwise = [nimSumAt(x:xs)(index)]


maxLengthList :: [[Int]] -> Int
maxLengthList [] = 0
maxLengthList(x:xs) = max(length(x))(maxLengthList(xs))

nimSumAt :: [[Int]] -> Int -> Int
nimSumAt(x:xs) index = nimBalance(nimgetElem(x:xs)(index))

nimBalance :: [Int] -> Int
nimBalance [] = 0
nimBalance(x:xs) | even(x + nimBalance(xs)) = 0
                 | otherwise = 1

nimgetElem :: [[Int]] -> Int -> [Int]
nimgetElem [] _ = [0]
nimgetElem(x:xs)(index) | index < 0 = 0 : nimgetElem(xs)(index)
                        | index > length(x) - 1 = 0 : nimgetElem(xs)(index)
                        | otherwise = x!!index : nimgetElem(xs)(index)
                  