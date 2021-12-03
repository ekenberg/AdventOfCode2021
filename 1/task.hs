

main2 :: IO ()
main2 = do
  input <- readFile "input.txt"
  let depths = map (read :: String -> Int) . words $ input
  print $ countIncreases $ sum3s depths

sum3s :: [Int] -> [Int]
sum3s (x:y:z:xs) = (x + y + z) : (sum3s (y:z:xs))
sum3s _ =   []

--------------------------------------------------------------------

countIncreases :: [Int] -> Int
countIncreases xs = length . filter id $zipWith (<) xs (tail xs)

--------------------------------------------------------------------

main1 :: IO ()
main1 = do
  input <- readFile "input.txt"
  let depths = map (read :: String -> Int) $ words input
  print $ countIncreases depths
