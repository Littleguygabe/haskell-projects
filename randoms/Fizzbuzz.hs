run :: Int -> [String]
run r = map transFizzBuzz [1..r]

transFizzBuzz :: Int -> String
transFizzBuzz x
    | m5 && m3 = "FizzBuzz"
    | m5 = "Buzz"
    | m3 = "Fizz"
    | otherwise = show x
    where
        m5 = mul5 x
        m3 = mul3 x

mul5 :: Int -> Bool
mul5 x = if (rem x 5) == 0 then True else False

mul3 :: Int -> Bool
mul3 x = if (rem x 3) == 0 then True else False