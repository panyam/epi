
import Data.Bits

decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

clear_lowest_set_bit x = x .&. (x - 1)

lowest_set_bit :: Int -> Int
lowest_set_bit x = x .&. complement (x - 1)

num_set_bits_simple 0 = 0
num_set_bits_simple x 
  | x .&. 1 == 1 = 1 + num_set_bits_simple (x `shiftR` 1)
  | otherwise = num_set_bits_simple (x `shiftR` 1)

num_set_bits 0 = 0
num_set_bits x = 1 + num_set_bits (clear_lowest_set_bit x)

-- EPI 5.4
closest_neighbour_by_weight x = closest_neighbour_by_weight_aux x 0
    where closest_neighbour_by_weight_aux x i 
            | two_digits /= 0 && two_digits /= 3 = xor x (shiftL 3 i)
            | otherwise = closest_neighbour_by_weight_aux x (i + 1)
              where two_digits = (x `shiftR` i) .&. 3

closest_neighbour_by_weight_test x = ((decToBin x), (decToBin (closest_neighbour_by_weight x)))

-- EPI 5.5
power_set :: [Char] -> [[Char]]
power_set [] = []
power_set alphabet = [str_from_int x | x <- [0 .. ((1::Int) `shiftL` numAlpha) - 1]]
  where 
    numAlpha = length alphabet
    str_from_int 0 = ""
    str_from_int x = (str_from_int (clear_lowest_set_bit x)) ++ 
                        ([alphabet !! (lowest_bit x)])
        where lowest_bit x = floor (logBase (fromIntegral 2) (fromIntegral (lowest_set_bit x)))

