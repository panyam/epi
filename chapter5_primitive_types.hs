
import Data.Bits
import Data.Char
import qualified Data.Set as Set  

decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

clear_lowest_set_bit :: Int -> Int
clear_lowest_set_bit x = x .&. (x - 1)

lowest_set_bit :: Int -> Int
lowest_set_bit x = x .&. complement (x - 1)

lowest_set_bit_index :: Int -> Int
lowest_set_bit_index x = floor (logBase (fromIntegral 2) (fromIntegral (lowest_set_bit x)))

set_bit_positions :: Int -> [Int]
set_bit_positions 0 = []
set_bit_positions x = (lowest_set_bit_index x) : (set_bit_positions (clear_lowest_set_bit x))

num_set_bits_simple :: Int -> Int
num_set_bits_simple 0 = 0
num_set_bits_simple x 
  | x .&. 1 == 1 = 1 + num_set_bits_simple (x `shiftR` 1)
  | otherwise = num_set_bits_simple (x `shiftR` 1)

num_set_bits 0 = 0
num_set_bits x = 1 + num_set_bits (clear_lowest_set_bit x)

-- EPI 5.1
parity x = 1 .&. num_set_bits x

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
    str_from_int x = [alphabet !! i | i <- set_bit_positions x]

-- EPI 5.6

indexOf :: Eq a => a -> [a] -> Int
indexOf item xs
    | length firstMatched == 0 = -1
    | otherwise = fst (head firstMatched)
    where
        matchesItem x = snd x /= item
        firstMatched = dropWhile matchesItem 
                        (zip [0 .. (length xs) - 1] xs)

base10digits = "0123456789"
base16digits = "0123456789abcdef"
digitIndices digits = [ indexOf (chr i) digits | i <- [0 .. 255]]

intToString :: Int -> Int -> [Char]
intToString base x 
            | x < 0 = "-" ++ intToString base (-x)
            | x < base = [intToDigit x]
            | otherwise = (intToString base (div x base)) ++ [intToDigit (mod x base)]


stringToInt :: Int -> [Char] -> Int
stringToInt base (x:xs)
            | x == '-' = - (stringIntHelper base xs)
            | otherwise = stringIntHelper base (x:xs)
            where stringIntHelper base (x:xs)
                    | dx >= base = error ("Invalid digit: " ++ [x])
                    | length xs == 0 = digitToInt x
                    | otherwise = (dx * (floor (fromIntegral base ** fromIntegral digitsLeft))) + (stringIntHelper base xs)
                    where 
                        digitsLeft = (length xs)
                        dx = digitToInt x

stringIntHelper2 base xs = 
            sum [ digit_to_value digit position |
                (position, digit) <- (zip dig_positions xs)]
    where 
        dig_positions = [(length xs) - 1, (length xs) - 2 .. 0]
        digit_to_value digit position = (digitToInt digit) * 
                            floor (base ** fromIntegral position)

-- EPI 5.7

base1tobase2 b1 s1 b2 = intToString b2 (stringToInt b1 s1)

-- EPI 5.8

spreadsheet_column_encoding xs = 
        sum [ digit_to_value digit pos | (pos, digit) <- dig_positions xs]
    where
        reverse_indexes xs = [(length xs) - 1, (length xs) - 2 .. 0]
        dig_positions xs = zip  (reverse_indexes xs) xs
        symbols = ['A' .. 'Z']
        ourIndexes = digitIndices symbols
        digit_to_value digit pos = (1 + (ourIndexes !! (ord digit))) * 
                                    floor (26 ** fromIntegral pos)

-- EPI 5.9

elias_encode_int :: Int -> [Char]
elias_encode_int x = (take (len - 1) (repeat '0')) ++ xbase2
    where 
        xbase2  = intToString 2 x
        len     = (length xbase2)

elias_decode_str :: Int -> [Char] -> Int
elias_decode_str size xs = stringToInt 2 (take size xs)

elias_encode :: [Int] -> [Char]
elias_encode xs = concat (map elias_encode_int xs)

elias_decode_helper :: Int -> [Char] -> [Int]
elias_decode_helper  nzeroes [] = []
elias_decode_helper  nzeroes (f:fs)
        | f == '0' = elias_decode_helper  (1 + nzeroes) fs
        | otherwise = (elias_decode_str (1 + nzeroes) (f:fs)) : (elias_decode_helper  0 (drop nzeroes fs))

elias_decode = elias_decode_helper 0

-- EPI 5.10 - GCD of two numbers
greatest_common_divisor x 0 = x
greatest_common_divisor 0 y = y
greatest_common_divisor x y
    | x_is_even && y_is_even = 2 * greatest_common_divisor (div x 2) (div y 2)
    | x_is_odd && y_is_even = greatest_common_divisor x (div y 2)
    | x_is_even && y_is_odd = greatest_common_divisor (div x 2) y
    | x < y = greatest_common_divisor x (y - x)
    | x > y = greatest_common_divisor (x - y) x
    | otherwise = x
    where
        x_is_even = (mod x 2) == 0
        y_is_even = (mod y 2) == 0
        x_is_odd = not x_is_even
        y_is_odd = not y_is_even

