
import Data.Bits
import Data.List
import Data.Ord
import Data.Char
import qualified Data.Set as Set  
import Data.Array

arrayFromList input startIndex = array bounds [(i + startIndex, x) | (i,x) <- (zip indexes input)]
            where
                bounds = (startIndex, startIndex + (length input) - 1)
                indexes = [0 .. length input]

swapItems xs a b = xs // [(b, xa)] // [(a, xb)]
            where
                xa = xs!a
                xb = xs!b

-- EPI 6.1
dutch_flag_partition xs i = elems (dutch_flag_partition' arrayXS i 0 0 ((length xs) - 1))
    where
        arrayXS = arrayFromList xs 0
        pivot = arrayXS ! i
        dutch_flag_partition' xs i s e l
            | e > l = xs
            | (xs!e) < pivot = dutch_flag_partition' (swapItems xs s e) i (s + 1) (e + 1) l
            | (xs!e) == pivot = dutch_flag_partition' xs i s (e + 1) l
            | otherwise = dutch_flag_partition' (swapItems xs e l) i s e (l - 1)

-- EPI 6.3

third (a,b,c) = c

max_diff_nsquared xs = maximumBy (comparing third) 
                        [(i,j,xj - xi) | (i,xi) <- zip [0.. length xs - 1] xs, 
                                         (j,xj) <- zip [0.. length xs - 1] xs, i < j]

-- max_increase_from_index :: [Int] -> [(Int,Int)]
max_increase_from_index :: (Num a, Ord a) => [a] -> [(Integer, a)]
max_increase_from_index [] = []
max_increase_from_index (x:[]) = [(0,x)]
max_increase_from_index (x:xs)
    | x <= aj = (j,aj):max_inc_from_next
    | otherwise = (toInteger (length max_inc_from_next), x) : max_inc_from_next
    where 
        max_inc_from_next = max_increase_from_index xs
        (j, aj) = head max_inc_from_next


minimum_robot_battery :: (Num a, Ord a) => [(a,a,a)] -> (Integer, Integer, a)
minimum_robot_battery xs = maximumBy (comparing third) [(toInteger i,toInteger (lxs - fromInteger j),aj-ai) | 
                            (i,ai,(j,aj)) <- zip3 [0..((length ms) - 1)] ms ms2]
    where 
        ms = map third xs
        ms2 = max_increase_from_index ms
        lxs = length xs - 1

-- EPI 6.4 - Generalized Max Difference
-- EPI 6.4 A

max_increase_forward :: (Num a, Ord a) => [a] -> [(Integer, a, a)]
max_increase_forward [] = []
max_increase_forward (x:xs) = max_increase_forward' 0 (0,x) (x:xs)
    where
        max_increase_forward' i _ [] = []
        max_increase_forward' i (j,aj) (x:xs)
            | x >= aj = (j,aj,x - aj) : (max_increase_forward' (i + 1) (j,aj) xs)
            | otherwise = (i,x,0) : (max_increase_forward' (i + 1) (i,x) xs)

max_increase_backward :: (Num a, Ord a) => [a] -> [(Integer, a, a)]
max_increase_backward xs = max_increase_backward' 0 xs
max_increase_backward' i [] = []
max_increase_backward' i [x] = [(i,x,0)]
max_increase_backward' i (x:xs)
    | x <= aj = (j,aj,aj - x) : rest
    | otherwise = (i,x,0) : rest
    where 
        rest = max_increase_backward' (i + 1) xs
        (j,aj,diff) = head rest

max_increase_k2 :: (Num a, Ord a) => [a] -> a
max_increase_k2 xs = maximumBy compare (max_increase_k2_iter fs rs)
    where 
        fs = drop 1 (take (length xs - 1) (max_increase_forward xs))
        rs = drop 2 (max_increase_backward xs)
        max_increase_k2_iter [] [] = []
        max_increase_k2_iter ((a1,a2,a3):fs) ((b1,b2,b3):rs) = (a3 + b3) : (max_increase_k2_iter fs rs)

-- EPI 6.5 - Subset summing to 0 mod n
mod_till_k :: [Integer] -> [Integer]
mod_till_k xs = mod_till_k' 0 xs
    where
        n = length xs
        mod_till_k' :: Integer -> [Integer] -> [Integer]
        mod_till_k' _ [] = []
        mod_till_k' prev (x:xs) = curr : (mod_till_k' curr xs)
            where curr = mod (x + prev) (toInteger n)

zero_mod_n_subset :: [Integer] -> (Integer, Integer)
zero_mod_n_subset xs = zero_mod_n_subset' 0 index_table (mod_till_k xs)
    where 
        n = length xs
        index_table = array (0, toInteger (n - 1)) [(toInteger i, -1) | i <- [0..n - 1]]
        zero_mod_n_subset' i index_table (x:xs)
            | x == 0 = (0,i)
            | index_table!x /= -1 = (index_table!x,i)
            | otherwise = zero_mod_n_subset' (i + 1) (index_table // [(toInteger x,toInteger i)]) xs

-- EPI 6.19 - Reverse words in a string
reverse_words :: [Char] -> [Char]
reverse_words [] = []
reverse_words xs = initial_spaces ++ (reverse after_spaces) ++ reverse_words remaining
    where
        initial_spaces = takeWhile isSpace xs
        spaces_dropped = dropWhile isSpace xs
        after_spaces = takeWhile (\x -> not (isSpace x)) spaces_dropped
        remaining = drop (length after_spaces) spaces_dropped

