
import Data.Bits
import Data.List
import Data.Ord
import Data.Char
import qualified Data.Set as Set  
import Data.Array

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

-- EPI 6.6 - Longest contiguious increasing sub array
longest_contig_inc_subarray :: (Ord a) => [a] -> (Int, Int)
longest_contig_inc_subarray [] = (-1, -1)
longest_contig_inc_subarray (x:xs) = longest_contig_inc_subarray' (0, x, 0, x) xs
    where
    longest_contig_inc_subarray' (i,ai,j,aj) [] = (i,j)
    longest_contig_inc_subarray' (i,ai,j,aj) (x:xs) 
            | x >= aj = longest_contig_inc_subarray' (i,ai,j + 1,x) xs
            | otherwise = longest_contig_inc_subarray' (j + 1,x,j + 1,x) xs

lcisa :: (Ord a) => [a] -> (Int, Int)
lcisa [] = (-1,-1)
lcisa xs = lcisa' (0,1) 0 xs
    where 
        lcisa' (start,maxlen) i [] = (start,maxlen)
        lcisa' (start,maxlen) i xs
            | nextlen > maxlen = lcisa' nextbest
                                    (i + maxlen + inc_prefix)
                                    (drop inc_prefix rest)
            | otherwise = lcisa' (start,maxlen) (i + maxlen) rest
            where
                first_l = take maxlen xs
                rest = drop maxlen xs
                dec_prefix = largest_dec_prefix (reverse first_l)
                inc_prefix = largest_inc_prefix rest
                nextlen = inc_prefix + dec_prefix
                nextbest = (i + maxlen - dec_prefix, nextlen) 

largest_inc_prefix [] = 0
largest_inc_prefix (x:[]) = 1
largest_inc_prefix (x:y:xs)
        | x <= y = 1 + largest_inc_prefix (y:xs)
        | otherwise = 1

largest_dec_prefix [] = 0
largest_dec_prefix (x:[]) = 1
largest_dec_prefix (x:y:xs)
        | x >= y = 1 + largest_dec_prefix (y:xs)
        | otherwise = 1


-- EPI 6.9 - Big Integer multiplication
bigint_multiply :: [Char] -> [Char] -> [Char]
bigint_multiply ('-':xs) ('-':ys) = bigint_multiply xs ys
bigint_multiply ('-':xs) (y:ys) = '-' : (bigint_multiply xs (y:ys))
bigint_multiply (x:xs) ('-':ys) = '-' : (bigint_multiply (x:xs) ys)
bigint_multiply xs ys = ""


-- EPI 6.13 - Rotate an array
rotate_array xs j = rotate_array' xs 0
    where 
        lxs = length xs
        j' = mod j lxs
        gcd_lxs_j = greatest_common_divisor lxs j'
        numtimes = div lxs gcd_lxs_j
        rotate_array' xs start_index
            | start_index >= gcd_lxs_j = xs
            | otherwise = m_rotations ys (j' - (start_index + 1)) j' numtimes
            where
                ys = rotate_array' xs (start_index + 1)

-- Performs "n" jumps of "size" each starting from a given index, wrapping around if necessary.
-- For example if A = [1,2,3,4,5,6,7,8,9,10], 
--    m_rotations A 0 3 3 would cause the following jumps:
--          1 -> 4 -> 7, with A being:
--    [1, 2, 3, 1, 5, 6, 4, 8, 9, 10]
m_rotations xs index size m = elems (m_rotations' (arrayFromList xs 0) index (xs!!index) size m)
    where
        len = length xs
        m_rotations' arr curr_index curr_value size numleft 
            | curr_index < 0 || size <= 0 || numleft <= 0 = arr
            | otherwise = m_rotations' (arr // [(next_index, curr_value)]) next_index next_value size (numleft - 1)
            where
                next_index = mod (curr_index + size) len
                next_value = arr!next_index

-- EPI 6.13 - A simpler algorithm that is similar to the reversal of words in a sentence
rotate_array_simple xs j = reverse (take j rxs) ++ reverse (drop j rxs) 
        where rxs = reverse xs

-- EPI 6.14 - Sudoku Checker
zero_matrix cols rows = array ((0,0), (cols - 1,rows - 1)) [((i,j), 0) | i <- [0..cols - 1], j <- [0..rows - 1]]

-- EPI 6.15 - Print an array spirally
data Direction = North | East | South | West
clockwise_of North = East
clockwise_of East = South
clockwise_of South = West
clockwise_of West = North
vector_of North = (0, -1)
vector_of South = (0, 1)
vector_of East = (1, 0)
vector_of West = (-1, 0)
primary_coord North = 1
primary_coord South = 1
primary_coord East = 0
primary_coord West = 0

next_pt dir (x,y) = ((x + fst (vector_of dir)),(y + snd (vector_of dir)))
prev_pt dir (x,y) = ((x - fst (vector_of dir)),(y - snd (vector_of dir)))
print_spirally width height = print_spirally' East (0,0) 0 [width, height]
    where
        print_spirally' dir (x,y) t [w,h]
            | w == 0 || h == 0 = []
            | t < [w,h] !! curr_coord = (y * width + x + 1) : print_spirally' dir (next_pt dir (x,y)) (t+1) [w,h]
            | otherwise = print_spirally' next_dir (next_pt next_dir (prev_pt dir (x,y))) 0 [next_w,next_h]
            where 
                curr_coord = primary_coord dir
                dir_vector = vector_of dir
                next_dir = clockwise_of dir
                next_dir_vector = vector_of next_dir
                next_w = w - (abs (fst next_dir_vector))
                next_h = h - (abs (snd next_dir_vector))

-- row_in_matrix :: Array Int Int -> [Int]
-- row_in_matrix matrix row = []

-- EPI 6.18 - Runlength encoding
run_length_encode :: [Char] -> [Char]
run_length_encode xs = run_length_encode' 0 '\0' xs
    where
        run_length_encode' 0 _ [] = []
        run_length_encode' 0 _ (x:xs) = run_length_encode' 1 x xs
        run_length_encode' count curr_ch [] = (show count) ++ [curr_ch]
        run_length_encode' count curr_ch (x:xs)
            | curr_ch == x = run_length_encode' (count + 1) curr_ch xs
            | otherwise = (show count) ++ [curr_ch] ++ run_length_encode' 1 x xs

run_length_decode :: [Char] -> [Char]
run_length_decode xs = run_length_decode' 0 xs
    where
        run_length_decode' _ [] = []
        run_length_decode' count (x:xs)
            | isDigit x = run_length_decode' ((count * 10) + (digitToInt x)) xs
            | otherwise = (replicate count x) ++ run_length_decode' 0 xs
            

-- EPI 6.19 - Reverse words in a string
reverse_words :: [Char] -> [Char]
reverse_words [] = []
reverse_words xs = initial_spaces ++ (reverse after_spaces) ++ reverse_words remaining
    where
        initial_spaces = takeWhile isSpace xs
        spaces_dropped = dropWhile isSpace xs
        after_spaces = takeWhile (\x -> not (isSpace x)) spaces_dropped
        remaining = drop (length after_spaces) spaces_dropped

