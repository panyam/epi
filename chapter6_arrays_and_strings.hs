
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

max_increase_from_index :: [Int] -> [(Int,Int)]
max_increase_from_index [] = []
max_increase_from_index (x:[]) = [(0,x)]
max_increase_from_index (x:xs)
    | x <= aj = (j,aj):max_inc_from_next
    | otherwise = (length max_inc_from_next, x) : max_inc_from_next
    where 
        max_inc_from_next = max_increase_from_index xs
        (j, aj) = head max_inc_from_next


minimum_robot_battery :: [(Int,Int,Int)] -> (Int,Int,Int)
minimum_robot_battery xs = maximumBy (comparing third) [(i,lxs - j,aj-ai) | 
                            (i,ai,(j,aj)) <- zip3 [0..((length ms) - 1)] ms ms2]
    where 
        ms = map third xs
        ms2 = max_increase_from_index ms
        lxs = length xs - 1

