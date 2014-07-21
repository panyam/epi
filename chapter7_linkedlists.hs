
-- EPI 7.1 - Merge two sorted lists

data List a = Nil | Cons a (List a)
    deriving (Show)

ll_tail Nil = Nil
ll_tail (Cons x Nil) = Cons x Nil
ll_tail (Cons x xs) = ll_tail xs

ll_taken 0 _ = Nil
ll_taken _ Nil = Nil
ll_taken num (Cons x xs) = Cons x (ll_taken (num - 1) xs)

ll_dropn 0 xs = xs
ll_dropn _ Nil = Nil
ll_dropn num (Cons x xs) = ll_dropn (num - 1) xs

ll_append (Cons x Nil) ys = Cons x ys
ll_append (Cons x xs) ys = Cons x (ll_append xs ys)

ll_from_list [] = Nil
ll_from_list (x:xs) = Cons x (ll_from_list xs)

ll_to_list Nil = []
ll_to_list (Cons x xs) = x : (ll_to_list xs)

ll_merge_two_sorted xs Nil = xs
ll_merge_two_sorted Nil ys = ys
ll_merge_two_sorted (Cons x xs) (Cons y ys)
        | x < y = Cons x (ll_merge_two_sorted xs (Cons y ys))
        | otherwise = Cons y (ll_merge_two_sorted (Cons x xs) ys)


-- EPI 7.2 - 

