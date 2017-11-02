module LeviCivita where


-- the totally antisymmetric symbol $epsilon_ijk$, labeled by [a, b, c]
leviCivita' :: (Num k, Eq a) => [a] -> k -> a -> a -> a -> k
leviCivita' [a, b, c] v x y z | [x, y, z] == [a, b, c] =  v
                              | [z, x, y] == [a, b, c] =  v
                              | [y, z, x] == [a, b, c] =  v
                              | [x, z, y] == [a, b, c] = -v
                              | [y, x, z] == [a, b, c] = -v
                              | [z, y, x] == [a, b, c] = -v
                              | otherwise = 0
leviCivita' _ _ _ _ _ = 0

leviCivita :: (Num k) => Int -> Int -> Int -> k
leviCivita = leviCivita' [1, 2, 3] 1
