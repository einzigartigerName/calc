module Cursor
    ( Cursor (..)
    , empty
    , fromList
    , fromListSelect
    , toList
    , selectFirst
    , selectLast
    , selectPrev
    , selectPrevN
    , selectNext
    , selectNextN
    , insertAbove
    , insertTop
    , insertBelow
    , insertBottom
    , insertListAbove
    , insertListTop
    , insertListBelow
    , insertListBottom
    , isEmpty
    , moveSelectionUp
    , moveSelectionDown
    , removeSelected
    , removeSelectedFocusNext
    , removeSelectedFocusPrev
    )
    where


{-------------------------------------------------------------------------------------------------
                                            Data
-------------------------------------------------------------------------------------------------}
-- | Cursor
data Cursor a = Cursor
    { previous :: [a] -- reverse order
    , selected :: Maybe a
    , next :: [a]
    } deriving (Show, Eq)


{-------------------------------------------------------------------------------------------------
                                            Creation
-------------------------------------------------------------------------------------------------}
-- | create Cursor from List; focus last item
fromList :: [a] -> Cursor a
fromList [] = empty
fromList (x : xs) = Cursor {previous = xs, selected = Just x, next = []}

-- | create Cursor form List; try focusing nth item
fromListSelect :: Int -> [a] -> Maybe (Cursor a)
fromListSelect index li = if length li < index + 1 || index < 0
    then Nothing
    else let (n, p) = splitAt index li
        in Just $ Cursor {previous = reverse n, selected = Just (head p), next = tail p}

-- | create List from Cursor
toList :: Cursor a -> [a]
toList c = let  n = next c
                s = selected c
                p = reverse $ previous c
    in case s of
        Just sel -> reverse $  p ++ [sel] ++ n
        Nothing -> []


{-------------------------------------------------------------------------------------------------
                                            Selection
-------------------------------------------------------------------------------------------------}
-- | try selecting first item in cursor
selectFirst :: Cursor a -> Maybe (Cursor a)
selectFirst c = case toList c of
    [] -> Nothing
    li -> fromListSelect 0 li

-- | try selecting last item in cursor
selectLast :: Cursor a -> Maybe (Cursor a)
selectLast c = case toList c of
    [] -> Nothing
    li -> fromListSelect (length li - 1) li

-- | try selecting next item in cursor
selectNext :: Ord a => Cursor a -> Maybe (Cursor a)
selectNext c = if next c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = tail cNext, selected = Just $ head cNext, previous = cSel : cPrev}

-- | try selecting nth next item in cursor
selectNextN :: Ord a => Int -> Cursor a -> Maybe (Cursor a)
selectNextN n c
    | n > 0 = selectNext c >>= (\nc -> selectNextN (n - 1) nc)
    | otherwise = Just c

-- | try selecting previous item in cursor
selectPrev :: Ord a => Cursor a -> Maybe (Cursor a)
selectPrev c = if previous c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = cSel : cNext, selected = Just $ head cPrev, previous = tail cPrev} 

-- | try selecting nth previous item in cursor
selectPrevN :: Ord a => Int -> Cursor a -> Maybe (Cursor a)
selectPrevN n c
    | n > 0 = selectPrev c >>= (\nc -> selectPrevN (n - 1) nc)
    | otherwise = Just c


{-------------------------------------------------------------------------------------------------
                                            Single Insertion
-------------------------------------------------------------------------------------------------}
-- | insert item at top
insertTop :: Cursor a -> a -> Cursor a
insertTop c a = case selected c of
    Just _  -> c {previous = (previous c) ++ [a]}
    Nothing -> empty {selected = Just a}

-- | insert item at bottom
insertBottom :: Cursor a -> a -> Cursor a
insertBottom c a = case selected c of
    Just _  -> c {next = (next c) ++ [a]}
    Nothing -> empty {selected = Just a}

-- | insert item above selection
insertAbove :: Cursor a -> a -> Cursor a
insertAbove c a = let p = previous c
    in case selected c of
        Nothing -> c {selected = Just a}
        Just _ -> c {previous = a : p}

-- | insert item below selection
insertBelow :: Cursor a -> a -> Cursor a
insertBelow c a = let n = next c
    in case selected c of
        Nothing -> c {selected = Just a}
        Just _ -> c {next = a : n}


{-------------------------------------------------------------------------------------------------
                                            List Insertion
-------------------------------------------------------------------------------------------------}
-- | insert List at top
insertListTop :: Cursor a -> [a] -> Cursor a
insertListTop c (x:xs) = insertListTop (insertTop c x) xs
insertListTop c [] = c

-- | insert List at bottom
insertListBottom :: Cursor a -> [a] -> Cursor a
insertListBottom c (x:xs) = insertListBottom (insertBelow c x) xs
insertListBottom c [] = c

-- | insert List above selection
insertListAbove :: Cursor a -> [a] -> Cursor a
insertListAbove c (x:xs) = insertListAbove (insertAbove c x) xs
insertListAbove c [] = c

-- | insert List above selection
insertListBelow :: Cursor a -> [a] -> Cursor a
insertListBelow c (x:xs) = insertListBelow (insertBelow c x) xs
insertListBelow c [] = c


{-------------------------------------------------------------------------------------------------
                                            Move Items
-------------------------------------------------------------------------------------------------}
-- | move selected Item down
moveSelectionDown :: Cursor a -> Maybe (Cursor a)
moveSelectionDown c =
    let p = previous c
        n = next c 
    in case selected c of
        Just _ -> if length p > 0
            then Just $ c {previous = tail p, next = head p : n}
            else Nothing
        Nothing -> Nothing

-- | move selected Item up
moveSelectionUp :: Cursor a -> Maybe (Cursor a)
moveSelectionUp c =
    let p = previous c
        n = next c 
    in case selected c of
        Just _ -> if length n > 0
            then Just $ c {previous = head n : p, next = tail n}
            else Nothing
        Nothing -> Nothing


{-------------------------------------------------------------------------------------------------
                                            Removing
-------------------------------------------------------------------------------------------------}
-- | remove selected item; first Item in cursor will be selected
removeSelected :: Cursor a -> Cursor a
removeSelected c = case selected c of
    Nothing -> c
    Just _ -> let   n = next c
                    p = previous c
        in fromList $ (reverse p) ++ n

-- | remove selected item and focus next in cursor
removeSelectedFocusNext :: Ord a => Cursor a -> Cursor a
removeSelectedFocusNext c = case selectNext c of
    Just n -> let prev = previous n in n {previous = tail prev}
    Nothing -> if previous c == []
        then empty
        else let prev = previous c in Cursor {next = [], selected = Just $ head prev, previous = tail prev}

-- | remove selected item and select previous in cursor
removeSelectedFocusPrev :: Ord a => Cursor a -> Cursor a
removeSelectedFocusPrev c = case selectPrev c of
    Just prev -> let n = next prev in prev {next = tail n}
    Nothing -> if next c == []
        then empty
        else let n = next c in Cursor {next = tail n, selected = Just $ head n, previous = []}


{-------------------------------------------------------------------------------------------------
                                            Util
-------------------------------------------------------------------------------------------------}
-- | check cursor empty
isEmpty :: Cursor a -> Bool
isEmpty c = case selected c of
    Just _ -> False
    Nothing -> True

-- | default empty cursor
empty :: Cursor a
empty = Cursor {next = [], selected = Nothing, previous = []}