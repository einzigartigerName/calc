module Cursor where

data Cursor a = Cursor
    { next :: [a] -- reverse order
    , selected :: Maybe a
    , previous :: [a]
    } deriving (Show, Eq)

fromList :: [a] -> Cursor a
fromList [] = Cursor {next = [], selected = Nothing, previous = []}
fromList (x : xs) = Cursor {next = xs, selected = Just x, previous = []}

fromListSelected :: Int -> [a] -> Maybe (Cursor a)
fromListSelected index li = if length li < index + 1 || index < 0
    then Nothing
    else let (n, p) = splitAt index li
        in Just $ Cursor {next = reverse n, selected = Just (head p), previous = tail p}

toList :: Cursor a -> [a]
toList c = let  n = reverse $ next c
                s = selected c
                p = previous c
    in case s of
        Just sel -> n ++ [sel] ++ p
        Nothing -> []

selectFirst :: Ord a => Cursor a -> Maybe (Cursor a)
selectFirst c = case toList c of
    [] -> Nothing
    li -> Just $ fromList li

selectNext :: Ord a => Cursor a -> Maybe (Cursor a)
selectNext c = if next c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = tail cNext, selected = Just $ head cNext, previous = cSel : cPrev} 

selectPrevious :: Ord a => Cursor a -> Maybe (Cursor a)
selectPrevious c = if previous c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = cSel : cNext, selected = Just $ head cPrev, previous = tail cPrev} 
