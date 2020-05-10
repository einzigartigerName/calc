module Dictionary where

data Element a b = Element (a, b)
    deriving Show

data Dictionary a b = Dictionary [Element a b]
    deriving Show

asList :: Dictionary a b -> [(a, b)]
asList (Dictionary dict) = [(key, value) | (Element (key, value)) <- dict]

insert :: Dictionary a b -> a -> b -> Dictionary a b
insert (Dictionary dict) key value = Dictionary (Element (key, value) : dict)

find :: Dictionary a b -> a -> (a -> a -> Bool) -> Maybe b
find (Dictionary []) _ _ = Nothing
find (Dictionary (Element (k, v) : es)) key f = if f key k
    then Just v
    else find (Dictionary es) key f

new :: Dictionary a b
new = Dictionary []