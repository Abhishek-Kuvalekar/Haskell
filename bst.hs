data Tree k v = EmptyTree | Node k v (Tree k v) (Tree k v)
	deriving (Read, Eq)

singleton :: k -> v -> Tree k v
singleton k v = Node k v EmptyTree EmptyTree

insert :: (Ord k) => k -> v -> Tree k v -> Tree k v
insert k v EmptyTree = singleton k v
insert k v (Node a b left right)
	| k == a = Node a b left right
	| k < a = Node a b (insert k v left) right
	| k > a = Node a b left (insert k v right)

instance (Show k, Show v) => Show (Tree k v)
	where
   show t = drawTree t " "

drawTree :: (Show k, Show v) => Tree k v -> String -> String
drawTree EmptyTree spaces = spaces ++ "*\n"
drawTree (Node k v EmptyTree EmptyTree) spaces = spaces ++ "(" ++ show k ++ "," ++ show v ++ ")" ++ "\n"
drawTree (Node k v tl tr) spaces = spaces++ "(" ++ show k ++ "," ++ show v ++ ")" ++ "\n" ++ drawTree tl (' ':' ':spaces) ++ drawTree tr (' ':' ':spaces)
