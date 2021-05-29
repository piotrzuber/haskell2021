module Graph where
import Set(Set)
import qualified Set as Set
import Data.List((\\), nub)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
    empty       = Relation Set.empty Set.empty
    vertex a    = Relation (Set.Singleton a) Set.empty
    union a b   = Relation ((domain a) <> (domain b)) ((relation a) <> (relation b))
    connect a b = Relation ((domain a) <> (domain b)) ((relation a) <> (relation b) <> prod) 
        where prod = Set.fromList [(u, v) | u <- Set.toList (domain a), v <- Set.toList (domain b)]
                
instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Graph Basic where
    empty           = Empty
    vertex a        = Vertex a
    union a Empty   = a
    union Empty a   = a
    union a b       = Union a b
    connect a Empty = a
    connect Empty a = a
    connect a b     = Connect a b

instance Ord a => Eq (Basic a) where
    (==) a b = 
        let fba = fromBasic a
            fbb = fromBasic b
        in  domain fba == domain fbb && relation fba == relation fbb 

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty         = empty
fromBasic (Vertex a)    = vertex a
fromBasic (Union a b)   = union (fromBasic a) (fromBasic b)
fromBasic (Connect a b) = connect (fromBasic a) (fromBasic b)

instance (Ord a, Show a) => Show (Basic a) where
    show a = 
        let fba = fromBasic a
            dom = domain fba
            rel = relation fba
            isolatedVertices = nub (Set.toAscList dom) \\ nub (concat [[u, v] | (u, v) <- Set.toAscList rel])
        in "edges " ++ show(Set.toAscList rel) ++ " + vertices " ++ show isolatedVertices

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot a =
    let fba = fromBasic a
        dom = domain fba
        rel = relation fba
        isolatedVertices = nub (Set.toAscList dom) \\ nub (concat [[u, v] | (u, v) <- Set.toAscList rel])
        digraphFormattedEdges = foldr (\(u, v) z -> show u ++ " -> " ++ show v ++ ";\n" ++ z) "" $ Set.toAscList rel
        digraphFormattedVertices = foldr (\v z -> show v ++ ";\n" ++ z) "" isolatedVertices
    in "digraph {\n" ++ digraphFormattedEdges ++ digraphFormattedVertices ++ "}"

instance Functor Basic where
    fmap f Empty         = Empty
    fmap f (Vertex a)    = Vertex $ f a
    fmap f (Union a b)   = Union (fmap f a) (fmap f b)
    fmap f (Connect a b) = Connect (fmap f a) (fmap f b)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV u v uv a = fmap (\x -> if x == u || x == v then uv else x) a

instance Applicative Basic where
    pure                  = Vertex
    (<*>) Empty _         = Empty
    (<*>) (Vertex f) a    = fmap f a
    (<*>) (Union f g) a   = Union (f <*> a) (g <*> a)
    (<*>) (Connect f g) a = Connect (f <*> a) (g <*> a)

instance Monad Basic where
    return                = Vertex
    (>>=) Empty _         = Empty
    (>>=) (Vertex a) f    = f a
    (>>=) (Union a b) f   = Union (a >>= f) (b >>= f)
    (>>=) (Connect a b) f = Connect (a >>= f) (b >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV uv u v a = a >>= (\x -> if x == uv then Union (Vertex u) (Vertex v) else (Vertex x))
