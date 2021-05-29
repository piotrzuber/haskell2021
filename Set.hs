module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List(group, sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _     = False

member :: Eq a => a -> Set a -> Bool
member _ Empty         = False
member a (Singleton b) = a == b
member a (Union b c)   = member a b || member a c

singleton :: a -> Set a
singleton a = Singleton a

fromList :: [a] -> Set a
fromList []      = Empty
fromList [a]     = Singleton a
fromList (a: as) = Union (Singleton a) (fromList as)

toList :: Set a -> [a]
toList Empty         = []
toList (Singleton a) = [a]
toList (Union a b)   = (toList a) ++ (toList b)

toAscList :: Ord a => Set a -> [a]
toAscList a = map head $ group $ sort $ toList a

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union a Empty = a
union Empty a = a
union a b     = Union a b

insert :: a -> Set a -> Set a
insert a b = Union (Singleton a) b

instance Ord a => Eq (Set a) where
    (==) a b = toAscList a == toAscList b

instance Semigroup (Set a) where
    (<>) = union

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show a = show (toList a)

instance Functor Set where
    fmap f Empty         = Empty
    fmap f (Singleton a) = Singleton $ f a
    fmap f (Union a b)   = Union (fmap f a) (fmap f b)
