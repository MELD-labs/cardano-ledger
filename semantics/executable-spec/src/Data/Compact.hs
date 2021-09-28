{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- HeapWords for Array and PrimArray
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Compact where

import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
import qualified Data.Array.MArray as MutA
import GHC.Arr(STArray(..),unsafeFreezeSTArray)
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.ByteString.Short (ShortByteString,toShort,fromShort)
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Data.Primitive.Types (Prim (..))
import Cardano.Prelude (HeapWords (..))
import GHC.Exts( IsList (toList) )
import Control.Monad.ST (ST, runST)
import Data.List(sort,sortBy)
import Debug.Trace(trace)
import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    serialize',
    unsafeDeserialize',
  )
import Data.Text(Text,pack)  

shorten :: ToCBOR t => t -> ShortByteString
shorten x = toShort (serialize' x)
-- ============================================================================================
-- Arrays that support Binary search

class Indexable t a where
  index :: t a -> Int -> a
  isize :: t a -> Int
  fromlist :: [a] -> t a
  tolist :: t a -> [a]

-- | Find the index of 'k'. Use 'lo' and 'hi' to narrow the scope where 'k' may occur
--   This is possible because we assume 'arr' is maintained in ascending order of keys.
binsearch :: (Ord k, Indexable arr k) => Int -> Int -> k -> arr k -> Maybe Int
binsearch lo hi _k _v | lo > hi = Nothing
binsearch lo hi k v | lo == hi = if index v lo == k then Just lo else Nothing
binsearch lo _hi k v | index v lo == k = Just lo
binsearch _lo hi k v | index v hi == k = Just hi
binsearch lo hi _k _v | lo + 1 == hi = Nothing
binsearch lo hi k v = (if index v mid > k then binsearch lo mid k v else binsearch mid hi k v)
  where
    mid = lo + (div (hi - lo) 2)


-- | Find the index and the value at the least upper bound of 'target'
alub :: (Ord t1, Indexable t2 t1) => (Int, Int) -> t2 t1 -> t1 -> Maybe (Int, t1)
alub (lo, hi) arr target
  | lo > hi = Nothing
  | target <= index arr lo = Just (lo, index arr lo)
  | lo == hi = Nothing
  | lo + 1 == hi && index arr lo < target && target <= index arr hi = Just (hi, index arr hi)
  | True = if target <= index arr mid then (alub (lo, mid) arr target) else (alub (mid, hi) arr target)
  where
    mid = lo + (div (hi - lo) 2)

-- Instances

instance Indexable PA.Array x where
  index = PA.indexArray
  isize = PA.sizeofArray
  fromlist = PA.arrayFromList
  tolist arr = foldr (:) [] arr

instance Prim a => Indexable PrimArray a where
  index = indexPrimArray
  isize = sizeofPrimArray
  fromlist = primArrayFromList
  tolist = toList

instance Indexable (A.Array Int) a where
  index = (A.!)
  isize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr
  fromlist xs = (A.listArray (0, length xs -1) xs)
  tolist arr = foldr (:) [] arr

-- =======================================================
-- Abtract Searchable types

class Ord key => Search t key where
  search :: key -> t -> Maybe Int

instance Ord key => Search (PA.Array key) key
   where search key v = binsearch 0 (isize v - 1) key v
instance (Prim key,Ord key) => Search (PrimArray key) key
   where search key v = binsearch 0 (isize v - 1) key v
instance Ord key => Search (A.Array Int key) key
   where search key v = binsearch 0 (isize v - 1) key v

instance Search t key => Search [t] key where
   search _ [] = Nothing
   search key (x:xs) =
     case search key x of
       Nothing -> search key xs
       Just i -> Just i

instance Search t key => Search (Node t) key where
   search key (Node _ x) = search key x

instance Search t key => Search (t,x)  key where
  search key (t,x) = search key t

-- ========================================================
-- Pairs of Mutable Arrays and ImMutable Arrays that can be converted

class Indexable arr a => ArrayPair arr marr a | marr -> arr, arr -> marr where
  mindex :: marr s a -> Int -> ST s a
  msize :: marr s a -> Int
  mnew :: Int -> ST s (marr s a)
  mfreeze :: marr s a -> ST s (arr a) -- This should be the unsafe version that does not copy
  mwrite :: marr s a -> Int -> a -> ST s ()

instance ArrayPair PA.Array PA.MutableArray a where
  msize = PA.sizeofMutableArray 
  mindex = PA.readArray
  mnew n = PA.newArray n undefined
  mfreeze = PA.unsafeFreezeArray
  mwrite = PA.writeArray

instance Prim a => ArrayPair PrimArray MutablePrimArray a where
  msize = sizeofMutablePrimArray
  mindex = readPrimArray
  mnew = newPrimArray  
  mfreeze = unsafeFreezePrimArray
  mwrite = writePrimArray

newtype MutArray s t = MutArray (STArray s Int t)

instance ArrayPair (A.Array Int) MutArray a where
  msize (MutArray (STArray lo hi _ _)) = hi - lo + 1 
  mindex (MutArray arr) i = MutA.readArray arr i
  mnew n = MutArray <$> (MutA.newArray_ (0,n-1))
  mfreeze (MutArray arr) = unsafeFreezeSTArray arr
  mwrite (MutArray arr) i a = MutA.writeArray arr i a

-- ============================================================

-- | An array where some indices have been nix'ed, or marked as deleted
data NixArr arr k where
  NixArr :: Indexable arr k => {-# UNPACK #-} !Int  -> -- Actual elements (size arr - size delete-set)
                               !(arr k) ->
                               !(CompactPrimSet Int) ->
                               NixArr arr k

instance (Show k,Indexable arr k) => Show (NixArr arr k) where
  show x = show (tolist x)

instance Search (arr k) k => Search (NixArr arr k) k where
  search key (NixArr _ arr del) =
    case search key arr of
      Nothing -> Nothing
      Just i -> if hasElem i del then Nothing else (Just i)

instance (Indexable arr k) => Indexable (NixArr arr) k where
  index (NixArr _ arr del) k = index arr k 
  isize (NixArr _ arr del) = isize arr
  fromlist xs =  NixArr (length xs) (fromlist xs) emptyset
  tolist (NixArr _ arr del) = removeNixedIndices del arr

removeNixedIndices del xs = help (zip [0..] (tolist xs))
   where help ((i,x):more) = if hasElem i del then help more else x : help more
         help [] = []

narr1 :: NixArr PrimArray Int
narr1@(NixArr nsize1 ys _) = fromlist [ i | i <- [0..8]]
narr2 = NixArr (nsize1 - 2) ys (makeSet [3,6])


-- ===========================================================================
-- An array where the elements are stored serialized in groups of fixed size

-- These two functions control how to serialize
toBytes :: ToCBOR a => a -> ShortByteString
toBytes x = toShort (serialize' x)

fromBytes :: FromCBOR a => ShortByteString -> a
fromBytes x = (unsafeDeserialize' (fromShort x))

-- | How many slots do you need to store 'nominalsize' elements if they are serialized into
--   groups, where each group is a list of length 'groupsize'. The last group may have fewer items.
serialSize nominalsize groupsize = (nominalsize `div` groupsize) + fix
  where fix = if (nominalsize `mod` groupsize)==0 then 0 else 1

-- | Chop a list into 'groupsize' sub lists
--   Invariant: (length (chopN groupsize xs) == serialSize (length xs))
chopN :: Int -> [t] -> [[t]]
chopN groupsize [] = []
chopN groupsize xs = take groupsize xs : chopN groupsize (drop groupsize xs)

data SerialArray arr v where
   SerialArray:: (Indexable arr ShortByteString,FromCBOR v) =>
       {-# UNPACK #-} !Int -> -- Nominal size of the arr. Indices run from [0..nominal -1]
       {-# UNPACK #-} !Int -> -- groupsize, so the actual size of the array is (serialSize nominalsize groupsize)
       arr ShortByteString ->
       SerialArray arr v

instance Show t => Show (SerialArray arr t) where
   show (SerialArray nominal group arr) = show bytes
     where bytes = concat $ map (fromBytes @[t]) (tolist arr)

instance (Indexable arr ShortByteString,ToCBOR t, FromCBOR t) => Indexable (SerialArray arr) t where
   isize (SerialArray nominal _ _) = nominal
   index (SerialArray nominal groupsize arr) i =
     if (i >=0) && (i < nominal)
        then (fromBytes (index arr (i `div` groupsize))) !! (i `mod` groupsize)
        else error ("index "++show i++" out of SerialArray nominal range (0,"++show(nominal-1)++").")
   fromlist = serialArrayFromlist @t 10
   tolist (SerialArray _ _ arr) = concat (map (fromBytes @[t]) (tolist arr))


serialArrayFromlist :: forall t arr. (ToCBOR t,FromCBOR t,Indexable arr ShortByteString) => Int -> [t] -> SerialArray arr t
serialArrayFromlist groupsize ts = SerialArray nominalsize groupsize arr
  where nominalsize = length ts
        arr = fromlist (map (toBytes @[t]) (chopN groupsize ts))

sa2 :: SerialArray (A.Array Int) Text
sa2 = serialArrayFromlist 4 (map (pack . show) [1..15])


mergeParSerialNodes ::
  ( ArrayPair arr2 marr2 ShortByteString,
    ArrayPair arr1 marr1 k,
    MergeParallel arr1 k,
    Ord k,
    ToCBOR v
  ) =>
  [(Int, Node (arr1 k, SerialArray arr2 v))] ->
  Int ->
  Int ->
  (arr1 k, arr2 ShortByteString, (Int, Int, [v]))
mergeParSerialNodes xs s1 s2 = with2MutArray s1 s2 (builder xs) where
  builder xs marr1 marr2 = inOrder smallerP doneP (action marr1 marr2) xs (pure(0,0,[]))
  action marr1 marr2 comp (_size,Node _ (keys,vals@(SerialArray _ _ _))) =
     do { (i,j,vs) <- comp
        ; mwrite marr1 i (index keys i)
        ; let vs2 = (index vals i) : vs
        ; if length vs == 10 {- group size might vary -}
             then do { mwrite marr2 j (toBytes (reverse vs)); pure(i+1,j+1,[]) }
             else pure(i+1,j,vs2) }
            

-- ================================================================
-- Functions for using mutable initialization in a safe manner.
-- Using these functions is the safe way to use the method 'mfreeze'

withMutArray:: ArrayPair arr marr a => Int -> (forall s. marr s a -> ST s x) -> (arr a,x)
withMutArray n process = runST $ do
  marr <- mnew n
  x <- process marr
  arr <- mfreeze marr
  pure (arr, x)

with2MutArray ::
  ( ArrayPair arr1 marr1 a, ArrayPair arr2 marr2 b) =>
  Int ->
  Int ->
  (forall s. marr1 s a -> marr2 s b -> ST s x) ->
  (arr1 a, arr2 b,x)
with2MutArray size1 size2 process = runST $ do
  arr1 <- mnew size1
  arr2 <- mnew size2
  x <- process arr1 arr2
  arr3 <- mfreeze arr1
  arr4 <- mfreeze arr2
  pure (arr3, arr4, x)

-- ================================================
-- Merging sorted arrays

class Indexable t a => MergeVector t a where
  smallerV:: Ord a => (Int,Node (t a)) -> (Int,Node (t a)) -> Bool
  smallerV (i1,Node _ arr1) (i2,Node _ arr2) = index arr1 i1 < index arr2 i1
  doneV:: (Int,Node (t a)) -> Maybe (Int,Node (t a))
  doneV (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  actionV:: ans -> (Int,Node (t a)) -> ans
  actionV ans _ = ans

class Indexable t a => MergeParallel t a where
  smallerP:: Ord a => (Int,Node (t a,b)) -> (Int,Node (t a,b)) -> Bool
  smallerP (i1,Node _ (arr1,_)) (i2,Node _ (arr2,_)) = index arr1 i1 < index arr2 i1
  doneP:: (Int,Node (t a,b)) -> Maybe (Int,Node (t a,b))
  doneP (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  actionP:: ans -> (Int,Node (t a,b)) -> ans
  actionP ans _ = ans


-- ================================================
-- Merging N-sorted arrays

-- | A node carries a 'size' and some array-like type 'arr'
data Node arr = Node {-# UNPACK #-} !Int arr
  deriving Show

-- | Split a list into (smallest-element, larger-elements). 'x' is the smallest seen so far.
smallest :: (a -> a -> Bool) -> a -> [a] -> [a] -> (a, [a])
smallest _ x [] larger = (x,larger)
smallest smaller x (y:ys) larger =
   if smaller x y
      then smallest smaller x ys (y:larger)
      else smallest smaller y ys (x:larger)

-- | Build an 'ans' by applying 'action' in ascending order of 't'.
--   Works by choosing the smallest 't' in each pass (call). The 'done' function
--   returns (Just more) when the 't' object has more things to process.
inOrder :: 
   (t -> t -> Bool) ->
   (t -> Maybe t) ->
   (ans -> t -> ans) ->
   [t] -> ans -> ans  
inOrder smaller done action items ans = loop items ans where
   loop [] ans = ans
   loop (x:xs) ans = 
     case smallest smaller x xs [] of
       (small,larger) -> case done small of
          Nothing ->  loop larger (action ans small)
          Just more -> loop (more:larger) (action ans small)

-- =========================

-- | Merge many Nodes into 1 Node. In the pattern (i,Node size arr). 'i' is the next entry in 'arr'
--   that is ready to be merged into the output.
mergeMany:: (ArrayPair arr marr k, Ord k) => [(Int, Node (arr k))] -> marr s k -> ST s Int
mergeMany xs arr = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ xs) (j,Node _ ys) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ xs) = do { count <- n; mwrite arr count (index xs i); pure(count+1)}

-- | Merge 'ns' into one Node with 'size' entries.
mergeNodes ::
  ( ArrayPair arr marr key,
    Ord key
  ) => Int -> [Node (arr key)] -> Node (arr key)
mergeNodes size ns = Node size (project(withMutArray size (mergeMany (map start ns))))
  where start x = (0,x)
        project (x,_) = x

-- =========================

-- | Merge many Nodes into 1 Node. Each node carries a set of parallel arrays.
--   In the pattern (i,Node size (keys,vals)). 'i' is the next entry
--   in 'keys' and 'vals' that is ready to be merged into the output.
mergeManyParallel::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) =>
  [(Int, Node (arr1 k, arr2 v))] -> marr1 s k -> marr2 s v -> ST s Int
mergeManyParallel xs marr1 marr2 = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ (xs,_)) (j,Node _ (ys,_)) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ (keys,vals)) =
     do { count <- n
        ; mwrite marr1 count (index keys i)
        ; mwrite marr2 count (index vals i)
        ; pure(count+1)}

-- | Merge 'ns' (which carry parallel arrays) into one Node with 'size' entries.
mergeParallel ::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) => Int -> [Node (arr1 k, arr2 v)] -> Node (arr1 k, arr2 v)
mergeParallel size ns = Node size (project(with2MutArray size size (mergeManyParallel (map start ns))))
    where start x = (0,x)
          project (x,y,_) = (x,y)

-- =========================

mergeManyNixParallel::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) =>
  [(Int, Node (NixArr arr1 k, arr2 v))] -> marr1 s k -> marr2 s v -> ST s Int
mergeManyNixParallel xs marr1 marr2 = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ (NixArr _ xs d1,_)) (j,Node _ (NixArr _ ys d2,_)) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ (NixArr size keys del,vals)) =
     do { count <- n
        ; if hasElem count del
             then pure count
             else do { mwrite marr1 count (index keys i)
                     ; mwrite marr2 count (index vals i)
                     ; pure(count+1) } }

mergeNixParallel ::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) => Int -> [Node (NixArr arr1 k, arr2 v)] -> Node (NixArr arr1 k, arr2 v)
mergeNixParallel size ns = Node size (project(with2MutArray size size (mergeManyNixParallel (map start ns))))
    where start x = (0,x)
          actualSize = sum(map (\ (Node _ (NixArr size _ _,_)) -> size) ns)
          project (x,y,_) = (NixArr actualSize x emptyset,y)

-- A simple test
rrr :: [Int]
rrr = inOrder smaller done action [[1,4,7],[3],[11,34,78,99,145],[2,6,8,9]] []
  where smaller (x:xs) (y:ys) = x < y
        done (x:y:zs) = Just(y:zs)
        done _ = Nothing
        action ans (x:_) = x:ans


        
-- ================================================================================
-- Sets of objects as lists of nodes of ascending size. Each size is a power of 2.
-- In each (Node size primarray) the 'primarray' has 'size' components. The sum
-- of the sizes is the total number of elements in the set. 

newtype CompactPrimSet t = CompactPrimSet [Node (PrimArray t)]

instance (Prim t,Show t) => Show(CompactPrimSet t) where
  show (CompactPrimSet ts) = show(map unNode ts)
    where unNode(Node _ xs) = tolist xs

emptyset :: CompactPrimSet t
emptyset = CompactPrimSet []

hasElem :: (Prim t,Ord t) => t -> CompactPrimSet t -> Bool
hasElem t (CompactPrimSet xs) =
  case search t xs of
    Nothing -> False
    Just _ -> True

insertElem :: (Prim t, Ord t) => t -> CompactPrimSet t -> CompactPrimSet t
insertElem t (set@(CompactPrimSet nodes)) =
  if hasElem t set
     then set
     else case splitAtFullPrefix 1 (Node 1 (fromlist [t])) nodes of
           (size,prefix,tail) -> CompactPrimSet ((mergeNodes size prefix):tail)

makeSet :: (Ord t,Prim t) => [t] -> CompactPrimSet t
makeSet ts = CompactPrimSet (map node nodes) where
    nodes = pieces ts
    node (n, ps) = Node n (fromlist (sort ps))  


ss1, ss2, ss3, ss4, ss5, ss6, ss7, ss8, ss9 :: CompactPrimSet Int
ss1 = CompactPrimSet []
ss2 = insertElem 99 ss1
ss3 = insertElem 33 ss2
ss4 = insertElem 12 ss3
ss5 = insertElem 6 ss4
ss6 = insertElem 22 ss5
ss7 = insertElem 71 ss6
ss8 = insertElem 81 ss7
ss9 = insertElem 51 ss8

-- ==============================================================
-- Overloaded operations on (Map k v)

class Maplike m k v where
  makemap :: [(k,v)] -> m k v
  lookupmap :: Ord k => k -> m k v -> Maybe v
  insertmap :: Ord k => k -> v -> m k v -> m k v

-- ===========================================================
-- (Map key value) as lists of nodes of ascending size. Each size is a power of 2.
-- In each (Node size (keyArr,valArr)) the 'keyArr' and 'valArr' have 'size' components.
-- These are parallel arrays. The key at index i keyArr, has its associated value at index i valArr
-- The sum of the sizes is the total number of elements in the set. 

newtype PrimMap key val = PrimMap [Node (PrimArray key,PA.Array val)]

instance (Prim k,Show k,Show v) => Show(PrimMap k v) where
  show (PrimMap ts) = "PrimMap\n   "++show keys++"\n   "++show vals
    where unNode(Node _ (x,y)) = (tolist x, tolist y)
          pairs = map unNode ts
          (keys,vals) = unzip pairs

instance (Ord k,Prim k) => Maplike PrimMap k v where
   lookupmap key (PrimMap []) = Nothing
   lookupmap key (PrimMap (Node _ (ks,vs) : more)) =
     case search key ks of
       Nothing -> lookupmap key (PrimMap more)
       Just i -> Just(index vs i)

   insertmap k v (PrimMap nodes) =
      case splitAtFullPrefix 1 (Node 1 (fromlist [k],fromlist[v])) nodes of
         (size,prefix,tail) -> PrimMap ((mergeParallel size prefix):tail)

   makemap = makeMap

makeMap :: (Prim k,Ord k) => [(k,v)] -> PrimMap k v
makeMap pairs = PrimMap (map node nodes) where
    nodes = pieces pairs
    node (n, ps) = Node n (fromlist ks, fromlist vs)
      where (ks,vs) = unzip (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) ps)

m2,m3 :: PrimMap Int String
m2 = makeMap [(i,show i) | i <- [1..21]]
m3 = foldl accum (PrimMap []) (reverse [(i,show i) | i <- [1..21 ::Int]])
  where accum ans (k,v) = insertmap k v ans

-- =====================================================================
-- NixMap supports deletion, and overwriting insertion

newtype NixMap k v = NixMap [Node (NixArr PrimArray k,PA.Array v)]

instance (Prim k,Show k,Show v) => Show(NixMap k v) where
  show (NixMap ts) = "NixMap\n   "++show keys++"\n   "++show vals
    where unNode(Node _ (x@(NixArr _ _ ds),y)) = (tolist x,removeNixedIndices ds y)
          pairs = map unNode ts
          (keys,vals) = unzip pairs

instance (Ord k,Prim k) => Maplike NixMap k v where
   makemap = makeNixMap
   lookupmap k (NixMap []) = Nothing
   lookupmap k (NixMap ((Node _ (NixArr _ keys ds,vals)):more)) =
      case search k keys of
        Nothing -> lookupmap k (NixMap more)
        Just i -> if hasElem i ds
                     then Nothing
                     else Just(index vals i)
   insertmap = insertNixMap

makeNixMap :: (Prim k,Ord k) => [(k,v)] -> NixMap k v
makeNixMap pairs = NixMap (map node nodes) where
    nodes = pieces pairs
    node (n, ps) = Node n (fromlist ks, fromlist vs)
      where (ks,vs) = unzip (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) ps)

insertNixMap :: (Prim k,Ord k) => k -> v -> NixMap k v -> NixMap k v
insertNixMap k v (NixMap nodes) =
   case splitAtFullPrefix 1 (Node 1 (fromlist [k],fromlist[v])) (findAndMark k nodes) of
         (size,prefix,tail) -> NixMap ((mergeNixParallel size prefix):tail)
  
deleteNixMap :: (Prim k, Ord k) => k -> NixMap k v -> NixMap k v
deleteNixMap k (NixMap nodes) = NixMap(findAndMark k nodes)

findAndMark :: (Search (arr1 k) k) => k -> [Node (NixArr arr1 k,arr2 v)] -> [Node (NixArr arr1 k,arr2 v)]
findAndMark k [] = []
findAndMark k (nodes@(node@(Node n (NixArr m ks ds,vs)) : more )) =
  case search k ks of
    Nothing -> node : findAndMark k more
    Just i -> if hasElem i ds
                 then nodes
                 else (Node n (NixArr (m+1) ks (insertElem i ds),vs)) : more

nm1 :: NixMap Int String
nm1 = makemap [(i,show i) | i <- [1..7]]

-- ============================================================
-- Code for binary merging of adjacent nodes, instead of n-ary

addSetNode :: (Ord t, Prim t) => Node (PrimArray t) -> [Node (PrimArray t)] -> [Node (PrimArray t)]
addSetNode node [] = [node]
addSetNode (node@(Node n arr1)) (nodes@((Node m arr2) : more))
  | n < m = node : nodes
  | n == m = addSetNode (mergeSetNodes n m arr1 arr2) more
  | True = error ("SetNodes not in ascending Order.")

mergeSetNodes :: forall t. (Ord t, Prim t) => Int -> Int -> PrimArray t -> PrimArray t -> Node (PrimArray t)
mergeSetNodes size1 size2 arr1 arr2 = Node totsize (fst (withMutArray totsize make))
  where
    totsize = size1 + size2
    make :: forall s. MutablePrimArray s t -> ST s ()
    make marr = loop 0 0 0
      where
        loop :: Int -> Int -> Int -> ST s ()
        loop i1 i2 next =
          case (i1 < size1, i2 < size2, next < totsize) of
            (True, True, True) ->
              let x1 = index arr1 i1
                  x2 = index arr2 i2
               in case compare x1 x2 of
                    LT -> do writePrimArray marr next x1; loop (i1 + 1) i2 (next + 1)
                    GT -> do writePrimArray marr next x2; loop i1 (i2 + 1) (next + 1)
                    EQ -> error ("Duplicates in mergeSetNodes.")
            (True, False, True) -> do writePrimArray marr next (index arr1 i1); loop (i1 + 1) i2 (next + 1)
            (False, True, True) -> do writePrimArray marr next (index arr2 i2); loop i1 (i2 + 1) (next + 1)
            _ -> pure ()

-- =======================================================
-- Encoding lists with the structure of binary numbers

-- | binary encoding of 'n', least significant bit on the front of the list
binary :: Int -> [Int]
binary 0 = []
binary 1 = [(1)]
binary n = (mod n 2) : binary (div n 2)

-- | Compute a sparse list of non-zero Binary digits and their positional weights to represent 'n'
--   For example (sparseBinary 25) returns [(1,1),(1,8),(1,16)], I.e. we need: 1 one,
--   1 eight, and 1 sixteen.  Since this is binary, and we don't store the 0's, the digits are aways 1.
--   and the weights are powers of 2.
sparseBinary :: Int -> [(Int, Int)]
sparseBinary n = fix 1 (binary n)
  where
    fix _ [] = []
    fix m (x : xs) =
      if x == 0
        then fix (m * 2) xs
        else (x, m) : fix (m * 2) xs

-- | Split a list of length 'n' into pieces, each piece has a power of two as its length.
-- For example:  pieces [1..11]  -->  [(1,[1]), (2,[2,3]), (8,[4,5,6,7,8,9,10,11])]
pieces :: [a] -> [(Int, [a])]
pieces xs = chop parts xs
  where
    parts = sparseBinary (length xs)


chop [] _zs = []
chop ((_, n) : ys) zs = (n, take n zs) : chop ys (drop n zs)

-- =========================================================
-- HeapWords instances

instance (HeapWords v) => HeapWords (A.Array Int v) where
  heapWords arr = foldl accum (3 + n) arr
    where
      accum ans v = ans + heapWords v
      n = isize arr

instance (Prim a, HeapWords a) => HeapWords (PrimArray a) where
  heapWords arr = 2 + (sizeofPrimArray arr * heapWords (index arr 0))

-- =================================================

-- | Looking for a full prefix, i.e. a prefix which has contiguous powers of two
--   for example: splitAtFullPrefix 1 (Node 1 1) [Node 1 1,Node 2 2, Node 4 4, Node 8 8,Node 32 32,Node 128 128]
--   returns (16, [Node 1 1,Node 1 1,Node 2 2,Node 4 4,Node 8 8], [Node 32 32,Node 128 128])
--   because [1,2,4,8] is the longest contiguous prefix consisting of adjacent powers of 2.
splitAtFullPrefix :: Int -> Node a -> [Node a] -> (Int,[Node a],[Node a])
splitAtFullPrefix next (node@(Node n _)) [] = (n,[node],[])
splitAtFullPrefix next (node1@(Node n _)) ((node2@(Node m _)):more) =
  if next==m
     then case splitAtFullPrefix (next*2) node2 more of
            (count,prefix,rest) -> (count+n, node1:prefix, rest)
     else (n,[node1],node2:more)

sp1 = splitAtFullPrefix 1 (Node 1 1) [Node 1 1,Node 2 2, Node 4 4, Node 8 8,Node 32 32,Node 128 128]






-- ========================================================
-- efficient merge of 2 sorted arrays

-- Mergeing two arrays using an abstract 'cont' to add things
mergeArrWithCont ::
   forall t arr ans. (Ord t,Indexable arr t) =>
      Int ->
      Int ->
      arr t ->
      arr t -> ans -> (ans -> t -> Either Int Int -> ans) -> ans
mergeArrWithCont size1 size2 arr1 arr2 !ans cont = loop 0 0 0 ans
  where
    totsize = size1 + size2
    loop :: Int -> Int -> Int -> ans -> ans
    loop i1 i2 next answer =
      case (i1 < size1, i2 < size2, next < totsize) of
        (True, True, True) ->
          let x1 = index arr1 i1
              x2 = index arr2 i2
          in case compare x1 x2 of
               LT -> loop (i1 + 1) i2 (next + 1) (cont answer x1 (Left i1))
               GT -> loop i1 (i2 + 1) (next + 1) (cont answer x2 (Right i2))
               EQ -> error ("Duplicates in mergeArrWithCont.")
        (True, False, True) -> loop (i1 + 1) i2 (next + 1) (cont answer (index arr1 i1) (Left i1))
        (False, True, True) -> loop i1 (i2 + 1) (next + 1) (cont answer (index arr2 i2) (Right i2))
        _ -> answer

mergeParallel2 :: (ArrayPair vals mvals v, ArrayPair keys mkeys key, Ord key,Indexable arr key) =>
     Int ->
     Int -> 
     arr key ->
     arr key ->
     vals v ->
     vals v ->
     (keys key, vals v, ())
mergeParallel2 size1 size2 keys1 keys2 vals1 vals2 =
   with2MutArray (size1+size2) (size1+size2) $
   (\ m1 m2 -> const () <$> mergeArrWithCont size1 size2 keys1 keys2 (pure 0) (action vals1 vals2 m1 m2))

getEither:: (Indexable t1 p) => t1 p -> t1 p -> Either Int Int -> p
getEither v1 v2 (Left i)  = index v1 i
getEither v1 v2 (Right i) = index v2 i

action :: (ArrayPair keys mkeys key, ArrayPair vals mvals v) =>
  vals v ->
  vals v ->
  mkeys s key ->
  mvals s v ->
  ST s Int ->
  key ->
  Either Int Int ->
  ST s Int    
action v1 v2 mkeys mvals indexM k eitheri = do
   !index <- indexM
   mwrite mkeys index k
   mwrite mvals index (getEither v1 v2 eitheri)
   pure(index + 1)