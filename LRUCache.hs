{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           Control.Exception   (assert)
import           Control.Lens        (makeLenses, to, (&), (.~), (^.))
import           Control.Monad       (forM_, unless, when)
import           Data.Bifunctor      (second)
import           Data.Bool           (bool)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (fromJust, isNothing)
import           Data.Sequence       (Seq, ViewL (..), ViewR (..), viewl, viewr, (<|),
                                      (><))
import qualified Data.Sequence       as S
import           Prelude             hiding (length)
import           System.Random       (randomRIO)

{-
lru cache
not more than 5000 elements

accepts queries
recent queries are remembered

key → value
interface:
- V get(K key);
- put(K key, V value);


*implementation:*

two-linked list
Node contains key and value

Also has hash-map

get:
1. get from hashmap
2. move to head of list
3. return the value

put:
1. if not max size → add to head and hashmap
2. otherwise → add and also throw away the least recent one
-}

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen False _ a = a
applyWhen True f  a = f a

data LRUCache k v = LRUCache
    { _lruSeq   :: Seq k
    , _lruMap   :: HashMap k v
    , _lruLimit :: Int
    } deriving (Show)

$(makeLenses ''LRUCache)

seqInit :: Seq a -> (a, Seq a)
seqInit s = case viewr s of
    EmptyR    -> error "seqInit failed with null seq"
    (s' :> e) -> (e,s')

seqTail :: Seq a -> Seq a
seqTail s = case viewl s of
    EmptyL    -> error "seqTail failed with null seq"
    (_ :< s') -> s'

length :: LRUCache k v -> Int
length cache = cache ^. lruSeq . to S.length

empty :: Int -> LRUCache k v
empty i = assert (i >= 0) $ LRUCache S.empty M.empty i

put :: (Hashable k, Eq k) => k -> v -> LRUCache k v -> LRUCache k v
put k v c =
    assert (length c <= c ^. lruLimit) $
    c & lruSeq .~ newSeq & lruMap .~ newMap
  where
    oldSeq = c ^. lruSeq
    isFull = S.length oldSeq == c ^. lruLimit
    (removedK,cutSeq) = seqInit oldSeq
    newSeq = k <| bool oldSeq cutSeq isFull
    newMap = applyWhen isFull (M.delete removedK) $ M.insert k v $ c ^. lruMap

get :: (Hashable k, Eq k) => k -> LRUCache k v -> Maybe (LRUCache k v, v)
get k c =
    assert (length c <= c ^. lruLimit) $
    (c & lruSeq .~ newSeq,) <$>
    M.lookup k (c ^. lruMap)
  where
    newSeq = uncurry (><) . second seqTail . S.spanl (== k) $ c ^. lruSeq

test :: Int -> IO ()
test limit = do
    samples <- mapM (\i -> (i,) <$> randomRIO (1::Int,100)) [0..limit] -- limit+1 elems
    let c = foldl (\c' (k,v) -> put k v c') (empty limit) $ take limit samples
        (lK,lV) = last samples
        headK = fst $ head samples
    unless (isNothing $ get headK $ put lK lV c) $
        error "last element is not deleted"
    when (isNothing $ get headK $ put lK lV $ fst $ fromJust $ get headK c) $
        error "last element disappeared"
    forM_ (take limit samples) $ \(k,v) -> do
        let err = error $ "couldn't retrieve " ++ show k
        case get k c of
            Nothing     -> err
            Just (_,v') -> unless (v' == v) err

main :: IO ()
main = do
    forM_ [10..100] test
    putStrLn "_____________"
    putStrLn "|> success <|"
    putStrLn "^^^^^^^^^^^^^"
