{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-|
Module      : Tries
Description : An autocomplete implementation along with functions for working with tries.
Copyright   : (c) Ian Waudby-Smith, 2019
License     : BSD3
Maintainer  : iwaudbysmith@gmail.com
Stability   : experimental
Portability : POSIX

This module provides an implementation of autocomplete along with functions for working with tries.
-}


module Tries where

import Debug.Trace
import Data.List
import Data.Maybe
import qualified Data.Heap as Heap
import Data.MinMaxQueue (MinMaxQueue)
import qualified Data.MinMaxQueue as MMQ
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import qualified Data.Text.IO as TextIO
import System.FilePath
import System.IO
import GHC.Generics (Generic)
import Control.DeepSeq

-- | Trie + autocomplete examples in a REPL environment
--
-- >>> import Tries
-- >>> mytrie = trieFromList [(30, "an"), (12, "to"), (25, "ted"), (20, "a"), (14, "tea")]
-- >>> mytrie
-- a 20.0 30.0
--   n 30.0 -1.0
-- t -1.0 25.0
--   o 12.0 -1.0
--   e -1.0 25.0
--     a 14.0 -1.0
--     d 25.0 -1.0
-- >>> mytrie_augmented = insertWords [(5, "in"), (15, "inn")] mytrie
-- >>> mytrie_augmented
-- a 20.0 30.0
--    n 30.0 -1.0
-- t -1.0 25.0
--    o 12.0 -1.0
--    e -1.0 25.0
--       a 14.0 -1.0
--       d 25.0 -1.0
-- i -1.0 15.0
--    n 5.0 15.0
--       n 15.0 -1.0
-- isInTrie "inn" mytrie
-- False
-- isInTrie "inn" mytrie_augmented
-- True
-- >>> removePrefix "inn"  mytrie_augmented
-- a 20.0 30.0
--    n 30.0 -1.0
-- t -1.0 25.0
--    o 12.0 -1.0
--    e -1.0 25.0
--       a 14.0 -1.0
--       d 25.0 -1.0
-- i -1.0 5.0
--    n 5.0 -1.0
-- >>> updateWeight sqrt "ted" mytrie_augmented
-- a 20.0 30.0
--    n 30.0 -1.0
-- t -1.0 14.0
--    o 12.0 -1.0
--    e -1.0 14.0
--       a 14.0 -1.0
--       d 5.0 -1.0
-- i -1.0 15.0
--    n 5.0 15.0
--       n 15.0 -1.0
-- >>> mytrie_augmented -- Notice that this trie was never modified.
-- a 20.0 30.0
--    n 30.0 -1.0
-- t -1.0 25.0
--    o 12.0 -1.0
--    e -1.0 25.0
--       a 14.0 -1.0
--       d 25.0 -1.0
-- i -1.0 15.0
--    n 5.0 15.0
--       n 15.0 -1.0
-- >>> autocomplete "t" mytrie 3
-- [(25.0,"ted"),(14.0,"tea"),(12.0,"to")]
-- >>> autocomplete "in" mytrie 5
-- []
-- >>> autocomplete "in" mytrie_augmented 5
-- [(15.0,"inn"),(5.0,"in")]



-- | The generic Trie data structure.
data Trie =
  -- | The empty trie
  EmptyTrie |
  -- | Non-empty tries are prefix nodes
  PrefixNode
  {
    -- | A string of length one (or 0 if we're at the root node)
    prefix       :: String,
    -- | The weight of the prefix. This is -1 for non-words and non-negative for words
    weight       :: Double,
    -- | The maximum weight of all children of this node
    max_child_wt :: Double,
    -- | A HashMap of the children. Keys are prefixes, values are tries
    children     :: (HashMap String Trie)
  }
          deriving (Eq, Generic, NFData)

-- | Override "show" so that it displays something more "tree" like
instance Show Trie where
  show trie =
    if trie == EmptyTrie then
      "EmptyTrie"
    else
      displayTrieAtLevel trie 0
    where displayTrieAtLevel t l =
            if (Tries.prefix t == "") then
              (intercalate "" $
               map (\x -> displayTrieAtLevel x 0) (getChildrenAsList t))
            else
              -- print out the prefix, weight, max_child_wt
              (intercalate " " $ replicate l "  " ++
               [Tries.prefix t, show $ Tries.weight t, show $ Tries.max_child_wt t]) ++
              "\n" ++
              -- run displayTrieAtLevel on all children, with increased level
              (intercalate "" $
               map (\x -> displayTrieAtLevel x (l + 1)) (getChildrenAsList t))

-- | The root node from which we can build more interesting 'Trie's.
rootNode :: Trie
rootNode = PrefixNode "" (-1) (-1) HashMap.empty

-- * Higher-order functions for children of Tries

-- | Map a function on the 'children' of a 'Trie', and return the updated 'Trie'.
mapChildren :: (Trie -> Trie) -> Trie -> Trie
mapChildren fn trie =
  -- update the trie's max child weights
  updateMaxChildWeight $
  trie
  {
    children = HashMap.map
               -- apply fn but also update the max child weight of every child
               (updateMaxChildWeight . fn) $
               Tries.children trie
  }

-- | Filter 'children' of a 'Trie' out that don't satisfy a given predicate,
-- and return the 'Trie'.
filterChildren :: (Trie -> Bool) -> Trie -> Trie
filterChildren predicate trie =
  -- update the trie's max child weights
  updateMaxChildWeight
  -- update the children to be the filtered hash map
  trie{ children = HashMap.filter predicate $ Tries.children trie }

-- | Reduce the 'children' of a 'Trie' using a binary function and a starting value.
foldrChildren :: (Trie -> a -> a) -> a -> Trie -> a
foldrChildren fn start trie =
  -- simply perform a foldr on all the children in the trie
  HashMap.foldr fn start $ Tries.children trie

-- * Generic functions for working with Tries

-- | Get the 'children' of a 'Trie' as a list.
getChildrenAsList :: Trie -> [Trie]
getChildrenAsList EmptyTrie = []
getChildrenAsList trie      =
  -- Get the trie (value) from each element in the HashMap
  Prelude.map snd $
  -- Convert the children of type (HashMap key value) to a [(key, value)] list of tuples
  (HashMap.toList . Tries.children) trie

-- | Given a 'prefix' and a 'Trie', get the next node to search through.
-- e.g. if 'prefix' = "hell" and 'Trie' has 'prefix' of "hel" then this function
-- will return the node with 'prefix' "hell" (or the 'EmptyTrie' if it doesn't exist).
getNextNode :: String -> Trie -> Trie
getNextNode prefix trie =
  -- Lookup the next prefix in the trie.
  case HashMap.lookup [(head prefix)] (Tries.children trie) of
    -- If it isn't in there, then there is no next node. So return the EmptyTrie
    Nothing        -> EmptyTrie
    -- If it's there, return it
    Just some_node -> some_node

-- | Get the node of the specified prefix in the 'Trie'.
-- If not in the 'Trie', return 'EmptyTrie'.
getNode :: String -> Trie -> Trie
  -- If we ended up at an EmptyTrie, clearly we couldn't find prefix
getNode _ EmptyTrie = EmptyTrie
  -- If we're at the node being searched for, return the current node
getNode "" trie     = trie
  -- otherwise, recurse on the next node with tail of the prefix
getNode prefix trie = getNode (tail prefix)
                      (getNextNode prefix trie)

-- | Check to see if a 'prefix' is in a 'Trie'.
isInTrie :: String -> Trie -> Bool
isInTrie prefix trie
  | getNode prefix trie == EmptyTrie = False
  | otherwise                        = True


-- | Get the maximum 'weight' in a 'Trie'.
-- This includes the 'weight' of the current node and all 'children'.
getMaxWeight :: Trie -> Double
getMaxWeight EmptyTrie = (-1)
getMaxWeight trie      = max (Tries.weight trie)
                         (Tries.max_child_wt trie)

-- | Update the 'max_child_wt' of the current node (not of any 'children').
-- Warning: this function assumes that all children have accurate 'max_child_wt' values
-- This is used to update a 'Trie' whose 'max_child_wt' value becomes stale once
-- a child is deletmax_weighted (for example).
updateMaxChildWeight :: Trie -> Trie
updateMaxChildWeight EmptyTrie = EmptyTrie
updateMaxChildWeight trie      =
  trie { max_child_wt = updated_max_child_wt }
  -- This foldr is just finding the maximum weight in all
  -- of trie's children (and their max_child_wt's)
  where updated_max_child_wt = foldrChildren
          -- This function takes the maximum of a running_max,
          -- the child's weight, and the child's max_child_wt
          (\some_node running_max ->
              max running_max $ max
              (Tries.weight some_node)
              (Tries.max_child_wt some_node))
          -- Start with -1
          (-1) trie

-- ** Inserting nodes into Tries

-- | Insert a child 'Trie' into a parent trie and update the 'max_child_wt'
-- of the parent.
insertChild :: Trie -> Trie -> Trie
insertChild child EmptyTrie  = EmptyTrie
insertChild EmptyTrie parent = parent
-- if the above don't apply, add the child to the parent trie,
-- but use updateMaxChildWeight to update the stale trie
insertChild child parent     = updateMaxChildWeight
                               parent
                               {
                                 children = HashMap.insert
                                            (Tries.prefix child)
                                            (child)
                                            (Tries.children parent)
                               }

-- | Fast version of 'insertChild'.
-- This does NOT update max_child_wt for arbitrary insertions - this is only
-- for adding new children to tries that are otherwise not updated.
-- This will throw an error if the child is not valid (e.g. "the" is not a valid child of "t").
insertChildFast :: Trie -> Trie -> Trie
insertChildFast child parent =
  parent { max_child_wt = maximum [(Tries.max_child_wt parent),
                                   (Tries.weight child),
                                   (Tries.max_child_wt child)],
           children = HashMap.insert
                      (Tries.prefix child)
                      (child)
                      (Tries.children parent)
         }

-- | Insert a 'prefix' and its 'weight' into the 'Trie'.
insertPrefix :: Double -> String -> Trie -> Trie
insertPrefix weight prefix EmptyTrie = insertPrefix weight prefix rootNode
insertPrefix weight "" trie          = trie{ weight = weight }
insertPrefix weight prefix trie      = insertChildFast next_node trie
  -- if the above don't apply, insert the tail of the
  -- string into the appropriate child
  where next_node = case getNextNode prefix trie of
                      -- if the next node doesn't exist yet, create it
                      EmptyTrie -> insertPrefix weight (tail prefix)
                                   (PrefixNode
                                     [(head prefix)]
                                     (-1) (-1) HashMap.empty)
                      -- otherwise, insert the prefix into the existing node
                      some_node -> insertPrefix weight (tail prefix) some_node

-- | Insert weighted 'prefix'es into the 'Trie'.
-- This is essentially a wrapper that performs 'insertPrefix' many times.
insertWords :: [(Double, String)] -> Trie -> Trie
insertWords weighted_words trie =
  foldr insertWeightedWord trie weighted_words
  where insertWeightedWord =
          -- insertWeightedWord is just insertPrefix that takes (weight, word)
          -- as a single argument rather than weight and word as two arguments
          (\some_wtd_word some_trie -> insertPrefix (fst some_wtd_word)
                                       (snd some_wtd_word) some_trie)

-- | Build a 'Trie' from a list of weighted words.
trieFromList :: [(Double, String)] -> Trie
trieFromList weighted_words = insertWords weighted_words rootNode

-- ** Updating Tries

-- | Update the 'weight' of a node using a custom update function.
updateWeight :: (Double -> Double) -> String -> Trie -> Trie
updateWeight update_fn prefix trie
  -- If the trie is the EmptyTrie, throw an error
  | trie == EmptyTrie = error "Can't update weight of a non-existing node"
  -- If we've reached the end of the input string, update weight
  | null prefix       = trie{ weight = update_fn $ weight trie }
  -- Otherwise, update the weight of the next node
  | otherwise         = insertChild next_node trie
  where next_node = updateWeight update_fn (tail prefix)
                    (getNextNode prefix trie)

-- | Update the 'weight' of a node using a custom update function if the node exists.
-- If it doesn't exist, create it with 0 'weight'.
insertOrUpdate :: (Double -> Double) -> String -> Trie -> Trie
insertOrUpdate _ word EmptyTrie    = insertPrefix 0 word rootNode
insertOrUpdate update_fn "" trie   = trie{ weight = update_fn $ weight trie }
insertOrUpdate update_fn word trie =
  case getNextNode word trie of
    -- If the next_node is the EmptyTrie, insert the word
    EmptyTrie -> insertPrefix 0 word trie
    -- Otherwise, insert or update the weight of the next node
    next_node -> insertChild node_to_insert trie
      where
        node_to_insert = insertOrUpdate update_fn (tail word)
                         next_node

-- | Rescale the 'weight's in the 'Trie' by an arbitrary update function.
rescaleWeights :: (Double -> Double) -> Trie -> Trie
rescaleWeights rs_fn EmptyTrie = EmptyTrie
rescaleWeights rs_fn trie      = -- run rescaleWeights on all children
                                 mapChildren
                                 (\some_child -> rescaleWeights rs_fn some_child)
                                 -- give the trie its rescaled weight
                                 trie{ weight = rescaled_weight }
  where rescaled_weight = case Tries.weight trie of
                            -- if it's a non-word, keep weight it at -1
                            (-1)        -> (-1)
                            -- if it's a word, apply rs_fn to its weight
                            word_weight -> rs_fn word_weight

-- ** Removing nodes from Tries

-- | Remove the child (specified as a 'prefix') from the parent.
-- This is primarily a helper function for 'removePrefix'.
removeChild :: String -> Trie -> Trie
removeChild child_prefix parent =
  -- delete a child, and then use updateMaxChildWeight to update the stale trie
  updateMaxChildWeight parent {children = children_without_prefix}
  where
    children_without_prefix =
      case HashMap.lookup child_prefix (Tries.children parent) of
        -- If the there is no child with the specified prefix, do nothing
        Nothing         -> Tries.children parent
        -- Otherwise,
        Just child_trie ->
          -- If the child has no children, just delete the node completely
          if children child_trie == HashMap.empty then
            HashMap.delete
            child_prefix
            (Tries.children parent)
          -- If the child has some children, set its weight to -1
          -- (to preserve the children)
          else
            HashMap.insert
            child_prefix
            child_trie{ weight = -1 }
            (Tries.children parent)

-- | Remove a 'prefix' from the 'Trie'.
removePrefix :: String -> Trie -> Trie
removePrefix _ EmptyTrie = EmptyTrie
removePrefix prefix trie =
  -- If the node we want to delete is a direct child,
  if HashMap.lookup prefix (Tries.children trie) /= Nothing then
    -- Return the current node, but with prefix deleted from children,
    -- and max_child_wt updated
    removeChild prefix trie
  else
    case getNextNode prefix trie of
      -- If there's no next node, the prefix we searched for cannot be in the trie
      EmptyTrie -> trie
      -- On the other hand, if it exists, then return trie but recursively run
      -- removePrefix on the child, next_node
      next_node -> pruneTrie 0 $
        insertChild (removePrefix (tail prefix) next_node) trie

-- | Run 'pruneTrie' on children whose maximum weight (max(weight, max child weight))
-- is greater than or equal to `threshold`.
-- This is effectively just a helper function for 'pruneTrie'.
pruneChildren :: Double -> Trie -> Trie
pruneChildren threshold trie =
  -- run pruneTrie on all the filtered children
  mapChildren
  (\some_child -> pruneTrie threshold some_child) $
  -- Filter out all children whose weight and max_child_wt
  -- are below the threshold
  filterChildren
  (\some_child -> getMaxWeight some_child >= threshold)
  trie

-- | Remove all nodes in a 'Trie' whose weights are below a threshold.
pruneTrie :: Double -> Trie -> Trie
pruneTrie _ EmptyTrie    = EmptyTrie
pruneTrie threshold trie = pruneChildren threshold $
                           trie
                           {
                             weight = updated_weight
                           }
  where
    updated_weight = if Tries.weight trie < threshold then
                       -- if the weight is less than the threshold,
                       -- then set it to (-1), effectively deleting it
                       (-1)
                     else
                       -- otherwise, keep it as is.
                       Tries.weight trie

-- * Creating Tries from text files

-- | Convert a correctly formed input line to a weighted word.
textToWtdWord :: Text.Text -> (Double, String)
textToWtdWord line =
  case Text.splitOn "\t" line of
    -- If this is in fact a list of length two,
    [wt_text, word] -> case TextRead.double $ Text.strip wt_text of
                         -- If the stripped text preceding "\t" is a double,
                         Right (wt, "") -> (wt, Text.unpack word)
                         -- Otherwise, the weight is malformed
                         _              -> error $
                                           "Malformed weighted word: " ++ Text.unpack line
    -- Otherwise, the whole row is malformed
    _               -> error $ "Malformed weighted word: " ++ Text.unpack line

-- | Read terms from a specified 'FilePath' and produce a 'Trie'.
readTerms :: FilePath -> IO Trie
readTerms fp = do
  -- Read in the text file as a string
  trie_text <- TextIO.readFile fp
  let weighted_words =
        -- Remove the first and last entires (first is a count, last is "")
        (init . tail) $
        -- Get a weighted word from each line in trie_text
        map textToWtdWord $
        -- Get all the lines of trie_text in a list
        Text.splitOn "\n" trie_text
  -- Build the trie with the weighted_words
  return $ insertWords weighted_words rootNode

-- * Autocomplete

-- | Give a priority for 'autocomplete'.
-- This is mainly because 'MinMaxQueue' is a min-priority queue, so we need to flip
-- the priority by negating the 'weight' of the word.
autocompletePriority :: (Double, String) -> Double
autocompletePriority x = -(fst x)

-- | Get the minimum weight in the minmaxqueue
-- The smallest weight is actually the "maximum" in the MMQ since
-- the priorities are the negative weights
mmqMinWeight :: (MMQ.MinMaxQueue Double (Double, String)) -> Double
mmqMinWeight q = case MMQ.peekMax q of
                   -- if the q is empty, largest weight is -1 by convention
                   Nothing        -> (-1)
                   -- otherwise, get the weight of the "min" of q
                   Just some_item -> fst $ some_item

-- | Insert the weighted word (the tuple (`weight`, `word`)) into `q`
mmqInsert :: Double -> String ->
             (MMQ.MinMaxQueue Double (Double, String)) ->
             (MMQ.MinMaxQueue Double (Double, String))
mmqInsert (-1) word q   = q
mmqInsert weight word q = MMQ.insert autocompletePriority
                          (weight, word) q

-- | Get all matching terms for a given 'prefix'.
-- This does the majority of the work for 'autocomplete'.
getMatchingPrefixes :: String -> String -> Int -> Trie ->
                       (MMQ.MinMaxQueue Double (Double, String)) ->
                       (MMQ.MinMaxQueue Double (Double, String))
getMatchingPrefixes prefix word_so_far k trie q
  | trie == EmptyTrie       = q
  -- If we still need to search for the node whose value represents prefix
  | not (null prefix)       = getMatchingPrefixes (tail prefix)
                              (word_so_far ++ [head prefix])
                              k (getNextNode prefix trie) q
  -- If the q is maxed out AND there are no children with larger weight
  -- than the minimum in the queue, then just ignore them.
  | MMQ.size q == k &&
    Tries.max_child_wt trie
    <=  mmqMinWeight q      = new_q
  -- Otherwise, add current node (if applicable) and child matches to the existing q
  | otherwise               = foldrChildren
                              getChildMatches
                              new_q trie
  where
    new_q = mmqInsert (Tries.weight trie) word_so_far q
    -- this function essentially runs getMatchingPrefixes on child_trie
    -- with an updated word_so_far and some updated queue
    getChildMatches child_trie updated_q =
      getMatchingPrefixes ""
      (word_so_far ++ Tries.prefix child_trie)
      k child_trie updated_q

-- | Produce the top k words in a given 'Trie' that match a
-- particular 'prefix', sorted by weight.
autocomplete :: String -> Trie -> Int -> [(Double, String)]
autocomplete prefix trie k =
  -- Get the elements from the priority queue
  MMQ.elems $
  -- get all matching prefixes, and start the word_so_far to ""
  getMatchingPrefixes
  prefix "" k trie $
  -- Initialize with an empty queue with a maximum size
  MMQ.withMaxSize MMQ.empty k

-- | Produce the top k words in a given list of weighted words that match a
-- particular query, sorted by weight.
slowComplete :: String -> [(Double, String)] -> Int -> [(Double, String)]
slowComplete query weighted_words k =
  -- Get the k (weight, word) tuples with the highest weight
  Heap.take k
-- Create a Maximum Priority Heap, prioritized by the weight of each word
  (Heap.fromList
  -- Filter out all of the elements of weighted_words whose value doesn't start with "query"
   (filter isQueryPrefixOfWord weighted_words) :: Heap.MaxPrioHeap Double String) where
  isQueryPrefixOfWord = (\x -> query `isPrefixOf`snd x)


