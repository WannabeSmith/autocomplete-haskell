{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : CTries
Description : Foreign function interface of Tries built for C
Copyright   : (c) Ian Waudby-Smith, 2019
License     : BSD3
Maintainer  : iwaudbysmith@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the foreign function interface of Tries.hs so that they can be called by C.
-}


module CTries where

import Tries
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Control.Monad

showC :: (StablePtr Trie) -> IO CString
showC trie_ptr = do
  trie     <- deRefStablePtr trie_ptr
  newCString $ show trie

foreign export ccall showC :: (StablePtr Trie) -> IO CString

emptyTrieC :: IO (StablePtr Trie)
emptyTrieC = newStablePtr EmptyTrie

foreign export ccall emptyTrieC :: IO (StablePtr Trie)

rootNodeC :: IO (StablePtr Trie)
rootNodeC = newStablePtr rootNode

foreign export ccall rootNodeC :: IO (StablePtr Trie)

getNextNodeC :: CString -> (StablePtr Trie) -> IO (StablePtr Trie)
getNextNodeC cprefix trie_ptr = do
  prefix <- peekCString cprefix
  trie   <- deRefStablePtr trie_ptr
  newStablePtr $ getNextNode prefix trie

foreign export ccall getNextNodeC :: CString ->
                                     (StablePtr Trie) ->
                                     IO (StablePtr Trie)

getNodeC :: CString -> (StablePtr Trie) -> IO (StablePtr Trie)
getNodeC cprefix trie_ptr = do
  prefix   <- peekCString cprefix
  trie     <- deRefStablePtr trie_ptr
  node_ptr <- newStablePtr $
              getNode prefix trie
  return node_ptr

foreign export ccall getNodeC :: CString ->
                                 (StablePtr Trie) ->
                                 IO (StablePtr Trie)

isInTrieC :: CString -> (StablePtr Trie) -> IO CInt
isInTrieC cprefix trie_ptr = do
  prefix <- peekCString cprefix
  trie   <- deRefStablePtr trie_ptr
  let result = fromBool $ isInTrie prefix trie
  return result

foreign export ccall isInTrieC :: CString ->
                                  (StablePtr Trie) ->
                                  IO CInt

getMaxWeightC :: (StablePtr Trie) -> IO CDouble
getMaxWeightC trie_ptr = do
  trie <- deRefStablePtr trie_ptr
  let max_weight = getMaxWeight trie
  return (CDouble max_weight)

foreign export ccall getMaxWeightC :: (StablePtr Trie) -> IO CDouble


updateMaxChildWeightC :: (StablePtr Trie) -> IO (StablePtr Trie)
updateMaxChildWeightC trie_ptr = do
  trie <- deRefStablePtr trie_ptr
  newStablePtr $ updateMaxChildWeight trie

foreign export ccall updateMaxChildWeightC :: (StablePtr Trie) -> IO (StablePtr Trie)


insertChildC :: (StablePtr Trie) -> (StablePtr Trie) -> IO (StablePtr Trie)
insertChildC child_ptr parent_ptr = do
  child  <- deRefStablePtr child_ptr
  parent <- deRefStablePtr parent_ptr
  newStablePtr $ insertChild child parent

foreign export ccall insertChildC :: (StablePtr Trie) ->
                                     (StablePtr Trie) ->
                                     IO (StablePtr Trie)


insertPrefixC :: CDouble -> CString -> (StablePtr Trie) -> IO (StablePtr Trie)
insertPrefixC cweight cprefix trie_ptr = do
  let weight = realToFrac cweight
  prefix <- peekCString cprefix
  trie   <- deRefStablePtr trie_ptr
  newStablePtr $ insertPrefix weight prefix trie

foreign export ccall insertPrefixC :: CDouble -> CString ->
                                      (StablePtr Trie) ->
                                      IO (StablePtr Trie)


insertWordsC :: (Ptr CDouble) -> (Ptr CString) -> CInt ->
                (StablePtr Trie) -> IO (StablePtr Trie)
insertWordsC cweight_array cword_array cnum_words trie_ptr = do
  -- get the number of words as an Int
  let num_words = fromIntegral cnum_words
  -- get the weight list, but the weights are still CDoubles
  cdouble_wtl <- peekArray num_words cweight_array
  -- convert all CDoubles to Doubles
  let weight_list = map realToFrac cdouble_wtl
  -- get the word list but all words are CStrings
  cstring_wdl <- peekArray num_words cword_array
  -- Since `map peekCString cstring_wl` has the type `[IO String]`, we
  -- use `sequence` to get something of the type `IO [String]` and then use
  -- the `<-` operator to get something of type [String]
  word_list  <- sequence $ map peekCString cstring_wdl
  -- get the trie from its C pointer
  trie       <- deRefStablePtr trie_ptr
  -- zip weights and words into something of type `(Double, String)`
  let weighted_words = zip weight_list word_list
  -- apply insertWords and then put inside a stable pointer
  newStablePtr $ insertWords weighted_words trie

foreign export ccall insertWordsC :: (Ptr CDouble) -> (Ptr CString) -> CInt ->
                                     (StablePtr Trie) -> IO (StablePtr Trie)

trieFromListC :: (Ptr CDouble) -> (Ptr CString) ->
                 CInt -> IO (StablePtr Trie)
trieFromListC cweight_array cword_array cnum_words = do
  croot_node <- rootNodeC
  insertWordsC cweight_array cword_array cnum_words croot_node

foreign export ccall trieFromListC :: (Ptr CDouble) -> (Ptr CString) ->
                                      CInt -> IO (StablePtr Trie)


removePrefixC :: CString -> (StablePtr Trie) -> IO (StablePtr Trie)
removePrefixC cprefix trie_ptr = do
  prefix <- peekCString cprefix
  trie   <- deRefStablePtr trie_ptr
  newStablePtr $ removePrefix prefix trie

foreign export ccall removePrefixC :: CString -> (StablePtr Trie) ->
                                      IO (StablePtr Trie)


pruneTrieC :: CDouble -> (StablePtr Trie) -> IO (StablePtr Trie)
pruneTrieC cthresh trie_ptr = do
  let threshold = realToFrac cthresh
  trie <- deRefStablePtr trie_ptr
  newStablePtr $ pruneTrie threshold trie

foreign export ccall pruneTrieC :: CDouble -> (StablePtr Trie) -> IO (StablePtr Trie)

readTermsC :: CString -> IO (StablePtr Trie)
readTermsC cfp = do
  fp   <- peekCString cfp
  trie <- readTerms fp
  newStablePtr trie

foreign export ccall readTermsC :: CString -> IO (StablePtr Trie)

autocompleteByReferenceC :: (Ptr CDouble) -> (Ptr CString) -> CString ->
                            (StablePtr Trie) -> CInt -> IO ()
autocompleteByReferenceC cweight_array cword_array cprefix trie_ptr ck = do
  prefix <- peekCString cprefix
  trie   <- deRefStablePtr trie_ptr
  let k = fromIntegral ck
  -- get a tuple of lists (rather than list of tuples)
  let results_tuple = unzip $ autocomplete prefix trie k
  cword_array' <- sequence $ map newCString $ snd results_tuple
  -- Write the arrays to the pointers provided
  pokeArray cweight_array (map CDouble $ fst results_tuple)
  pokeArray cword_array cword_array'

foreign export ccall autocompleteByReferenceC :: (Ptr CDouble) ->
                                                 (Ptr CString) ->
                                                 CString ->
                                                 (StablePtr Trie) -> CInt -> IO ()


slowCompleteByReferenceC :: (Ptr CDouble) -> (Ptr CString) ->
                            CInt -> CString -> (Ptr CDouble) ->
                            (Ptr CString) -> CInt -> IO ()
slowCompleteByReferenceC cwt_arr_out cwd_arr_out cnum_words cprefix
  cwt_arr_in cwd_arr_in ck = do
  let num_words = fromIntegral cnum_words
  let k = fromIntegral ck
  -- get the weight list, but the weights are still CDoubles
  cdouble_wtl    <- peekArray num_words cwt_arr_in
  -- convert all CDoubles to Doubles
  let weight_list = map realToFrac cdouble_wtl
  -- get the word list but all words are CStrings
  cstring_wdl    <- peekArray num_words cwd_arr_in
  -- Since `map peekCString cstring_wl` has the type `[IO String]`, we
  -- use `sequence` to get something of the type `IO [String]` and then use
  -- the `<-` operator to get something of type [String]
  word_list      <- sequence $ map peekCString cstring_wdl
  let weighted_words = zip weight_list word_list
  prefix         <- peekCString cprefix
  -- create tuple of weights and words
  let results_tuple = unzip $ slowComplete prefix weighted_words k
  let cwt_arr_out' = map CDouble $ fst results_tuple
  -- again, get something of type `IO [String]` from something of type `[IO String]`
  cwd_arr_out'   <- sequence $ map newCString $ snd results_tuple
  -- write the arrays to the pointers provided
  pokeArray cwt_arr_out cwt_arr_out'
  pokeArray cwd_arr_out cwd_arr_out'

foreign export ccall slowCompleteByReferenceC :: (Ptr CDouble) -> (Ptr CString) ->
                                                 CInt -> CString -> (Ptr CDouble) ->
                                                 (Ptr CString) -> CInt -> IO ()
