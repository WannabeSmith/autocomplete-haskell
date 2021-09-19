#include <stdio.h>
#include <string.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "CTries_stub.h"
#endif

///
/// This simply calls `hs_init` with the appropriate parameters.
///
HsBool tries_init(void){
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  return HS_BOOL_TRUE;
}

///
/// This simply calls `hs_exit`.
///
void tries_end(void){
  hs_exit();
}

///
/// A wrapper that calls `showC` which is a C wrapper around the `show` function
/// and prints the resulting string using `printf`.
void print_trie(HsStablePtr trie)
{
  printf("%s\n\n", showC(trie));
}

///
/// A wrapper that calls `emptyTrieC` to get a stable pointer to an `EmptyTrie`.
///
HsStablePtr get_empty_trie(void)
{
  return emptyTrieC();
}

///
/// A wrapper that calls `rootNodeC` to get a stable pointer to a root node.
///
HsStablePtr get_root_node(void)
{
  return rootNodeC();
}

///
/// A wrapper that calls `getNextNodeC` with a string and a stable pointer and
/// returns a stable pointer to the resulting `Trie`
///
HsStablePtr get_next_node(char* prefix, HsStablePtr trie)
{
  return getNextNodeC(prefix, trie);
}

///
/// A wrapper that calls `getNodeC` with a string and a stable pointer to a `Trie`
/// and returns a stable pointer to the resulting `Trie`.
///
HsStablePtr get_node(char* prefix, HsStablePtr trie)
{
  return getNodeC(prefix, trie);
}

///
/// A wrapper that calls `isInTrieC` with a string and a stable pointer to a `Trie`
/// and returns an integer which is 1 if True and 0 if False.
///
int is_in_trie(char* prefix, HsStablePtr trie)
{
  return isInTrieC(prefix, trie);
}

///
/// A wrapper that calls `getMaxWeightC` with a stable pointer to a `Trie` and
/// return the maximum weight in that `Trie` as a `double`.
///
double get_max_weight(HsStablePtr trie)
{
  return getMaxWeightC(trie);
}

///
/// A wrapper that calls `insertPrefixC` with a double, a string, and a stable
/// pointer to a `Trie` and returns a stable pointer to the resulting
/// `Trie`.
///
HsStablePtr insert_prefix(double weight, char* word, HsStablePtr trie)
{
  return insertPrefixC(weight, word, trie);
}

///
/// A wrapper that calls `insertWordsC` with an array of doubles, an array of
/// strings, an integer, and a stable pointer to a `Trie`, and returns a stable
/// pointer to the constructed `Trie`.
///
HsStablePtr insert_words(double weights[], char** words,
                         int num_words, HsStablePtr trie)
{
  return insertWordsC(weights, words, num_words, trie);
}

///
/// A wrapper that calls `trieFromListC` with an array of doubles, an array of
/// strings, an integer, and returns a stable pointer to the constructed `Trie`.
///
HsStablePtr trie_from_arrays(double weights[], char** words,
                             int num_words)
{
  return trieFromListC(weights, words, num_words);
}

///
/// A wrapper that calls `removePrefixC` with a string and a stable pointer to a
/// `Trie` and returns a stable pointer to the `Trie` with the given node removed.
///
HsStablePtr remove_prefix(char* prefix, HsStablePtr trie)
{
  return removePrefixC(prefix, trie);
}

///
/// A wrapper that calls `readTermsC` with a string and returns a stable pointer to the
/// `Trie` built from the specified file.
///
HsStablePtr read_terms(char* file_path)
{
  return readTermsC(file_path);
}

///
/// A wrapper that calls `autocompleteByReferenceC` with an array of doubles
/// to be modified, an array of strings to be modified, a string, a stable pointer
/// to a `Trie`, and an integer. This function modifies the arguments `weights_out`
/// and `words_out` by reference to include the results of `autocomplete`.
///
void autocomplete(double* weights_out, char** words_out,
                  char* word, HsStablePtr trie, int k)
{
  autocompleteByReferenceC(weights_out, words_out, word, trie, k);
}

///
/// A wrapper that calls `slowcompleteByReferenceC` with an array of doubles
/// to be modified, an array of strings to be modified, an integer, a string, an
/// array of doubles, an array of strings, and an integer.
/// This function modifies the arguments `weights_out` and `words_out` by
/// reference to include the results of `slowcomplete`.
///
void slowcomplete(double* weights_out, char** words_out,
                  int num_words, char* prefix, double* weights_in,
                  char** words_in, int k)
{
  slowCompleteByReferenceC(weights_out, words_out, num_words,
                           prefix, weights_in, words_in, k);
}
