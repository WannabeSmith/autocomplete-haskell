#ifndef TRIES_H
#define TRIES_H

#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
  ///
  /// @brief Initialize the interface with the Haskell-based `Trie`s.
  ///
  /// This *must* be called before using any of the other functions
  /// provided by this library. When you're done using them, call
  /// 'tries_end'.
  /// @code
  /// tries_init();
  /// // Do things with tries
  /// tries_end();
  /// @endcode
  ///
  HsBool tries_init(void);
  ///
  /// @brief End the interface with Haskell-based Tries.
  ///
  /// After calling
  /// this function, there's no guarantee that any of the other
  /// functions in this library will work.
  ///
  /// Example:
  /// @code
  /// tries_init();
  /// // Do things with tries
  /// tries_end();
  /// @endcode
  ///
  void tries_end(void);
  ///
  /// @brief Print the string representation of the trie.
  ///
  /// In `Tries.hs`, there's a custom `show` function defined to return
  /// a string that represents a trie. `print_trie` simply prints that string.
  ///
  /// Example:
  /// @code
  /// print_trie(get_empty_trie());
  /// @endcode
  ///
  void print_trie(HsStablePtr trie);
  ///
  /// @brief Get an `EmptyTrie`.
  ///
  /// Get a stable pointer to the `EmptyTrie` as defined in `Tries.hs`.
  ///
  /// Example:
  /// @code
  /// HsStablePtr empty_trie = get_empty_trie();
  /// @endcode
  ///
  /// @return stable pointer to a `Tries.hs` `EmptyTrie`
  ///
  HsStablePtr get_empty_trie(void);
  ///
  /// @brief Get a root node.
  ///
  /// Get a stable pointer to a root node as defined in `Tries.hs`.
  ///
  /// Example:
  /// @code
  /// HsStablePtr root_node = get_root_node();
  /// @endcode
  ///
  /// @return stable pointer to a `Tries.hs` root node
  ///
  HsStablePtr get_root_node(void);
  ///
  /// @brief Get the next node for searching.
  ///
  /// Given a prefix and a stable pointer to a trie, get a stable pointer
  /// to the next node on the path to the sub-trie for the given prefix.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// HsStablePtr mynode = get_next_node("ted", mytrie);
  /// @endcode
  ///
  /// @param prefix The prefix to be searched for
  /// @param trie The trie to search
  ///
  /// @return stable pointer to the next node
  ///
  HsStablePtr get_next_node(char* prefix, HsStablePtr trie);
  ///
  /// @brief Get a node by name.
  ///
  /// Given a prefix and a stable pointer to a trie, get a stable pointer
  /// to the subtrie for that prefix if it exists. Otherwise, get a stable pointer
  /// to the EmptyTrie.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// HsStablePtr mynode = get_node("te", mytrie);
  /// @endcode
  ///
  /// @param prefix The prefix to be searched for
  /// @param trie The trie to search
  ///
  /// @return stable pointer to the node
  ///
  HsStablePtr get_node(char* prefix, HsStablePtr trie);
  ///
  /// @brief Is the prefix in the trie?
  ///
  /// Return an integer indicating whether or not `prefix` is in `trie`.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// int ted_in_trie = is_in_trie("ted", mytrie); // Should be 1
  /// int tedtalk_in_trie = is_in_trie("tedtalk", mytrie); // Should be 0
  /// @endcode
  ///
  /// @param prefix The prefix to be searched for
  /// @param trie The trie to search
  ///
  /// @return 1 if in trie, 0 otherwise.
  ///
  int is_in_trie(char* prefix, HsStablePtr trie);
  ///
  /// @brief Maximum weight of nodes in a trie.
  ///
  /// Get the largest weight out of all nodes in a trie. This includes the top
  /// node and all of its children, its children's children, etc.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// double max_weight = get_max_weight(mytrie); // Should be 30
  /// @endcode
  ///
  /// @param trie A stable pointer to the trie
  ///
  /// @return A double with the maximum weight in the trie
  ///
  double get_max_weight(HsStablePtr trie);
  ///
  /// @brief Insert a prefix into a trie.
  ///
  /// Insert `word` with a weight of `weight` into the trie that `trie` points to.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// // This trie will now include "in" with a weight of 5.
  /// mytrie_augmented = insert_prefix(5, "in", mytrie);
  /// @endcode
  ///
  /// @param weight A double with the weight that we wish to assign to `word`
  /// @param word A string with the word that we'd like to insert
  /// @param trie A stable pointer to the trie into which we'd like to insert `word`
  ///
  /// @return A stable pointer to the trie with the inserted prefix.
  ///
  HsStablePtr insert_prefix(double weight, char* word, HsStablePtr trie);
  ///
  /// @brief Insert multiple words into a trie.
  ///
  /// Insert the words in the array, `words` and their corresponding weights
  /// in `weights` into `trie`. Also specify the length of the two arrays
  /// as `num_words` so that memory can be appropriately allocated.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// double new_weights[2] = {5, 15};
  /// char* new_words[2] = {"in", "inn"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// // This trie will now include "in" and "inn" with weights
  /// // of 5 and 15, respectively
  /// HsStablePtr mytrie_augmented = insert_words(new_weights, new_words,
  ///                                                          2, mytrie);
  /// @endcode
  ///
  /// @param weights An array of doubles with weights for the words
  /// @param word A string with the words that we'd like to insert
  /// @param trie A stable pointer to the trie into which we'll insert `words`
  ///
  /// @return A stable pointer to the trie with the inserted words.
  ///
  HsStablePtr insert_words(double weights[], char** words,
                           int num_words, HsStablePtr trie);
  ///
  /// @brief Build a trie from arrays of words and weights.
  ///
  /// Given an array of weights (as doubles) and an array of words (as strings),
  /// create a trie with those words and their corresponding weights.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// double new_weights[2] = {5, 15};
  /// char* new_words[2] = {"in", "inn"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// @endcode
  ///
  /// @param weights An array of doubles with weights for the words
  /// @param word A string with the words that we'd like to insert
  /// @param trie A stable pointer to the trie into which we'll insert `words`
  ///
  /// @return A stable pointer to the trie with the inserted words.
  ///
  HsStablePtr trie_from_arrays(double weights[], char** words,
                               int num_words);
  ///
  /// @brief Remove a prefix from a trie.
  ///
  /// Remove the node corresponding to `prefix` from `trie` if it's in the trie.
  /// If it's not in the trie, return the trie unchanged.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// double new_weights[2] = {5, 15};
  /// char* new_words[2] = {"in", "inn"};
  /// HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  /// // This modified trie will have "a" removed.
  /// mytrie_modified = remove_prefix("a", mytrie);
  /// @endcode
  ///
  /// @param prefix The prefix to be deleted from `trie`
  /// @param trie A stable pointer to the trie from which we'll delete `prefix`
  ///
  /// @return A stable pointer to the trie with the node removed.
  ///
  HsStablePtr remove_prefix(char* prefix, HsStablePtr trie);
  ///
  /// @brief Build a trie from a file
  ///
  /// Get a stable pointer to a trie built from the text file specified by
  /// `file_path`.
  ///
  /// Example:
  /// @code
  /// HsStablePtr mytrie4 = read_terms("../Data/mytrie4.txt");
  /// @endcode
  ///
  /// @param file_path A string representing the file path to the trie
  ///
  /// @return a stable pointer to the built trie
  ///
  HsStablePtr read_terms(char* file_path);
  ///
  /// @brief Perform autocomplete using tries
  ///
  /// Given a trie (built using one of the functions provided in this library),
  /// perform autocomplete for a given prefix and number of desired matching
  /// phrases.
  ///
  /// Example:
  /// @code
  /// HsStablePtr mytrie4 = read_terms("../Data/mytrie4.txt");
  /// double weights_out[3];
  /// char* words_out[3];
  /// // This doesn't return anything, but `weights_out` and `words_out` will include
  /// // the results from autocomplete (weights and words, respectively)
  /// autocomplete(weights_out, words_out, "t", mytrie4, 3);
  /// @endcode
  ///
  /// @param weights_out The array that will contain the resulting weights from autocomplete, sorted by weight
  /// @param words_out The array that will contain the resulting words from autocomplete, sorted by their respective weights
  /// @param word The prefix to be "autocompleted"
  /// @param trie Stable pointer to the trie containing the candidate words and weights for autocomplete
  /// @param k The number of top words/weights to get from autocomplete
  ///
  void autocomplete(double* weights_out, char** words_out,
                    char* word, HsStablePtr trie, int k);
  ///
  /// @brief Perform slowcomplete
  ///
  /// Given an array of input weights and input words, perform the task of
  /// autocomplete using the naive method (i.e. slowcomplete) for a given
  /// prefix and number of desired matching phrases.
  ///
  /// Example:
  /// @code
  /// double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  /// char* words[5] = {"an", "to", "ted", "a", "tea"};
  /// double weights_out[3];
  /// char* words_out[3];
  /// // This doesn't return anything, but `weights_out` and `words_out` will include
  /// // the results from slowcomplete (weights and words, respectively)
  /// slowcomplete(weights_out, words_out, 5, "t", weights, words, 3);
  /// @endcode
  ///
  /// @param weights_out The array that will contain the resulting weights from slowcomplete, sorted by weight
  /// @param words_out The array that will contain the resulting words from slowcomplete, sorted by their respective weights
  /// @param num_words The number of candidate words (i.e. the length of `words_in`)
  /// @param prefix The prefix to be "slowcompleted"
  /// @param weights_in An array of candidate weights
  /// @param words_in An array of candidate words
  /// @param k The number of top words/weights to get from slowcomplete
  ///
  void slowcomplete(double* weights_out, char** words_out,
                    int num_words, char* prefix,
                    double* weights_in, char** words_in, int k);
#ifdef __cplusplus
}
#endif
#endif

