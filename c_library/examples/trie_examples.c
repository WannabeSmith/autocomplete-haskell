#include <stdio.h>
#include <string.h>
#include "tries.h"

int main(void) {
  tries_init();

  // create an EmptyTrie
  HsStablePtr empty_trie = get_empty_trie();
  printf("get_empty_trie():\n");
  print_trie(empty_trie);

  // create a more interesting trie
  double weights[5] = {30.0, 12.0, 25.0, 20.0, 14.0};
  char* words[5] = {"an", "to", "ted", "a", "tea"};
  HsStablePtr mytrie = trie_from_arrays(weights, words, 5);
  printf("mytrie:\n");
  print_trie(mytrie);

  // get the next node when searching for "ted"
  printf("get_next_node(\"ted\", mytrie):\n");
  print_trie(get_next_node("ted", mytrie));

  // get the node for "te"
  printf("get_node(\"te\", mytrie):\n");
  print_trie(get_node("te", mytrie));

  // is "ted" in the trie?
  printf("is_in_trie(\"ted\", mytrie): %d\n", is_in_trie("ted", mytrie));

  // is "tedtalk" in the trie?
  printf("is_in_trie(\"tedtalk\", mytrie): %d\n",
         is_in_trie("tedtalk", mytrie));

  // what's the maximum weight in this trie?
  printf("get_max_weight(mytrie): %f\n", get_max_weight(mytrie));

  // insert some nodes
  double new_weights[2] = {5, 15};
  char* new_words[2] = {"in", "inn"};
  HsStablePtr mytrie_augmented = insert_words(new_weights, new_words,
                                              2, mytrie);
  printf("\nmytrie_augmented:\n");
  print_trie(mytrie_augmented);

  // remove a prefix
  mytrie_augmented = remove_prefix("in", mytrie_augmented);
  printf("\nmytrie_augmented after deleting \"in\"\n");
  print_trie(mytrie_augmented);

  // read in a trie
  HsStablePtr mytrie4 = read_terms("Data/mytrie4.txt");
  printf("mytrie4 using read_terms:\n");
  print_trie(mytrie4);

  // use autocomplete
  double weights_out[3];
  char* words_out[3];
  autocomplete(weights_out, words_out, "t", mytrie4, 3);
  printf("autocomplete results:\n");

  for (int i = 0; i < 3; i++){
    printf("(%f, %s)\n", weights_out[i], words_out[i]);
  }

  // Request the same autocomplete results but use slowcomplete
  slowcomplete(weights_out, words_out, 5, "t", weights, words, 3);

  printf("\nslowcomplete results:\n");
  for (int i = 0; i < 3; i++){
    printf("(%f, %s)\n", weights_out[i], words_out[i]);
  }

  tries_end();
  return 0;
}
