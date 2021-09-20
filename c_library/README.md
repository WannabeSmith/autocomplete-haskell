# C interface to autocomplete-haskell

## Running examples from the command line

The script [run_examples.sh](run_examples.sh) compiles the Haskell library, organizes to-be-exported FFI functions in a dependencies folder, and compiles the examples file `examples/trie_examples.c`: 

```zsh
sh run_examples.sh
```

### `c_library/examples/trie_examples.c`

```c
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
  HsStablePtr mytrie4 = read_terms("../Data/mytrie4.txt");
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
```

### Expected output from the above C program:

```zsh
get_empty_trie():
EmptyTrie

mytrie:
a 20.0 30.0
   n 30.0 -1.0
t -1.0 25.0
   o 12.0 -1.0
   e -1.0 25.0
      a 14.0 -1.0
      d 25.0 -1.0


get_next_node("ted", mytrie):
t -1.0 25.0
   o 12.0 -1.0
   e -1.0 25.0
      a 14.0 -1.0
      d 25.0 -1.0


get_node("te", mytrie):
e -1.0 25.0
   a 14.0 -1.0
   d 25.0 -1.0


is_in_trie("ted", mytrie): 1
is_in_trie("tedtalk", mytrie): 0
get_max_weight(mytrie): 30.000000

mytrie_augmented:
a 20.0 30.0
   n 30.0 -1.0
t -1.0 25.0
   o 12.0 -1.0
   e -1.0 25.0
      a 14.0 -1.0
      d 25.0 -1.0
i -1.0 15.0
   n 5.0 15.0
      n 15.0 -1.0



mytrie_augmented after deleting "in"
a 20.0 30.0
   n 30.0 -1.0
t -1.0 25.0
   o 12.0 -1.0
   e -1.0 25.0
      a 14.0 -1.0
      d 25.0 -1.0
i -1.0 15.0
   n -1.0 15.0
      n 15.0 -1.0


mytrie4 using read_terms:
a 20.0 30.0
   n 30.0 -1.0
t -1.0 25.0
   o 12.0 -1.0
   e -1.0 25.0
      a 14.0 -1.0
      d 25.0 -1.0


autocomplete results:
(25.000000, ted)
(14.000000, tea)
(12.000000, to)

slowcomplete results:
(25.000000, ted)
(14.000000, tea)
(12.000000, to)

```

## Unit tests

The script [run_tests.sh](run_tests.sh) compiles the Haskell library, organizes to-be-exported FFI functions in a dependencies folder, and compiles the unit test file `test/test_tries.c`: 

```zsh
sh run_tests.sh
```

## Documentation (using doxygen)

Documentation for the C interface can be generated using [doxygen](https://www.doxygen.nl/index.html). First, you will need to have doxygen installed. This can likely be found using your OS package manager. For example, using homebrew,

```zsh
brew install doxygen
```

Now, simply run doxygen on [doxygen_config](doxygen_config):

```zsh
doxygen doxygen_config
```

Finally, open the generated `html/files.html` to see documentation for [src/tries.c](src/tries.c) and [src/tries.h](src/tries.h). This can be done on macOS using the `open` command:

```zsh
open html/files.html
```
