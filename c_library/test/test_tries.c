#include <stdio.h>
#include <string.h>
#include "tries.h"
#include "Unity/src/unity.h"

void setUp(void) {
}

void tearDown(void) {
}

void test_is_in_trie(void) {
  HsStablePtr empty_trie = get_empty_trie();
  double weights[5] = {1.1, 2.2, 3.3, 4.4, 5.5};
  char* words[5] = {"one", "two", "three", "four", "five"};
  HsStablePtr mytrie = trie_from_arrays(weights, words, 5);

  TEST_ASSERT(is_in_trie("one", empty_trie) == 0);
  TEST_ASSERT(is_in_trie("one", mytrie) == 1);
}

void test_get_max_weight(void){
  HsStablePtr empty_trie = get_empty_trie();
  double weights[5] = {1.1, 2.2, 3.3, 4.4, 5.5};
  char* words[5] = {"one", "two", "three", "four", "five"};
  HsStablePtr mytrie = trie_from_arrays(weights, words, 5);

  TEST_ASSERT(get_max_weight(empty_trie) == -1);
  TEST_ASSERT(get_max_weight(mytrie) == 5.5);
}

void test_autocomplete_ReturnsEmptyList(void){
  HsStablePtr trie2 = read_terms("Data/mytrie2.txt");

  double weight_array[3] = {-1, -1, -1};
  char* word_array[3] = {NULL, NULL, NULL};

  autocomplete(weight_array, word_array, "i", trie2, 3);

  for(int i = 0; i < 3; i++){
    TEST_ASSERT(weight_array[i] == (-1.0));
    TEST_ASSERT(word_array[i] == NULL);
  }
}

void test_autocomplete_Returns2MatchesWhen3Requested(void){
  HsStablePtr trie3 = read_terms("Data/mytrie3.txt");

  double weight_array[3] = {-1, -1, -1};
  char* word_array[3] = {NULL, NULL, NULL};

  autocomplete(weight_array, word_array, "te", trie3, 3);

  TEST_ASSERT(weight_array[0] == (25));
  TEST_ASSERT(strcmp(word_array[0], "ted") == 0);
  TEST_ASSERT(weight_array[1] == (14));
  TEST_ASSERT(strcmp(word_array[1], "tea") == 0);
  TEST_ASSERT(weight_array[2] == (-1.0));
  TEST_ASSERT(word_array[2] == NULL);
}

void test_autocomplete_ReturnsTheCorrectMatches(void){
  HsStablePtr trie2 = read_terms("Data/mytrie2.txt");

  double weight_array[2] = {-1, -1};
  char* word_array[2] = {NULL, NULL};

  autocomplete(weight_array, word_array, "t", trie2, 2);

  TEST_ASSERT(weight_array[0] == (25));
  TEST_ASSERT(strcmp(word_array[0], "ted") == 0);
  TEST_ASSERT(weight_array[1] == (14));
  TEST_ASSERT(strcmp(word_array[1], "tea") == 0);
}

void test_slowcomplete_ReturnsEmptyList(void){
  double weight_array_in[4] = {20, 12, 14, 25};
  char* word_array_in[4] = {"a", "to", "tea", "ted"};

  double weight_array[3] = {-1, -1, -1};
  char* word_array[3] = {NULL, NULL, NULL};

  slowcomplete(weight_array, word_array, 4, "i",
               weight_array_in, word_array_in, 3);

  for(int i = 0; i < 3; i++){
    TEST_ASSERT(weight_array[i] == (-1.0));
    TEST_ASSERT(word_array[i] == NULL);
  }
}

void test_slowcomplete_Returns2MatchesWhen3Requested(void){
  double weight_array_in[4] = {20, 12, 14, 25};
  char* word_array_in[4] = {"a", "to", "tea", "ted"};

  double weight_array[3] = {-1, -1, -1};
  char* word_array[3] = {NULL, NULL, NULL};

  slowcomplete(weight_array, word_array, 4, "te", weight_array_in, word_array_in, 3);

  TEST_ASSERT(weight_array[0] == (25));
  TEST_ASSERT(strcmp(word_array[0], "ted") == 0);
  TEST_ASSERT(weight_array[1] == (14));
  TEST_ASSERT(strcmp(word_array[1], "tea") == 0);
  TEST_ASSERT(weight_array[2] == (-1.0));
  TEST_ASSERT(word_array[2] == NULL);
}

void test_slowcomplete_ReturnsTheCorrectMatches(void){
  double weight_array_in[4] = {20, 12, 14, 25};
  char* word_array_in[4] = {"a", "to", "tea", "ted"};

  double weight_array[2] = {-1, -1};
  char* word_array[2] = {NULL, NULL};

  slowcomplete(weight_array, word_array, 4, "t",
               weight_array_in, word_array_in, 2);

  TEST_ASSERT(weight_array[0] == (25));
  TEST_ASSERT(strcmp(word_array[0], "ted") == 0);
  TEST_ASSERT(weight_array[1] == (14));
  TEST_ASSERT(strcmp(word_array[1], "tea") == 0);
}

// not needed when using generate_test_runner.rb
int main(void) {
  tries_init();
  UNITY_BEGIN();
  RUN_TEST(test_is_in_trie);
  RUN_TEST(test_get_max_weight);
  RUN_TEST(test_autocomplete_ReturnsEmptyList);
  RUN_TEST(test_autocomplete_Returns2MatchesWhen3Requested);
  RUN_TEST(test_autocomplete_ReturnsTheCorrectMatches);
  RUN_TEST(test_slowcomplete_ReturnsEmptyList);
  RUN_TEST(test_slowcomplete_ReturnsTheCorrectMatches);
  RUN_TEST(test_slowcomplete_Returns2MatchesWhen3Requested);
  tries_end();
  return UNITY_END();
}
