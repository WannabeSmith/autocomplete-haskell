import Test.Hspec
import Tries
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import Data.MinMaxQueue (MinMaxQueue)
import qualified Data.MinMaxQueue as MMQ
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do

  {-
mytrie1:
a 20
t
  e
    a 14
  o 12
-}
  let teaNode1 = PrefixNode "a" 14 (-1) HashMap.empty
  let teNode1  = PrefixNode "e" (-1) 14 $ HashMap.fromList [("a", teaNode1)]
  let toNode1  = PrefixNode "o" 12 (-1) HashMap.empty
  let tNode1   = PrefixNode "t" (-1) 14 $ HashMap.fromList [("o", toNode1), ("e", teNode1)]
  let aNode1   = PrefixNode "a" 20 (-1) HashMap.empty
  let mytrie1  = PrefixNode "" (-1) 20 $ HashMap.fromList [("t", tNode1), ("a", aNode1)]

  {-
mytrie2:
a 20
t
  e
    a 14
    d 25
  o 12
-}

  let tedNode2 = PrefixNode "d" 25 (-1) HashMap.empty
  let teaNode2 = teaNode1
  let teNode2 = PrefixNode "e" (-1) 25 $ HashMap.fromList [("a", teaNode2), ("d", tedNode2)]
  let toNode2 = toNode1
  let tNode2 = PrefixNode "t" (-1) 25 $ HashMap.fromList [("e", teNode2), ("o", toNode2)]
  let aNode2 = PrefixNode "a" 20 (-1) HashMap.empty
  let mytrie2 = PrefixNode "" (-1) 25 $ HashMap.fromList [("a", aNode2), ("t", tNode2)]

  {-
mytrie3:
a 20
  n 19.25
t
  e
    a 14
    d 25
  o 12
-}
  let tNode3 = tNode2
  let anNode3 = PrefixNode "n" 19.25 (-1) HashMap.empty
  let aNode3 = PrefixNode "a" 20 19.25 $ HashMap.fromList [("n", anNode3)]
  let mytrie3 = PrefixNode "" (-1) 25 $ HashMap.fromList [("a", aNode3), ("t", tNode3)]

  {-
mytrie4:
a 20
  n 30
t
  e
    a 14
    d 25
  o 12
-}
  let tNode4 = tNode2
  let anNode4 = PrefixNode "n" 30 (-1) HashMap.empty
  let aNode4 = PrefixNode "a" 20 30 $ HashMap.fromList [("n", anNode4)]
  let mytrie4 = PrefixNode "" (-1) 30 $ HashMap.fromList[("a", aNode4), ("t", tNode4)]

  describe "mapChildren" $ do
    it "deletes all grandchildren when setting all children's children to empty" $ do
      mapChildren (\child -> child{ children = HashMap.empty }) mytrie4 `shouldBe`
        trieFromList [(20, "a"), (-1, "t")]
    it "returns the same trie when the function is the identity" $ do
      mapChildren (\child -> child) mytrie4 `shouldBe` mytrie4

  describe "filterChildren" $ do
    it "returns the same trie when the predicate is satisfied by all children" $
      filterChildren (\child -> Tries.max_child_wt child < 35) mytrie4 `shouldBe`
      mytrie4
    it "returns an updated trie when the filter only applies for some children" $
      filterChildren (\child -> Tries.max_child_wt child > 26) mytrie4 `shouldBe`
      trieFromList [(20, "a"), (30, "an")]
    it "removes all children when they don't satisfy the predicate" $
      filterChildren (\child -> Tries.max_child_wt child > 35) mytrie4 `shouldBe`
      rootNode

  describe "foldrChildren" $ do
    it "counts up the total of the max child weights" $ do
      foldrChildren
        (\child sum_so_far -> sum_so_far + Tries.max_child_wt child)
        0 mytrie4 `shouldBe` 55
    it "gets the maximum of all weights in the trie" $ do
       foldrChildren
         (\child sum_so_far -> max sum_so_far $
                               (max . Tries.weight) child $
                               Tries.max_child_wt child)
        (-1) mytrie4 `shouldBe` 30

  describe "getNextNode" $ do
    it "returns the EmptyTrie when the next node doesn't exist" $ do
      getNextNode "d" teNode1 `shouldBe` EmptyTrie
    it "returns the next node when the next node does exist" $ do
      getNextNode "d" teNode2 `shouldBe` tedNode2

  describe "getNode" $ do
    it "gets the specified (existing) node" $ do
      getNode "te" mytrie1 `shouldBe` teNode1
    it "returns the EmptyTrie when the string doesn't exist there" $ do
      getNode "team" mytrie1 `shouldBe` EmptyTrie

  -- insertWords implicitly tests insertWord and fromList
  describe "insertWords" $ do
    it "builds a trie from the rootNode when given an EmptyTrie" $ do
      insertWords
        [(12, "to"), (25, "ted"), (14, "tea"), (20, "a"), (30, "an")]
        EmptyTrie `shouldBe` mytrie4
    it "can construct a trie from an (unsorted) list of words, and a rootNode" $ do
      insertWords [(12, "to"), (25, "ted"), (14, "tea"), (20, "a"), (30, "an")] rootNode `shouldBe` mytrie4
    it "inserts words into a trie and update some max child weights when appropriate" $ do
      (insertWords [(25, "ted"), (19.25, "an")] mytrie1) `shouldBe` mytrie3
    -- In this example, note that "a" has an updated weight of 27.5
    it "updates weights when words already present (but doesn't erase children)" $ do
      insertWords [(12, "to"), (25, "ted"), (14, "tea"), (27.5, "a"), (30, "an")] rootNode `shouldBe` insertWords [(27.5, "a")] mytrie4

  describe "updateWeight" $ do
    it "throws an error when the node doesn't exist" $ do
      evaluate(updateWeight succ "ted" mytrie1) `shouldThrow` errorCall "Can't update weight of a non-existing node"
    it "updates the weight of a node when passed the successor function" $ do
      updateWeight succ "tea" mytrie1 `shouldBe`
        trieFromList [(20, "a"), (15, "tea"), (12, "to")]
    it "divides the weight of a node when passed such a function" $ do
      updateWeight (\x -> (1/5) * x) "an" mytrie4 `shouldBe`
        trieFromList
        [(20, "a"), (14, "tea"), (12, "to"), (25, "ted"), (6, "an")]

  describe "insertOrUpdate" $ do
    it "returns a new trie when given EmptyTrie" $ do
      insertOrUpdate succ "an" EmptyTrie `shouldBe` insertPrefix 0 "an" rootNode
    it "inserts when the node is not present" $ do
      insertOrUpdate succ "tears" mytrie4 `shouldBe` insertPrefix 0 "tears" mytrie4
    it "updates when the node is present" $ do
      insertOrUpdate succ "tears" (insertPrefix 0 "tears" mytrie4) `shouldBe`
        insertPrefix 1 "tears" mytrie4
    it "updates when the node is present and has children" $ do
      insertOrUpdate (\x -> x / 2) "a" mytrie4 `shouldBe`
        trieFromList
        [(10, "a"), (30, "an"), (12, "to"), (14, "tea"), (25, "ted")]

  describe "rescaleWeights" $ do
    it "returns the EmptyTrie when passed the EmptyTrie" $ do
      rescaleWeights id EmptyTrie `shouldBe` EmptyTrie
    it "leaves the trie untouched when the function is the identity" $ do
      rescaleWeights id mytrie4 `shouldBe` mytrie4
    it "increments every value by 1 when passed successor function" $ do
      rescaleWeights succ mytrie4 `shouldBe`
        trieFromList
        [(21, "a"), (31, "an"), (13, "to"), (15, "tea"), (26, "ted")]
    it "halves every value when passed the function that halves a double" $ do
      rescaleWeights (\x -> x/2) mytrie4 `shouldBe`
        trieFromList
        [(10, "a"), (15, "an"), (6, "to"), (7, "tea"), (12.5, "ted")]

  describe "isInTrie" $ do
    it "correctly determines that a node is in the trie" $ do
      isInTrie "an" mytrie3 `shouldBe` True
    it "correctly determines that a node is not in the trie" $ do
      isInTrie "an" mytrie1 `shouldBe` False

  describe "updateMaxChildWeight" $ do
    it "returns the EmptyTrie when passed it" $ do
      updateMaxChildWeight EmptyTrie `shouldBe` EmptyTrie
    it "Leaves the max_child_wt unchanged if it's correct" $ do
      updateMaxChildWeight teNode2 `shouldBe` teNode2
    it "Updates the max_child_wt if it's stale" $ do
      updateMaxChildWeight teNode2{children =
                                  HashMap.delete "d" $ Tries.children teNode2
                                 }
        `shouldBe` teNode2{children =
                          HashMap.delete "d" $ Tries.children teNode2,
                          max_child_wt = 14
                         }

  describe "removePrefix" $ do
    it "returns the EmptyTrie when given it" $ do
      removePrefix "ted" EmptyTrie `shouldBe` EmptyTrie
    it "removes a node from the tree when that node exists" $ do
      removePrefix "ted" mytrie2 `shouldBe` mytrie1
    it "leaves the trie untouched when the node does not exist" $ do
      removePrefix "ted" mytrie1 `shouldBe` mytrie1
    it "only deletes the node specified, and doesn't touch its children" $ do
      removePrefix "a" mytrie3 `shouldBe`
        trieFromList [(19.25, "an"), (14, "tea"), (25, "ted"), (12, "to")]
    it "removes in the same way that insertPrefix inserts" $ do
      removePrefix "a very long string"
        (insertPrefix 10 "a very long string" mytrie4) `shouldBe` mytrie4

  describe "getMaxWeight" $ do
    it "returns -1 for the EmptyTrie" $ do
      getMaxWeight EmptyTrie `shouldBe` (-1)
    it "gets the maximum weight of everything in a trie (including the node)" $ do
      getMaxWeight mytrie4 `shouldBe` 30

  -- Testing pruneTrie also implicitly tests its helper function, pruneChildren
  describe "pruneTrie" $ do
    it "returns the EmptyTrie when passed it" $ do
      pruneTrie 9000 EmptyTrie `shouldBe` EmptyTrie
    it "leaves the trie untouched when every node is above the threshold" $ do
      pruneTrie 1 mytrie4 `shouldBe` mytrie4
    it "deletes a few nodes that are below the threshold" $ do
      pruneTrie 20 mytrie4 `shouldBe`
        trieFromList
        [(20, "a"), (30, "an"), (25, "ted")]
    it "deletes all nodes when they're all below the threshold" $ do
      pruneTrie 30.01 mytrie4 `shouldBe` rootNode

  describe "getChildrenAsList" $ do
    it "returns an empty list when passed EmptyTrie" $ do
      getChildrenAsList EmptyTrie `shouldBe` []
    it "gets the children of a trie as a list, rather than a HashMap" $ do
      getChildrenAsList teNode2 `shouldBe` (Prelude.map snd $ HashMap.toList $ children teNode2)
    it "returns the empty list when the trie has no children" $ do
      getChildrenAsList anNode4 `shouldBe` []

  describe "textToWtdWord" $ do
    it "returns a tuple of a Double and a String when correctly formatted" $ do
      (textToWtdWord $ Text.pack "      1.5\tsome word") `shouldBe` (1.5, "some word")
    it "throws an error when the string has no tabs" $ do
      evaluate (textToWtdWord $ Text.pack "      1.5 some word") `shouldThrow` errorCall "Malformed weighted word:       1.5 some word"
    it "throws an error when the weight is malformed" $ do
      evaluate (textToWtdWord $ Text.pack "    3point5\tsome word") `shouldThrow` errorCall "Malformed weighted word:     3point5\tsome word"

  myFileTrie1 <- runIO $ readTerms "../Data/mytrie1.txt"
  myFileTrie2 <- runIO $ readTerms "../Data/mytrie2.txt"
  myFileTrie3 <- runIO $ readTerms "../Data/mytrie3.txt"
  myFileTrie4 <- runIO $ readTerms "../Data/mytrie4.txt"

  describe "readTerms" $ do
    it "returns mytrie1 from mytrie1.txt" $ do
      myFileTrie1 `shouldBe` mytrie1
    it "returns mytrie2 from mytrie2.txt" $ do
      myFileTrie2 `shouldBe` mytrie2
    it "returns mytrie3 from mytrie3.txt" $ do
      myFileTrie3 `shouldBe` mytrie3
    it "returns mytrie4 from mytrie4.txt when the file is in an arbitrary order" $ do
      myFileTrie4 `shouldBe` mytrie4

  describe "mmqMinWeight" $ do
    it "returns -1 if the queue is empty (by convention)" $ do
      mmqMinWeight MMQ.empty `shouldBe` (-1)
    it "returns the minimum weight in the queue when non-empty" $ do
      let myMmq = MMQ.fromListWith autocompletePriority [(1, "hello"), (2, "friend")]
      mmqMinWeight myMmq `shouldBe` 1

  describe "mmqInsert" $ do
    let myMmq = MMQ.fromListWith autocompletePriority [(1, "hello"), (2, "friend")]
    it "Does nothing when the weight to insert is -1 (by convention)" $ do
      mmqInsert (-1) "some word" myMmq `shouldBe` myMmq
    it "Inserts the non-(-1) weight with the autocomplete priority" $ do
      let myMmq2 = MMQ.fromListWith autocompletePriority
                   [(1, "hello"), (2, "friend"), (3, "some word")]
      mmqInsert 3 "some word" myMmq `shouldBe` myMmq2

  -- autocomplete implicity tests getMatchingPrefixes.
  -- getMatchingPrefixes also has an unpredictable order so it's not great to test
  describe "autocomplete" $ do
    it "returns the empty list when the string is not in the trie" $ do
      autocomplete "i" mytrie2 3 `shouldBe` ([] :: [(Double, String)])
    it "returns the top 2 matches when the string is in the trie" $ do
      autocomplete "t" mytrie2 2 `shouldBe` [(25, "ted"), (14, "tea")]
    it "does not return 't' when passed 'te', but does return 'ted' and 'tea'" $ do
      autocomplete "te" mytrie3 2 `shouldBe` [(25, "ted"), (14, "tea")]
    it "returns 2 words when there are only 2 matches, even though 3 were requested" $ do
      autocomplete "te" mytrie3 3 `shouldBe` [(25, "ted"), (14, "tea")]
    it "returns the child with higher weight even when a parent word node matches" $ do
      autocomplete "a" mytrie4 2 `shouldBe` [(30, "an"), (20, "a")]

  describe "slowComplete" $ do
    it "returns the empty list when the string is not in the trie" $ do
      slowComplete "i" [(20, "a"), (12, "to"), (14, "tea"), (25, "ted")] 3 `shouldBe` ([] :: [(Double, String)])
    it "returns the top 2 matches when the string is in the trie" $ do
      slowComplete "t" [(20, "a"), (12, "to"), (14, "tea"), (25, "ted")] 2 `shouldBe` [(25, "ted"), (14, "tea")]
    it "does not return 't' when passed 'te', but does return 'ted' and 'tea'" $ do
      slowComplete "te" [(20, "a"), (12, "to"), (14, "tea"), (25, "ted")] 2 `shouldBe` [(25, "ted"), (14, "tea")]
    it "returns 2 words when there are only 2 matches, even though 3 were requested" $ do
      slowComplete "te" [(20, "a"), (12, "to"), (14, "tea"), (25, "ted")] 3 `shouldBe` [(25, "ted"), (14, "tea")]

