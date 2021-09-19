module Main (main) where

import Criterion.Main
import Tries

autocompleteEnv = do
  nasdaq_trie <- readTerms "Data/nasdaq.txt"
  wiktionary_trie <- readTerms "Data/wiktionary.txt"
  artists_trie <- readTerms "Data/artists.txt"
  trademarks_trie <- readTerms "Data/trademarks.txt"
  movies_trie <- readTerms "Data/movies.txt"
  return (nasdaq_trie, wiktionary_trie,
          artists_trie, trademarks_trie,
          movies_trie)

songsEnv = do
  songs_trie <- readTerms "Data/songs.txt"
  return songs_trie

-- Our benchmarks
main :: IO ()
main = defaultMain
  [
  bgroup "readTerms" [
                       bench "nasdaq" $
                       whnfIO (readTerms "Data/nasdaq.txt"),
                       bench "wiktionary" $
                       whnfIO (readTerms "Data/wiktionary.txt"),
                       bench "arists" $
                       whnfIO (readTerms "Data/artists.txt"),
                       bench "trademarks" $
                       whnfIO (readTerms "Data/trademarks.txt"),
                       bench "movies" $
                       whnfIO (readTerms "Data/movies.txt"),
                       bench "songs" $
                       whnfIO (readTerms "Data/songs.txt")
                     ],

    env autocompleteEnv $ \ ~(nasdaq_trie,
                              wiktionary_trie,
                              artists_trie,
                              trademarks_trie,
                              movies_trie) ->
      bgroup "autocomplete" [
      bench "nasdaq with prefix = 'B' and k = 50" $
        whnf (\x -> autocomplete "B" x 50) nasdaq_trie,
      bench "nasdaq with prefix = 'Bank' and k = 50" $
        whnf (\x -> autocomplete "Bank" x 50) nasdaq_trie,
      bench "wiktionary with prefix = 't' and k = 5" $
        whnf (\x -> autocomplete "t" x 5) wiktionary_trie,
      bench "wiktionary with prefix = 't' and k = 100" $
        whnf (\x -> autocomplete "t" x 100) wiktionary_trie,
      bench "artists with prefix = 'The' and k = 5" $
        whnf (\x -> autocomplete "The" x 5) artists_trie,
      bench "trademarks with prefix = 'L' and k = 10" $
        whnf (\x -> autocomplete "L" x 10) trademarks_trie,
      bench "trademarks with prefix = 'LAW' and k = 10" $
        whnf (\x -> autocomplete "LAW" x 10) trademarks_trie,
      bench "movies with prefix = 'The' and k = 5" $
        whnf (\x -> autocomplete "The" x 5) movies_trie,
      bench "movies with prefix 'The Lord of the' and k = 5" $
        whnf (\x -> autocomplete "The Lord of the" x 5) movies_trie
    ]
    -- Uncommented songs environment because it takes up lots of memory
    --,

    -- env songsEnv $ \ ~(songs_trie) ->
    --   bgroup "autocomplete (songs)"
    -- [
    --   bench "songs with prefix 'I' and k = 5" $
    --     whnf (\x -> autocomplete "I" x 5) songs_trie
    -- ]
  ]
