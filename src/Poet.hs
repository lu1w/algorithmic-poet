{-
Author    : Yilu Wang 
Purpose   : Project 2 for Unimelb COMP30020 2024 Sem 2 - generation of 
            Haiku poems where there are 3 lines wiht 5, 7, 5 syllables 
            respectively on each line. 
Copyright   : (C) Yilu Wang, 2024


There are two main functions exposed to the outside world: `fillInPoem` and 
`generateAllHaikus`. These two functions generates poems that match specific 
syllables sequence(s), where `fillInPoem` follows the syllables sequence given 
by the caller, and `generateAllHaikus` follows the rules for Haiku poem (i.e. 5,
7, 5 syllables on each of the three lines in the poem). 

They both use a common lower level logic to compute the result, implemented in 
`mapToPoem`, where a map structure is used as the container to store and pass 
words around. This module relies heavily on the map structure to store the words 
and their corresponding number of syllables. 

The number of occurrances of each word in the output poems will not exceed its 
number of occurrances in the input wordlist (e.g. if the input wordlist does not
contain duplicates, the output poems will not have duplicate words). To achieve
this behaviour, every used-word is removed from the map structure before 
computing the next word in the output peom. See `mapToPoem` and other functions
for more implementation details. 
-}

module Poet 
  (fillInPoem, generateAllHaikus)
where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import ProblemSetup (Poem)
import Words (syllables)


{-| 
    A map of Int to [String], where the key represents the number of syllables,
    and the value represents the list of words with that number of syllables. 
    
    e.g. 
        +-----+------------------------+
        | Key | Value                  |
        +=====+========================+
        | 1   | ["a"]                  |
        | 2   | ["aback", "abaft"]     |
        | 3   | ["abalone", "abandon"] |
        +-----+------------------------+
        (the map represented using a table)
-}
type SyllablesWordsMap = M.Map Int [String]


{-|
    Take in a list of word; return a mapping from syllables to a list of words
    with that particular number of syllables. 
    
    It is assumed that we can get a valid syllables (i.e. non-Nothing) of all 
    input words using `syllables` from `Words` module. If any word in the list 
    of word does not allow this behavior, the function exits with an error. 
-}
getSyllablesWordsMap :: [String] -> SyllablesWordsMap
getSyllablesWordsMap =
    L.foldl' 
        (\mp word -> M.insertWith (++) (fromJust . syllables $ word) [word] mp) 
        M.empty  


{-|
    Take in a mapping from syllables to a list of words and a sequence of 
    syllables; return all the possible poems where the word syllables matches
    the sequence of syllables, using information from the mapping (i.e. only 
    words existed in the map will be used)
-}
mapToPoem :: SyllablesWordsMap -> [Int] -> [Poem] -- [[String]]
mapToPoem _ [] = [[]]
mapToPoem syllablesToWordsMap (sylb:sylbs) = 
    let generatePoems :: [String] -> [Poem]
        generatePoems wordsOfSylb = 
            [ word:remainingWords 
            | word <- wordsOfSylb
            , remainingWords <- 
                mapToPoem 
                    -- Remove the used word from the map before generating the
                    -- next word
                    (M.adjust (L.delete word) sylb syllablesToWordsMap)
                    sylbs
            ]
    in 
    maybe [] generatePoems (M.lookup sylb syllablesToWordsMap)


{-|
    Take in a mapping from syllables to a list of words and a list of sequences
    of syllables; return all the possible poems where the word syllables matches
    a sequence of syllables within the list of syllables sequences, using 
    information from the map (i.e. only words existed in the map will be used)
-}
mapToPoems :: SyllablesWordsMap -> [[Int]] -> [Poem]
mapToPoems _ [] = [] 
mapToPoems syllablesToWordsMap (sylbsSeq:sylbsSeqs) = 
    mapToPoem syllablesToWordsMap sylbsSeq ++ 
    mapToPoems syllablesToWordsMap sylbsSeqs
    
    
{-|
    Take in a `wordlist` and a `syllablesSequence`; return all the permutations
    of words that match the syllables pattern in `syllablesSequence` using words
    from `wordlist`. 
-}
fillInPoem :: [String] -> [Int] -> [Poem]
fillInPoem wordlist [] = []
fillInPoem wordlist syllablesSequence = 
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesWordsMap wordlist 
    in
    mapToPoem syllablesToWordsMap syllablesSequence


{-|
    Take in an integer; return all position lists of integers that sum up to it.
    If provided a non-positive integer, simply returns an empty list.  
-}
partitions :: Int -> [[Int]] 
partitions 0 = [[]]
partitions k = [ x:xs | x <- [1..k], xs <- partitions (k-x)]


{-|
    Take in a list of words as `wordlist`; return a list of all Haiku poems that
    can be written using the words from the input `wordlist`, where each poem is 
    represented as a list of words. 
    
    A Haiku poem contains three lines with 5, 7, 5 syllables on each line 
    respectively. The output poems are not grouped into lines but instead as 
    invidiual words in a list in the order they appear in the peom. 
-}
generateAllHaikus :: [String] -> [Poem]
generateAllHaikus wordlist = 
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesWordsMap wordlist
        
        partitionsFive :: [[Int]]
        partitionsFive = partitions 5

        partitionsSeven :: [[Int]]
        partitionsSeven = partitions 7

        haikuSyllablesSequences :: [[Int]] 
        haikuSyllablesSequences = 
            [ line1 ++ line2 ++ line3 
            | line1 <- partitionsFive
            , line2 <- partitionsSeven
            , line3 <- partitionsFive
            ]
    in 
    mapToPoems syllablesToWordsMap haikuSyllablesSequences