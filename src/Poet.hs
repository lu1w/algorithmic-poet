{-
    Author    : Yilu Wang 
    Purpose   : Project 2 for Unimelb COMP30020 2024 Sem 2 - generation of 
                Haiku poems where there are 3 lines wiht 5, 7, 5 syllables 
                respectively on each line. 
-}

module Poet
  (fillInPoem, generateAllHaikus)
where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import ProblemSetup (Poem, PoemMetric(..), PoemScore, distinct)
import Words (syllables)

import Debug.Trace

type SyllablesWordsMap = M.Map Int [String]


justSyllables :: String -> Int
justSyllables = fromJust . syllables


-- groupWordsBySyllables :: [String] -> [[String]]
-- groupWordsBySyllables wordlist =
--     let haveSameSyllables :: String -> String -> Bool
--         haveSameSyllables wd1 wd2 = syllables wd1 == syllables wd2
--     in
--     L.groupBy haveSameSyllables (L.sortOn justSyllables wordlist)

{- 
    Take in a wordlist; return a map, where the key is the number of syllables,
    and the value is the list of words in the wordlist with the number of 
    syllables.
-}
getSyllablesToWords :: [String] -> SyllablesWordsMap
getSyllablesToWords = 
    L.foldl' 
        (\mp word -> M.insertWith (++) (justSyllables word) [word] mp)
        M.empty


getWordsForAllSyllables :: SyllablesWordsMap -> [Int] -> Maybe [[String]]
getWordsForAllSyllables syllablesToWordsMap syllables =
    let wordsOfSyllables :: [Maybe [String]]
        wordsOfSyllables = map (`M.lookup` syllablesToWordsMap) syllables
    in
    sequence wordsOfSyllables
-- getWordsForAllSyllables :: SyllablesWordsMap -> [Int] -> Maybe [[String]]
-- getWordsForAllSyllables syllablesToWordsMap =
--     foldM (\acc s -> (:) <$> M.lookup s syllablesToWordsMap <*> Just acc) []


{-
    Take in a mapping from syllables to a list of words and a sequence of 
    syllables; return all the possible poems where the word syllables matches
    the sequence of syllables, using information from the mapping (i.e. only 
    words existed in the map will be used)
-}
_mapToPoem :: SyllablesWordsMap -> [Int] -> [Poem]
_mapToPoem syllablesToWordsMap syllablesSequence = 
    let wordsForSyllables :: Maybe [[String]]
        wordsForSyllables = 
            getWordsForAllSyllables syllablesToWordsMap syllablesSequence
    in 
    maybe [] (filter distinct . sequence) wordsForSyllables


mapToPoem :: SyllablesWordsMap -> [Int] -> [Poem] -- [[String]]
mapToPoem _ [] = [[]]
mapToPoem syllablesToWordsMap (sylb:sylbs) = 
    let generatePoems :: [String] -> [Poem]
        generatePoems wordsForSylb = 
            [ word:remainingWords 
            | word <- wordsForSylb
            , remainingWords <- 
                mapToPoem 
                    (M.adjust (L.delete word) sylb syllablesToWordsMap)
                    sylbs
            ]
    in 
    maybe [] generatePoems (M.lookup sylb syllablesToWordsMap)

{-
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


fillInPoem :: [String] -> [Int] -> [Poem]
fillInPoem wordlist syllablesSequence =
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesToWords wordlist 
    in 
    mapToPoem syllablesToWordsMap syllablesSequence


{-
    Partitions an interger into one or more integers that sum up to it. 
-}
partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions k = [x:xs | x <- [1..k], xs <- partitions (k-x)]


generateAllHaikus :: [String] -> [Poem]
generateAllHaikus wordlist = 
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesToWords wordlist
       
        -- fillInFiveSyllables :: [Poem]
        -- fillInFiveSyllables = 
        --     mapToPoems syllablesToWordsMap (partitions 5)
            
        -- fillInSevenSyllables :: [Poem]
        -- fillInSevenSyllables = 
        --     mapToPoems syllablesToWordsMap (partitions 7)

        partitionsFive :: [[Int]]
        partitionsFive = partitions 5

        partitionsSeven :: [[Int]]
        partitionsSeven = partitions 7

        
        syllablesSequences :: [[Int]] 
        syllablesSequences = 
            [ line1 ++ line2 ++ line3 
            | line1 <- partitionsFive
            , line2 <- partitionsSeven
            , line3 <- partitionsFive
            ]

        -- fillInHaikus :: [[Int]] -> [Poem]
        -- fillInHaikus [] = [] 
        -- fillInHaikus (sylbsSeq:sylbsSeqs) = 
        --     fillInPoem wordlist sylbsSeq ++ fillInHaikus sylbsSeqs
     
        -- fillInHaikus :: [[Int]] -> [Poem]
        -- fillInHaikus [] = [] 
        -- fillInHaikus (sylbsSeq:sylbsSeqs) = 
        --     mapToPoem syllablesToWordsMap sylbsSeq ++ 
        --     fillInHaikus sylbsSeqs
    in 
    -- filter 
    --     distinct 
    --     [ line1 ++ line2 ++ line3 
    --     | line1 <- fillInFiveSyllables
    --     , line2 <- fillInSevenSyllables
    --     , line3 <- fillInFiveSyllables
    --     ]
    mapToPoems syllablesToWordsMap syllablesSequences