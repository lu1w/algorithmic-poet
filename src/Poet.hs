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


mapToPoem :: SyllablesWordsMap -> [Int] -> [Poem]
mapToPoem syllablesToWordsMap syllablesSequence = 
    let wordsForSyllables :: Maybe [[String]]
        wordsForSyllables = 
            getWordsForAllSyllables syllablesToWordsMap syllablesSequence
    in 
    maybe [] (filter distinct . sequence) wordsForSyllables


-- getWordsForAllSyllables :: SyllablesWordsMap -> [Int] -> Maybe [[String]]
-- getWordsForAllSyllables syllablesToWordsMap =
--     foldM (\acc s -> (:) <$> M.lookup s syllablesToWordsMap <*> Just acc) []

fillInPoem :: [String] -> [Int] -> [Poem]
fillInPoem wordlist syllablesSequence =
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesToWords wordlist 
    in 
    mapToPoem syllablesToWordsMap syllablesSequence


partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions k = [x:xs | x <- [1..k], xs <- partitions (k-x)]


generateAllHaikus :: [String] -> [Poem]
generateAllHaikus wordlist = 
    let syllablesToWordsMap :: SyllablesWordsMap
        syllablesToWordsMap = getSyllablesToWords wordlist
       
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
     
        fillInHaikus :: [[Int]] -> [Poem]
        fillInHaikus [] = [] 
        fillInHaikus (sylbsSeq:sylbsSeqs) = 
            mapToPoem syllablesToWordsMap sylbsSeq ++ 
            fillInHaikus sylbsSeqs
    in 
    fillInHaikus syllablesSequences
