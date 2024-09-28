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

type MapSyllablesWords = M.Map Int [String]


justSyllables :: String -> Int
justSyllables = fromJust . syllables


groupWordsBySyllables :: [String] -> [[String]]
groupWordsBySyllables wordlist = 
    let haveSameSyllables :: String -> String -> Bool 
        haveSameSyllables wd1 wd2 = (syllables wd1) == (syllables wd2)
    in 
    L.groupBy haveSameSyllables (L.sortOn justSyllables wordlist)


getWordsForAllSyllables :: MapSyllablesWords -> [Int] -> Maybe [[String]]
getWordsForAllSyllables syllablesToWordsMap [] = Just []
getWordsForAllSyllables syllablesToWordsMap syllables = 
    let wordsOfSyllables :: [Maybe [String]]
        wordsOfSyllables = 
            map (\syl -> M.lookup syl syllablesToWordsMap) syllables 
    in 
    sequence wordsOfSyllables


fillInPoem :: [String] -> [Int] -> [Poem]
fillInPoem wordlist [] = []
fillInPoem wordlist wordSyllables = 
    let groupedWords = groupWordsBySyllables wordlist :: [[String]]
    in 
    let syllablesToWordsMap :: MapSyllablesWords
        syllablesToWordsMap = 
            L.foldl 
                (\mp wds -> M.insert (justSyllables (head wds)) wds mp) 
                M.empty 
                groupedWords 
    in 
    let wordsForSyllables :: Maybe [[String]]
        wordsForSyllables = 
            getWordsForAllSyllables syllablesToWordsMap wordSyllables
    in
    if wordsForSyllables == Nothing then []
    else filter distinct (sequence (fromJust wordsForSyllables))


generateAllHaikus :: [String] -> [Poem]
generateAllHaikus wordlist = fillInPoem wordlist [5, 7, 5]


