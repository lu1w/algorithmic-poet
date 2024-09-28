--Implement your solution here
--SEE THE PROJECT CODING GUIDELINES ON THE LMS FOR DETAILS OF
--THE CRITERIA THAT WILL BE EMPLOYED IN ASSESSING YOUR CODE.
--Please DELETE THIS WHOLE COMMENT, and write your own.

{-
    File       : Proj2.hs
    Author     : Yilu Wang 
    Student ID : 1362569
    Purpose    : Project 2 for Unimelb COMP30020 2024 Sem 2 - generation of 
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
{-
getWordsForAllSyllables syllablesToWordsMap (syl:syls) = 
    let wordsOfSyllables :: [String] 
        wordsOfSyllables = M.lookup syl syllablesToWordsMap 
    in
    if wordsOfSyllables == Nothing then Nothing 
    else 
        let wordsOfSyllablesList = 
                getWordsForAllSyllables syllablesToWordsMap syls 
        in 
        if wordsOfSyllablesList == Nothing then Nothing 
        else Just ((fromJust wordsOfSyllables):(fromJust wordsOfSyllablesList))
-}

-- TODO: implement this function
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

-- TODO: implement this function
-- each word must be distinct in the poem
generateAllHaikus :: [String] -> [Poem]
generateAllHaikus wordlist = fillInPoem wordlist [5, 7, 5]


