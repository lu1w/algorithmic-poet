module Tests1 where

import Poet (fillInPoem)
import Words (wordList)

wordlist100 = take 100 wordList
wordlist300 = take 300 wordList
wordlist500 = take 500 wordList

-- words1: 3syl*1, 4syl*1, 5syl*1
words1 = ["flowering", "jacaranda", "photosynthesis"]
-- words2: 3syl*2, 4syl*1, 5syl*1
words2 = ["cabinet", "flowering", "jacaranda", "photosynthesis"]
-- words3: 2syl*1, 3syl*1, 4syl*1, 5syl*1
words3 = ["above", "flowering", "jacaranda", "photosynthesis"]

-- words11: 1syl*5, 2syl*3, 3syl*2
words11 = ["a","abb","able","ache","ace","abase","abash","aardvark","abacus","abalone"]

test1 = -- (0.22 secs, 87,900,192 bytes)
    fillInPoem words1 [3,4,5] 
    == [["flowering","jacaranda","photosynthesis"]]
test2 = -- (0.22 secs, 87,900,192 bytes)
    fillInPoem words2 [3,4,3,5] 
    -- == [["cabinet","jacaranda","flowering","photosynthesis"],["flowering","jacaranda","cabinet","photosynthesis"]]

test3 = fillInPoem words11 [1,1,1,1,1,2,2,3,2,3] -- (1.41 secs, 162,990,612 bytes)
test4 = fillInPoem words11 [1,1,1,2,2,3,2,3,1,1] -- (1.49 secs, 163,047,488 bytes)
test5 = fillInPoem words11 [1,1,1,1,2,2,3,2,3] -- (1.28 secs, 148,220,208 bytes)
test6 = fillInPoem words11 [1,1,1,2,2,3,2,3] -- (0.71 secs, 107,374,416 bytes)
test7 = fillInPoem words11 [1,1,2,2,3,2,3] -- (0.50 secs, 101,361,112 bytes)
test8 = fillInPoem words11 [1,2,2,3,2,3] -- (0.38 secs, 100,865,356 bytes)
test9 = fillInPoem words11 [2,2,3,2,3,1,1,1,1,1] -- (1.62 secs, 163,909,224 bytes)
test10 = fillInPoem words11 [2,1,2,1,3,1,2,3,1,1] -- (1.50 secs, 161,744,096 bytes)


test111 = fillInPoem wordlist100 [1,2,3] -- (0.29 secs, 261,865,728 bytes)
test112 = fillInPoem wordlist100 [3,4,5] -- (0.44 secs, 342,308,280 bytes)
test113 = fillInPoem wordlist100 [3,3,5] -- (0.81 secs, 547,508,024 bytes)
test114 = fillInPoem wordlist100 [3,4,5,6] -- (1.41 secs, 838,194,600 bytes)
test115 = fillInPoem wordlist100 [3,5,3,4,3,4]

test131 = fillInPoem wordlist300 [1,2,3]
test132 = fillInPoem wordlist300 [3,4,5] -- (16.71 secs, 8,288,380,640 bytes)
test133 = fillInPoem wordlist300 [3,3,5] -- (30.00 secs, 14,930,567,264 bytes)
test134 = fillInPoem wordlist300 [3,4,5,6]
test135 = fillInPoem wordlist300 [3,5,3,4,3,4]

test151 = fillInPoem wordlist500 [1,2,3] -- (10.63 secs, 5,012,448,216 bytes)
test152 = fillInPoem wordlist500 [3,4,5] -- (70.64 secs, 34,341,004,936 bytes)
test153 = fillInPoem wordlist500 [3,3,5]
test154 = fillInPoem wordlist500 [3,4,5,6]

none1 = null (fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,3])
none2 = null (fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,6])
none3 = null (fillInPoem ["above", "flowering", "jacaranda", "photosynthesis"] [3,4,5,3])

tests = and [test1, none1, none2, none3]