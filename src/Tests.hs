module Tests where

import Poet (fillInPoem)

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
    == [["cabinet","jacaranda","flowering","photosynthesis"],["flowering","jacaranda","cabinet","photosynthesis"]]

test3 = fillInPoem words11 [1,1,1,1,1,2,2,3,2,3] -- (1.41 secs, 162,990,612 bytes)
test4 = fillInPoem words11 [1,1,1,2,2,3,2,3,1,1] -- (1.49 secs, 163,047,488 bytes)
test5 = fillInPoem words11 [1,1,1,1,2,2,3,2,3] -- (1.28 secs, 148,220,208 bytes)
test6 = fillInPoem words11 [1,1,1,2,2,3,2,3] -- (0.71 secs, 107,374,416 bytes)
test7 = fillInPoem words11 [1,1,2,2,3,2,3] -- (0.50 secs, 101,361,112 bytes)
test8 = fillInPoem words11 [1,2,2,3,2,3] -- (0.38 secs, 100,865,356 bytes)
test9 = fillInPoem words11 [2,2,3,2,3,1,1,1,1,1] -- (1.62 secs, 163,909,224 bytes)
test10 = fillInPoem words11 [2,1,2,1,3,1,2,3,1,1] -- (1.50 secs, 161,744,096 bytes)

none1 = null (fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,3])
none2 = null (fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,6])
none3 = null (fillInPoem ["above", "flowering", "jacaranda", "photosynthesis"] [3,4,5,3])

tests = and [test1, test2, none1, none2, none3]