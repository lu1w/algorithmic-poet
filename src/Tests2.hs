module Tests2 where

import Poet (generateAllHaikus)
import Words (wordList, syllables)

-- V1.0 - only evaluate the map once 
-- V2.0 - filter each line before cross product the lines 
-- V3.0 - remove previous word from map for each syllables in a line
-- V4.0 - remove previsou word from amp for each syllables in the poem 

test0 = 
    generateAllHaikus 
    ["abbreviation","abbreviator","americanisation"]
test1 =  
    -- length = 6624. 
    -- (1.37 secs, 1,957,709,520 bytes)
    -- (1.25 secs, 1,802,500,120 bytes) - V1.0 
    -- (0.28 secs, 377,004,096 bytes) - V3.0
    generateAllHaikus
    (take 10 wordList)

test1_2 =  
    -- length = 59136. 
    -- (5.92 secs, 7,352,794,792 bytes) - V1.0 
    -- (1.36 secs, 1,717,713,536 bytes) - V2.0
    -- (1.38 secs, 1,717,511,168 bytes) - V3.0
    generateAllHaikus
    (take 12 wordList)
    
test1_5 = 
    -- length = 899712. 
    -- (47.87 secs, 50,269,949,056 bytes)
    -- (45.18 secs, 50,038,674,328 bytes) - V1.0 
    -- (16.27 secs, 19,314,045,912 bytes) - V2.0
    -- (16.34 secs, 19,313,667,128 bytes) - V3.0
    generateAllHaikus
    (take 15 wordList) 

test2 = 
    -- length = 8249472
    -- (71.09 secs, 72,848,576,704 bytes) - V3.0
    -- (14.07 secs, 19,007,762,168 bytes) - V4.0
    generateAllHaikus
    (take 20 wordList)

test2_5 = 
    -- 8249472 
    -- (18.47 secs, 19,007,762,176 bytes) - V4.0
    generateAllHaikus
    (take 20 wordList)

test3 = 
    generateAllHaikus
    (take 30 wordList)

test5 = 
    generateAllHaikus
    (take 50 wordList)
    
test10 = 
    generateAllHaikus
    (take 100 wordList)


test90to100 = 
    -- length = 228
    -- (0.10 secs, 204,449,376 bytes) - V1.0 
    generateAllHaikus
    (drop 90 (take 100 wordList)) 
    
test90to105 = 
    -- length = 22440
    -- (0.22 secs, 310,334,072 bytes) - V1.0 
    generateAllHaikus
    (drop 90 (take 105 wordList)) 

test90to110 = 
    -- length = 934560 
    -- (4.23 secs, 3,747,928,032 bytes) - V1.0
    -- (4.43 secs, 4,320,881,208 bytes) - V2.0 
    -- (4.47 secs, 4,321,103,160 bytes) - V3.0
    -- (1.68 secs, 2,588,713,672 bytes) - V4.0
    generateAllHaikus
    (drop 90 (take 110 wordList)) 

test160to175 = 
    -- 1008000
    -- (5.27 secs, 4,707,992,432 bytes) - V1.0
    -- (5.53 secs, 5,446,570,856 bytes) - V2.0
    -- (5.51 secs, 5,446,756,232 bytes) - V3.0
    -- (1.98 secs, 2,720,293,312 bytes) - V4.0
    generateAllHaikus
    (drop 160 (take 175 wordList)) 


test170to180 = 
    -- length = 15552
    -- (1.89 secs, 2,516,914,544 bytes) - V1.0 
    -- (0.39 secs, 515,514,736 bytes) - V3.0
    -- (0.27 secs, 433,160,408 bytes) - V4.0
    generateAllHaikus
    (drop 170 (take 180 wordList)) 

test170to190 = 
    -- 540264
    -- (10.67 secs, 12,259,345,376 bytes) - V1.0
    -- (3.47 secs, 3,749,804,432 bytes) - V2.0
    -- (3.39 secs, 3,749,643,584 bytes) - V3.0
    -- (1.42 secs, 1,988,407,648 bytes) - V4.0
    generateAllHaikus
    (drop 170 (take 190 wordList)) 

test170to200 = 
    -- 9507780
    -- (15.11 secs, 19,922,482,352 bytes) - V4.0
    generateAllHaikus
    (drop 170 (take 200 wordList)) 

test370to390 = 
    -- 9016704
    -- (19.22 secs, 20,272,019,480 bytes) - V4.0
    generateAllHaikus
    (drop 370 (take 390 wordList)) 

test370to410 = 
    -- >2min - V4.0
    generateAllHaikus
    (drop 370 (take 410 wordList)) 

test470to500 = 
    -- 4412760
    -- (6.62 secs, 8,461,984,984 bytes) - V4.0
    generateAllHaikus
    (drop 470 (take 500 wordList)) 

test470to510 = 
    -- >2min - V4.0
    generateAllHaikus
    (drop 470 (take 510 wordList)) 

test00 = 
    -- length = 1157760
    -- > 2min - V3.0
    -- (13.95 secs, 17,226,037,528 bytes)!!! - V4.0
    generateAllHaikus 
    ["a","abb","able","ache","ace","abase","abash","aardvark","abacus","abalone"]


none0 = 
    generateAllHaikus 
    ["abbreviator","americanisation"]

none1 = 
    generateAllHaikus
    (filter (\x -> syllables x == Just 4) wordList)