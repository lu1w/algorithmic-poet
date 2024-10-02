module Tests2 where

import Poet (generateAllHaikus)
import Words (wordList, syllables)
import Tests1 (test2)

-- V1.0 - only evaluate the map once 
-- V2.0 - filter each line before cross product the lines 

test0 = 
    generateAllHaikus 
    ["abbreviation","abbreviator","americanisation"]
test1 =  
    -- length = 6624. 
    -- (1.37 secs, 1,957,709,520 bytes)
    -- (1.25 secs, 1,802,500,120 bytes) - V1.0 
    generateAllHaikus
    (take 10 wordList)

test1_2 =  
    -- length = 59136. 
    -- (5.92 secs, 7,352,794,792 bytes) - V1.0 
    -- (1.36 secs, 1,717,713,536 bytes) - V2.0
    generateAllHaikus
    (take 12 wordList)
    
test1_5 = 
    -- length = 899712. 
    -- (47.87 secs, 50,269,949,056 bytes)
    -- (45.18 secs, 50,038,674,328 bytes) - V1.0 
    -- (16.27 secs, 19,314,045,912 bytes) - V2.0
    generateAllHaikus
    (take 15 wordList) 

test2 = 
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
    generateAllHaikus
    (drop 90 (take 110 wordList)) 

test160to175 = 
    -- 1008000
    -- (5.27 secs, 4,707,992,432 bytes) - V1.0
    -- (5.53 secs, 5,446,570,856 bytes) - V2.0
    generateAllHaikus
    (drop 160 (take 175 wordList)) 


test170to180 = 
    -- length = 15552
    -- (1.89 secs, 2,516,914,544 bytes) - V1.0 
    generateAllHaikus
    (drop 170 (take 180 wordList)) 

test170to190 = 
    -- 540264
    -- (10.67 secs, 12,259,345,376 bytes) - V1.0
    -- (3.47 secs, 3,749,804,432 bytes) - V2.0
    generateAllHaikus
    (drop 170 (take 190 wordList)) 


test00 = 
    generateAllHaikus 
    ["a","abb","able","ache","ace","abase","abash","aardvark","abacus","abalone"]


none0 = 
    generateAllHaikus 
    ["abbreviator","americanisation"]

none1 = 
    generateAllHaikus
    (filter (\x -> syllables x == Just 4) wordList)