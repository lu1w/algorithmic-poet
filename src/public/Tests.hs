module Tests where 

import Proj2 

test1 = fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,5]
test2 = fillInPoem ["cabinet", "flowering", "jacaranda", "photosynthesis"] [3,4,3,5]

none1 = fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,3]
none2 = fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,6]
none3 = fillInPoem ["above", "flowering", "jacaranda", "photosynthesis"] [3,4,5,3]