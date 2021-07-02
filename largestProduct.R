# Given a list of integers, return the largest product that can be made by multiplying any three integers.
# For example, if the list is [-10, -10, 5, 2], we should return 500, since that's -10 * -10 * 5.
library(dplyr)

largestProduct <- function(list_of_integers) {

length_of_list <- length(list_of_integers)
possibleCombos <- NA

for (i in 1:length_of_list){

baseDf <- data.frame(base = list_of_integers[i], binary = 1)
comboDf1 <- data.frame(combo1 = list_of_integers, binary = 1)
comboDf2 <- data.frame(combo2 = list_of_integers, binary = 1)

finalDf <- baseDf %>% inner_join(comboDf1, by='binary')
finalDf <- finalDf %>% inner_join(comboDf2, by='binary')

finalDf <- finalDf %>% filter(base != combo1 & combo1 != combo2 & base != combo2) %>% select(-binary)
finalDf <- finalDf %>% mutate(product = base*combo1*combo2)
possibleCombos <- rbind(finalDf,possibleCombos)

}
largestCombo <- possibleCombos %>% filter(is.na(base) == FALSE) %>% arrange(desc(product)) %>% mutate(ranker = row_number()) %>% filter(ranker==1)

return(largestCombo$product[1])
}
list <- c(10,24,22,11,18,25,55,82,99,1000,5555)
largestProduct(list)
