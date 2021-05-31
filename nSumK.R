# Given a list of integers S and a target number k,
# write a function that returns a subset of S that adds up to k.

# If such a subset cannot be made, then return null.
# Integers can appear more than once in the list. You may assume all numbers in the list are positive.

library(dplyr)


nSumK <- function(integers,k_value) {

## turn list into dataframe
df <- data.frame(value = matrix(unlist(integers), nrow=length(integers), byrow=TRUE))

success <- FALSE
counter <- 0

while (!success & counter <= 1000000) { ## if more than 1m combinations don't result in a match, cut loop. this is somewhat arbitrary, so a point of optimization could begin here

randomPerm <- df %>% sample_n(sample(1:nrow(df),1),replace = FALSE)
permList <- list(randomPerm$value)
permSum <- sum(randomPerm$value)
success <- permSum == k_value

counter <- sum(counter,1)
}
return(list(permList,counter))
}

integerList <- c(4,2,1,2,3,54,7,1,11,12,56,79,32)
sumGoal <- 91

nSumK(integerList,sumGoal)
