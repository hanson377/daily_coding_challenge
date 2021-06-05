# Given a array of numbers representing the stock prices of a company in chronological order,
# write a function that calculates the maximum profit you could have made from buying and selling that stock once.
# You must buy before you can sell it.

# For example, given [9, 11, 8, 5, 7, 10], you should return 5, since you could buy the stock at 5 dollars and sell it at 10 dollars.

library(dplyr)

maxProfit <- function(numArray) {

 df <- data.frame(value = numArray)
 df <- df %>% mutate(order = row_number(), binary = 1)

 finalDf <- NA

 for (i in 1:max(df$order)) {

   tempDf <- df %>% filter(order == i) %>% select(value1 = value,binary)
   joinDf <- df %>% filter(order > i) %>% select(value2 = value,binary)

   tempDf <- tempDf %>% inner_join(joinDf, by='binary')
   finalDf <- rbind(tempDf,finalDf)

 }
 finalDf <- finalDf %>% mutate(diff = value2-value1) %>% mutate(profit_rank = rank(desc(diff)))
 finalDf <- finalDf %>% filter(profit_rank == 1) %>% select(value = value1)

 maxProfit <- max(finalDf$value)

 return(maxProfit)

}

array <- c(11, 10, 8, 5, 7, 5)
maxProfit(array)
