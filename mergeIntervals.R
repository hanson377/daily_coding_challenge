# Given a list of possibly overlapping intervals,
# return a new list of intervals where all overlapping intervals have been merged.

# The input list is not necessarily ordered in any way.
#For example, given [(1, 3), (5, 8), (4, 10), (20, 25)], you should return [(1, 3), (4, 10), (20, 25)].

library(stringr)
library(dplyr)

list <- c('[1,3]','[5,8]','[4,10]','[20,25]')


mergeIntervals <- function(list_of_intervals) {


intervals <- data.frame(value = NA, interval = NA)

for (i in list_of_intervals) {

  interval_i <- list_of_intervals[i]

  lower1 <- as.numeric(regexpr("\\[", interval_i)[1])
  lower2 <- as.numeric(regexpr(",", interval_i)[1])

  upper1 <- as.numeric(regexpr(",", interval_i)[1])
  upper2 <- as.numeric(regexpr("\\]", interval_i)[1])

  lower_limit <- as.integer(substr(interval_i,lower1+1,lower2-1))
  upper_limit <- as.integer(substr(interval_i,upper1+1,upper2-1))

  df <- data.frame(value = seq(from=lower_limit,to=upper_limit,by=1), interval = i)

  intervals <- rbind(intervals,df)

}
return(intervals)
}
mergeIntervals(list)


interval_i <- list[3]

lower1 <- as.numeric(regexpr("\\[", interval_i)[1])
lower2 <- as.numeric(regexpr(",", interval_i)[1])

upper1 <- as.numeric(regexpr(",", interval_i)[1])
upper2 <- as.numeric(regexpr("\\]", interval_i)[1])

lower_limit <- as.integer(substr(interval_i,lower1+1,lower2-1))
upper_limit <- as.integer(substr(interval_i,upper1+1,upper2-1))

df <- data.frame(value = seq(lower_limit,upper_limit,1), interval = i)



intervalDf <- data.frame(value = seq(lower_limit,upper_limit,1))
