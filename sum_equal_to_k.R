#Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
#For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.

sumEqualToK <- function(list_of_numbers,k) {

possibleCombos <- data.frame(value1 = NA,value2 = NA)
listLength <- length(list_of_numbers)

for (i in list_of_numbers) {

column1 <- rep(i,times=listLength)
column2 <- list_of_numbers

temp <- data.frame(value1 = column1,value2 = column2)

possibleCombos <- rbind(possibleCombos,temp)
}
possibleCombos <- subset(possibleCombos,is.na(value1) == FALSE)
possibleCombos$sum_of_numbers <- possibleCombos$value1+possibleCombos$value2

possibleCombos$binary <- possibleCombos$sum_of_numbers == k
possibleCombos$binary <- ifelse(possibleCombos$binary == TRUE, 1, 0)

if (max(possibleCombos$binary) == 1) {return(TRUE)}
else (return(FALSE))
}


## TEST FUNCTION
list1 <- c(1,2)
list2 <- c(22,89,3,111,3,9,10,11)
list3 <- c(1,3,67,55,34,92,11)
list4 <- c(3,5,345,4,5,6)
list5 <- c(1,2,8,9,10,35,45)

sumEqualToK(list1,3) ## should be true
sumEqualToK(list2,11) ## should be false
sumEqualToK(list3,3) ## should be false
sumEqualToK(list4,349) ## should be true
sumEqualToK(list5,10) ## should be true
