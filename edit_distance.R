## The edit distance between two strings refers to the minimum number of character insertions, deletions, and substitutions required to change one string to the other.
## For example, the edit distance between “kitten” and “sitting” is three: substitute the “k” for “s”, substitute the “e” for “i”, and append a “g”.

## Given two strings, compute the edit distance between them.
library(dplyr)

editDistance <- function(string1,string2) {

string1_chars <- nchar(string1)
string1_expanded <- character(string1_chars)

for (i in 1:string1_chars) {

  string1_expanded[i] <- substr(string1,i,i)

}
string1 <- data.frame(string1 =matrix(unlist(string1_expanded), nrow=length(string1_expanded), byrow=TRUE),stringsAsFactors=FALSE)

## now string2
string2_chars <- nchar(string2)
string2_expanded <- character(string2_chars)

for (i in 1:string2_chars) {

  string2_expanded[i] <- substr(string2,i,i)

}
string2 <- data.frame(string2 =matrix(unlist(string2_expanded), nrow=length(string2_expanded), byrow=TRUE),stringsAsFactors=FALSE)

## create joining bit
string1$row_number <- seq(1,nrow(string1),1)
string2$row_number <- seq(1,nrow(string2),1)

## if statement to identify
if (nrow(string1) > nrow(string2)) {finalDF <- string1 %>% left_join(string2,by='row_number')}
if (nrow(string2) > nrow(string1)) {finalDF <- string2 %>% left_join(string1,by='row_number')}
else {finalDF <- string1 %>% left_join(string2,by='row_number')}

##
finalDF$binary <- ifelse(finalDF$string1 == finalDF$string2,0,1)
finalDF$binary <- ifelse(is.na(finalDF$binary) == TRUE,1,finalDF$binary)
## reform
finalDF <- finalDF %>% select(string1,string2,binary)

## edit distance calculation
editDistCalc <- sum(finalDF$binary)

return(editDistCalc)
}
editDistance('test','tester') ## should return '2'
editDistance('josh','josh') ## should return '0'
editDistance('bingo','bango') ## should return '1'
