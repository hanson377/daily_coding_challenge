## given a string of round, curly, and square open and closing brackets,
## return whether the brackets are balanced (well-formed)

## for example, given the string "([])[]([])", you should return true
## given the string "([})", you should return false

matchChar <- function(character1,character2){

if (character1 == '{' & character2 == '}') {return(0)}
if (character1 == '{' & character2 != '}') {return(1)}

if (character1 == '}' & character2 == '{') {return(0)}
if (character1 == '}' & character2 != '{') {return(1)}

if (character1 == '(' & character2 == ')') {return(0)}
if (character1 == '(' & character2 != ')') {return(1)}

if (character1 == ')' & character2 == '(') {return(0)}
if (character1 == ')' & character2 != '(') {return(1)}

if (character1 == '[' & character2 == ']') {return(0)}
if (character1 == '[' & character2 != ']') {return(1)}

if (character1 == ']' & character2 == '[') {return(0)}
if (character1 == ']' & character2 != '[') {return(1)}
else {return(1)}

}

balancedString <- function(string) {

charCount <- nchar(string)
midwayPoint <- charCount/2

if (charCount/2 != round(charCount/2,digits=0)) {return(FALSE)} ## if length of string is odd, string will always be unbalanced, so return FALSE

else {

firstHalf <- substr(string,1,midwayPoint)
secondHalf <- substr(string,midwayPoint+1,charCount)

length <- nchar(firstHalf)

column1 <- character(length)
column2 <- character(length)

for (i in 1:length) {
  column1[i] <- substr(firstHalf,i,i)
  column2[i] <- substr(secondHalf,length-i+1,length-i+1)
}

column1 <- data.frame(firstHalf = matrix(unlist(column1), nrow=length(column1), byrow=TRUE))
column2 <- data.frame(secondHalf = matrix(unlist(column2), nrow=length(column2), byrow=TRUE))

df <- data.frame(column1,column2)

## now run logical test on each row of the dataframe generated immediatley above
result <- numeric(nrow(df))

for (i in 1:nrow(df)) {
  result[i] <- matchChar(df[[i,1]],df[[i,2]])
}

if (max(result) == 0) {return(TRUE)}
if (max(result) == 1) {return(FALSE)}
}
}


balancedString("([])[]([])")
