#R programming Coursera Programming assignment 1c: determine the correlation between nitrate and sulfate
# if the complete cases of records in that file is greater than a certain threshold.
# Author: Samir P Bhagwat
# function is defined below.
corr <- function(directory,threshold = 0) {
        # store all the files of the directory in a list.
        dr <- list.files(directory,full.names = TRUE) 
        cr <- numeric() # initiate a vector to store correlation values
        tr <- is.vector(x = .Machine$integer.max ) # initiate vector to store complete.cases values of a file
        for ( i in 1: 332) {
               fil <- read.csv(dr[i])
               tr[i] <- sum(complete.cases(fil))  
               # compare complete.cases value to the threshold
            if (tr[i] > threshold)  {
             cr<- rbind(cr,cor(fil[,"sulfate"],fil[,"nitrate"],use = "complete.obs"))
                   }
            
        } 
cr    # return correlation values vector
}

#Question 8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#Question 9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#Question 10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))