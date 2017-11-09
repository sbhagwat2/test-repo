#R programming Coursera Programming assignment 1b: Pollutant mean function
# Author: Samir P Bhagwat
#if I use rbind function, then the column names in the final data frames loses the assigned columnnames 
# they are automatically assigned. 
complete <- function(directory,id = 1:332) {
        ## make a list of all the files and store them in a character vector, set full name as True        
        dr <- list.files(directory,full.names = TRUE)
        ## make the data frame with two required columns
        d <- data.frame(id = integer(),nobs = integer())
        for (i in 1: length(id)) {
               fil <- read.csv(dr[id[i]])
               num <- sum(complete.cases(fil))
               r <- c(id[i],num)
               d <- rbind(d,r)
        }
        d
}

#Alternate implementation of data frame formation without the rbind function is shown below:
#R programming Coursera Programming assignment 1b: Pollutant mean function. This implementation preserves 
#proper column names
# Author: Samir P Bhagwat
complete <- function(directory,id = 1:332) {
        ## make a list of all the files and store them in a character vector, set full name as True        
        dr <- list.files(directory,full.names = TRUE)
        ## make the data frame with two required columns
        d <- data.frame(id = integer(),nobs = integer())
        for (i in 1: length(id)) {
                fil <- read.csv(dr[id[i]])
                r <- c(id[i],sum(complete.cases(fil)))
                d[i,] <- r
        }
        d
}


