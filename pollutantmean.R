# R programming Coursera Programming assignment 1a: Pollutant mean function
# Author: Samir P Bhagwat
pollutantmean <- function(directory,pollutant, id = 1:332) {
        ## make a list of all the files and store them in a character vector, set full name as True        
        dr <- list.files(directory,full.names = TRUE)
        # Initiate the data frame null
        d <- data.frame() 
        #create dataframe from appropriate files
        for (i in 1: length(id)) {
                d <- rbind(d,read.csv(dr[id[i]]))   
        }
        if (pollutant == "nitrate") {
                answer <- mean(d$nitrate,na.rm = TRUE)
        }
        if (pollutant == "sulfate") {
                answer <- mean(d$sulfate,na.rm = TRUE)
        }
        #Return answer
        answer
}