# Coursera JH Data Science - R Programming - Week 4 - Assignment 3 

# Acquire data from csv file
# Change working directory with setwd if needed

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


data_new <- data[, c(2, 7, 11, 17, 23)]


best <- function(state, outcome) {
        
        data_state <- data_new[which(data_new[, 2] == state), ]
        
        if (all(state != data_new[, 2])) {
                stop("invalid state")
        }
        
        # Select the outcome
        
        if (outcome == "heart attack") {
                data_updated <- data_state[, 1:3]
        } else if (outcome == "heart failure") {
                data_updated <- data_state[, c(1, 2, 4)]
        } else if (outcome == "pneumonia") {
                data_updated <- data_state[, c(1, 2, 5)]
        } else {
                stop("invalid outcome")
        }
        
        # Convert the outcome column to numeric type
        
        data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
        
        # Delete NAs from the outcome column
        
        non_na_indices <- which(!is.na(data_updated[, 3]))
        data_updated <- data_updated[non_na_indices, ]
        
        
        data_ordered <- data_updated[with(data_updated, order(data_updated[, 3], data_updated[, 1])), ]
        
       
        
        data_ordered[1, 1]
        
}


# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state.

