rem_chars <- function(x){
    
    #remove all the characters and return as a numeric vector
    x <- gsub("[^0-9]", "", x)
    x <- as.numeric(x)
    
}