norm_vector <- function (x) {

    # normalizes vector of values (x - mean)/Std_Deviation
    # input = vector of x
    # returns a list of norm_vector, mean & std deviation
    mean_x <- mean(x, na.rm=TRUE)
    std_x <- sd(x, na.rm=TRUE)
    z <- (x - mean_x)/std_x
    
    # try to return a list of the normalized vector, mean and Standard deviation
    
    ret <- list(vector=z, mean=mean_x, std=std_x)
    
    }
    
