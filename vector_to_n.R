vector_to_n <- function(vector, power, sample_size, names){

    #setup data.frame
    vector_n <- data.frame(matrix(NA, nrow=sample_size, ncol=2))
    
    #build data.frame with powers to vector
    for (i in 2:power){
    
        vector_n[[i-1]] <- vector^i
        
        #capture power number to be added to vector name in df
        vec_num_char <- as.character(i)
        
        #provide name to the vector
        names(vector_n[[i-1]]) <- paste(names, vec_num_char, sep="")
    }
    vector_n
}