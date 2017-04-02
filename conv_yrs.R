conv_yrs <- function(x){
    # takes the 2-digit year and converts to years from 2017
    # avoid the issues of any NA's by converting to 17
    x[is.na(x)] <- 16.5
    after_99 <- x <= 17 & x >= 0
    before_00 <- !after_99
    x <- after_99 * (17 - x) + before_00 * (117 - x)
        }
    