# The following program reads Lending Club data and prepares the data for analysis
# 
# The program is dependant on the following functions
#
# rem_chars     removes non-numeric characters and returns a numeric vector
# conv_yrs      converts a year (Ex. 2004) to the number of years from 2017  

setwd("C:/Users/mbroome/Documents/personal/LendingClub")

# Choose the file to prepare for analysis

raw.data <- read.csv(file.choose())

# Choose the number of rows and columns for analysis (Remove after testing)

# raw.data <- raw.data[26262:27272,]

# Remove irrelvant rows in this case when no loan id
# Select the first row "id" and if na remove these rows

# LendData <- LendData[complete.cases(LendData$id),]

# Breakout LendData based on loan_status

raw.Charge_Off <- raw.data[raw.data$loan_status == "Charged Off",]
raw.Fully_Paid <- raw.data[raw.data$loan_status == "Fully Paid",]

# Select Lending Data to build out workdata

LendData <- rbind(raw.Charge_Off, raw.Fully_Paid)

# determine the dimensions of the Lending Data copy and estblish an empty data frame

x <- dim(LendData)
total_samples <- x[1]
total_features <- x[2]
workdata = data.frame(matrix(NA, nrow=x[1], ncol=2))
Y = data.frame(matrix(NA, nrow=total_samples, ncol=0))

# Esablish a random number for each sample

LendData$random <- sample(1:total_samples, total_samples, replace=FALSE)

# Sort the samples to ensure rows are random 

LendData <- LendData[order(LendData$random),]


# Set a second data frame that will be a normalized for regression analysis
norm = data.frame(matrix(NA, nrow=total_samples, ncol=2))
norm_mean = data.frame(matrix(NA, nrow=1, ncol=2))
norm_std = data.frame(matrix(NA, nrow=1, ncol=2))


# build new data frames from LendData will all data in single file
# 
#   workdata (not normalized)


# determine the dimensions of the Lending Data copy and estblish an empty data frame

# Convert "term" of the loan from string to values of 30 and 60, normalize
#The rem_chars will remove all characters and convert to a numeric vector

workdata$term <- rem_chars(LendData$term)

# Simple copy of workdata that does not require changes but will be normalized

h.dat <- c("funded_amnt", "annual_inc", "fico_range_low", "open_acc", "pub_rec", "dti")
h.dat <- c(h.dat, "inq_last_6mths", "delinq_2yrs" )
h.ct <- length(h.dat)
for (i in 1:h.ct){
    Ltemp <- LendData[h.dat[i]]
    Ltemp[is.na(LendData[h.dat[i]])] <- 0
    workdata[h.dat[i]] <- Ltemp
    norm[h.dat[i]]<-workdata[h.dat[i]]
    norm_results <- norm_vector(workdata[[h.dat[i]]])
    norm[h.dat[i]] <- as.vector(norm_results$vector)
    norm_mean[h.dat[i]] <- norm_results$mean
    norm_std[h.dat[i]] <- norm_results$std
}

# remove dummy columns from data frames
norm_std$X1 <- NULL
norm_std$X2 <- NULL
norm$X1 <- NULL
norm$X2 <- NULL
norm_mean$X1 <- NULL
norm_mean$X2 <- NULL
workdata$X1 <- NULL
workdata$X2 <- NULL

#   Establish individual vectors for variables with classifications
#
#   home_ownership
#   purpose
#
#   Classify if home is owned, rented or a mortgage


# Move categorical housing data to workdata and normalized data frame
h.dat <- c("RENT","OWN","MORTGAGE")
h.ct <- length(h.dat)
for (i in 1:h.ct){
    workdata[h.dat[i]]<-as.numeric(LendData$home_ownership == h.dat[i])
    norm[h.dat[i]]<-workdata[h.dat[i]]
}

# Move categorical grade data to workdata and normalized data frame

h.dat <- c("A","B","C","D","E","F","G")
h.ct <- length(h.dat)
for (i in 1:h.ct){
    workdata[h.dat[i]]<-as.numeric(LendData$grade == h.dat[i])
    norm[h.dat[i]]<-workdata[h.dat[i]]
}


h.dat <- c("car","credit_card","debt_consolidation","educational","home_improvement","house")
h.dat <- c(h.dat,"major_purchase","medical","moving","other","renewable_energy")
h.dat <- c(h.dat,"vacation","wedding","small_business")
h.ct <- length(h.dat)
for (i in 1:h.ct){
    workdata[h.dat[i]]<-as.numeric(LendData$purpose == h.dat[i])
    norm[h.dat[i]]<-workdata[h.dat[i]]
    }

# Earliest Credit Line
# Convert credit line from factor to string

workdata$earliest_cr_line <- as.character(LendData$earliest_cr_line)
workdata$earliest_cr_line <-rem_chars(workdata$earliest_cr_line)
workdata$earliest_cr_line <- as.numeric(workdata$earliest_cr_line)
workdata$earliest_cr_line <- conv_yrs(workdata$earliest_cr_line)

# Revolving Credit Line : Convert to character, remove % and then convert"open_acc" to numeric

workdata$revol_util <- LendData$revol_util
workdata$revol_util <- gsub("%", "", workdata$revol_util)
workdata$revol_util <- as.numeric(workdata$revol_util)

# % of revolving credit utilized

workdata$revol_util <- as.character(workdata$revol_util)
workdata$revol_util <- as.numeric(workdata$revol_util)

# add to power of N for select vectors
# power sets the Nth power
# workdata_names allows for naming the new vectors

# Clean employment_length ("emp_length")
# Remove the characters from the field
# Identify elemenents with employment under 1 yr and adjust to .5 years
# Normalize em-_length 

workdata$emp_length <- rem_chars(LendData$emp_length)

# Identify employees less than 1 year
emp_less_symbol <- grep("<", LendData$emp_length)

# adjust employment length from 1 to .5
workdata$emp_length[emp_less_symbol] <- .5
workdata$emp_length[is.na(workdata$emp_length)] <- 0

sample_size <- nrow(workdata)


#
# Calculate annual % loss when Charged off
# Time Period is a % (funded_amnt - total_pymnt) / funded_amnt = pay_time
# Percentage loss (total_pymnt - funded_amnt) / funded_amnt = percent_loss
# annual_int = pay_time * percent_loss


# Convert character interest rate "xx.xx%" to a numeric
LendData$int_rate <- LendData$int_rate
LendData$int_rate <- gsub("%", "", LendData$int_rate)
LendData$int_rate <- as.numeric(LendData$int_rate)


workdata$pay_time <- (workdata$funded_amnt - LendData$total_pymnt) / workdata$funded_amnt
workdata$percent_loss <- (LendData$total_pymnt - workdata$funded_amnt) / workdata$funded_amnt
workdata$annual_int <- workdata$pay_time * workdata$percent_loss * 100

# Remove workdata$y_int <- 0
workdata$y_int <- 0
workdata$y_int[LendData$loan_status == "Charged Off"] <- workdata$annual_int
workdata$y_int[LendData$loan_status == "Fully Paid"] <- LendData$int_rate

# Remove percent_loss and pay_time as they are not needed
workdata$pay_time <- NULL
workdata$percent_loss <- NULL


# remove annual_int from workdata as no longer needed
# copy workdata$y_int to Y$y_int

# Establish predictors Y = Charge off (1 yes, 0 no)
# Establish separate df for Y

workdata$y_default <- 0
workdata$y_default[LendData$loan_status == "Charged Off"] <- 1

# Remove any random NAs
workdata[is.na(workdata)] <- 0
norm[is.na(norm)] <- 0

