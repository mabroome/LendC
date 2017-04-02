# The following program reads my Loans from Lending Club data and prepares the data for analysis
# 
# The program is dependant on the following functions
#
# rem_chars     removes non-numeric characters and returns a numeric vector
# conv_yrs      converts a year (Ex. 2004) to the number of years from 2017  

setwd("C:/Users/mbroome/Documents/personal/LendingClub")

# Choose the file to prepare for analysis

myraw.data <- read.csv(file.choose())


# Breakout LendMyData based on Status

myraw.Charge_Off <- myraw.data[myraw.data$Status == "Charged Off",]
myraw.Fully_Paid <- myraw.data[myraw.data$Status == "Fully Paid",]
myraw.Fully_Current <- myraw.data[myraw.data$Status == "Current",]

LendMyData <- rbind(myraw.Charge_Off, myraw.Fully_Paid, myraw.Fully_Current)
# determine the dimensions of the Lending Data copy and estblish an empty data frame

x <- dim(LendMyData)
total_samples <- x[1]
total_features <- x[2]
rd = data.frame(matrix(NA, nrow=x[1], ncol=2))
results = data.frame(matrix(NA, nrow=20))
rnum <- 1

# Loan Status 

h.dat <- c("Charged Off","Fully Paid","Current")

h.ct <- length(h.dat)
for (i in 1:h.ct){
    rd[h.dat[i]]<-as.numeric(LendMyData$Status == h.dat[i])
    new_name <- gsub(' ', "", h.dat[i])
    rd[new_name] <- rd[h.dat[i]]
    temp_name = h.dat[i]
    results$name[rnum] <- temp_name
    results$total[rnum] <- sum(rd[h.dat[i]])
    results$Percent[rnum] <- sum(rd[h.dat[i]])/total_samples
    rnum <- rnum + 1
    }




# Convert "term" of the loan from string to values of 30 and 60, normalize
#The rem_chars will remove all characters and convert to a numeric vector

rd$Term <- rem_chars(LendMyData$Term)
rd$Term <- as.numeric(rd$Term)

# Simple copy of rd that does not require changes but will be normalized

h.dat <- c("annual_inc", "fico_range_low")
h.dat <- c(h.dat, "inq_last_6mths" )
h.ct <- length(h.dat)
for (i in 1:h.ct){
    rd[h.dat[i]] <- LendMyData[h.dat[i]]
    }


#   Establish individual vectors for variables with classifications
#
#   home_ownership
#   purpose
#
#   Classify if home is owned, rented or a mortgage


# Move categorical housing data to rd and normalized data frame
h.dat <- c("RENT","OWN","MORTGAGE")
h.ct <- length(h.dat)
for (i in 1:h.ct){
    rd[h.dat[i]]<-as.numeric(LendMyData$home_ownership == h.dat[i])
    }

# Move categorical grade data to rd 

h.dat <- c("A","B","C","D","E","F","G")

# Remove the number from the grade

LendMyData$Grade <- gsub('[0-9]+', "", LendMyData$Grade)

h.ct <- length(h.dat)
for (i in 1:h.ct){
    rd[h.dat[i]]<-as.numeric(LendMyData$Grade == h.dat[i])
    r
    }


h.dat <- c("car","credit_card","debt_consolidation","educational","home_improvement","house")
h.dat <- c(h.dat,"major_purchase","medical","moving","other","renewable_energy")
h.dat <- c(h.dat,"vacation","wedding","small_business")
for (i in 1:h.ct){
    rd[h.dat[i]]<-as.numeric(LendMyData$purpose == h.dat[i])
        }


# Revolving Credit Line : Convert to character, remove % and then convert"open_acc" to numeric

rd$revol_util <- LendMyData$revol_util
rd$revol_util <- gsub("%", "", rd$revol_util)
rd$revol_util <- as.numeric(rd$revol_util)

# % of revolving credit utilized

rd$revol_util <- as.character(rd$revol_util)
rd$revol_util <- as.numeric(rd$revol_util)


# Clean employment_length ("emp_length")
# Remove the characters from the field
# Identify elemenents with employment under 1 yr and adjust to .5 years
# Normalize em-_length 

rd$emp_length <- rem_chars(LendMyData$emp_length)

# Identify employees less than 1 year
emp_less_symbol <- grep("<", LendMyData$emp_length)

# adjust employment length from 1 to .5
rd$emp_length[emp_less_symbol] <- .5
rd$emp_length[is.na(rd$emp_length)] <- 0

sample_size <- nrow(rd)


#
# Calculate annual % loss when Charged off
# Time Period is a % (funded_amnt - total_pymnt) / funded_amnt = pay_time
# Percentage loss (total_pymnt - funded_amnt) / funded_amnt = percent_loss
# annual_int = pay_time * percent_loss


# Convert character interest rate "xx.xx%" to a numeric
LendMyData$InterestRate <- LendMyData$InterestRate
LendMyData$InterestRate <- gsub("%", "", LendMyData$InterestRate)
LendMyData$InterestRate <- as.numeric(LendMyData$InterestRate)


rd$pay_time <- (rd$funded_amnt - LendMyData$total_pymnt) / rd$funded_amnt
rd$percent_loss <- (LendMyData$total_pymnt - rd$funded_amnt) / rd$funded_amnt
rd$annual_int <- rd$pay_time * rd$percent_loss * 100

# Remove rd$y_int <- 0
rd$y_int <- 0
if (rd$y_int[LendMyData$Status == "Charged Off"]) { <- Y$annual_int } else {
    rd$y_int[LendMyData$Status ] <- LendMyData$InterestRate}

# Remove percent_loss and pay_time as they are not needed
rd$pay_time <- NULL
rd$percent_loss <- NULL

# Remove any random NAs
rd[is.na(rd)] <- 0


