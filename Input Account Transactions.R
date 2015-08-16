#
# Input transactions from a CSV file created by Mint.com. Date in Column A converted to Year in Column B and Month in Column C
#

# Input:  
yr <- 2015 # reject all transactions prior to this year
mo <- 7 # reject all transactions later than this month

trans <- read.csv("transactions.csv")
l1 <- length(trans[,1])
cat("Input from file:  ",l1," transactions",sep="")

trans$Original.Description <- lapply(trans$Original.Description, as.character) # convert factor to charater string
#
# assume Mint.com provides tranasctions from most recent date to oldest
#

# Remove any transactions before input year and month

trans <- trans[trans$Year >= yr,]

l6 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l1-l6," transactions ouside year range leaving ",l6," transactions",sep="")

trans <- trans[trans$Month <= mo,]
l2 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l6-l2," transactions ouside month range leaving ",l2," transactions",sep="")

# Convert to data frame
trans <- data.frame(trans)

# Select only accounts of interest

include.accts <- c("BankAmericard Cash Rewards Signature Visa","Chase VISA","COLLEGE CARD","Costco AmEx","Dirks Rev Trust","Eric", "Fidelity Investments Investment Rewards Signature Visa","James D Cotton, UA 12 09 1999 James Dirk Cotton Revocable Trust Trust Brokerage Account","JDC Planning","Schwab","Vickis Rev Trust","Wells Fargo Checking Acct")
trans <- trans[trans$Account.Name %in% include.accts,]
l3 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l2-l3," uninteresting account names transactions leaving ",l3," transactions",sep="")


### add a warning for rejected account names!!!!!!!!!!!!

## separate credit card payments into another vector, remove from trans

# cc <- trans[(trans$Category="Credit Card Payment"),]
#trans <- trans[!(trans$Category=="Credit Card Payment"),]

cc <- trans[trans$Category == "Credit Card Payment",]
trans <- trans[trans$Category != "Credit Card Payment",]
l4 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l3-l4," redundant credit card payments leaving ",l4," transactions",sep="")

#
# Remove credits to a separate vector
#
credits <- trans[trans$Transaction.Type == "credit",]
trans <- trans[trans$Transaction.Type != "credit",]
l5 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l4-l5," credits to account ",l5," transactions",sep="")


# Remove any transactions less than $1

trans <- trans[trans$Amount > 1,]
l6 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l5-l6," transactions less than $1 leaving ",l6," transactions",sep="")



