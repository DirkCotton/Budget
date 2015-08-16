#
# Input transactions from a CSV file created by Mint.com. Date in Column A converted to Year in Column B and Month in Column C
#

# Input:  
yr <- 2015 # reject all transactions prior to this year
mo <- 8 # reject all transactions prior to this month

trans <- read.csv("transactions.csv")

trans$Original.Description <- lapply(trans$Original.Description, as.character) # convert factor to charater string
#
# assume Mint.com provides tranasctions from most recent date to oldest
#

# Convert to data frame
trans <- data.frame(trans)

# Select only accounts of interest

include.accts <- c("BankAmericard Cash Rewards Signature Visa","Chase VISA","COLLEGE CARD","Costco AmEx","Dirks Rev Trust","Eric", "Fidelity Investments Investment Rewards Signature Visa","James D Cotton, UA 12 09 1999 James Dirk Cotton Revocable Trust Trust Brokerage Account","JDC Planning","Schwab","Vickis Rev Trust","Wells Fargo Checking Acct")
y <- trans[trans$Account.Name %in% include.accts,]

### add a warning for rejected account names!!!!!!!!!!!!

## separate credit card payments into another vector, remove from trans

# cc <- trans[(trans$Category="Credit Card Payment"),]
#trans <- trans[!(trans$Category=="Credit Card Payment"),]

cc <- trans[trans$Category == "Credit Card Payment",]
trans <- trans[trans$Category != "Credit Card Payment",]



