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
