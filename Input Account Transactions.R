# install("lubridate")
library(lubridate)
#
# Input transactions from a CSV file created by Mint.com. Date in Column A converted to Year in Column B and Month in Column C
#


# Input:  
firstyr <- 2015 # reject all transactions prior to this year
mo <- 12 # reject all transactions later than this month

trans <- read.csv("~/desktop/Mint Transactions Year Ending 12-20-2015.csv",stringsAsFactors = FALSE)

# Derive year and month from date in first column
yr <- year(parse_date_time(trans$Date,c('mdY','mdy')))
mnth <- month(parse_date_time(trans$Date,c('mdY','mdy')))
trans$Year <- yr
trans$Month <- mnth

l1 <- length(trans[,1])
cat("Input from file:  ",l1," transactions",sep="")

trans$Original.Description <- lapply(trans$Original.Description, as.character) # convert factor to charater string
trans$Original.Description <- vapply(trans$Original.Description, paste, collapse = ", ", character(1L))
#
# assume Mint.com provides tranasctions from most recent date to oldest
#

# Remove any transactions before input year and month

trans <- trans[trans$Year >= firstyr,]

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

# Warn about Transfers and delete those rows

trans$Category[trans$Original.Description == "Transfer to High Yield Checking -5206"] <- "Ali Support" # pull out Ali transfers
trans$Category[trans$Original.Description == "Transfer to High Yield Checking -7570"] <- "Eric Expenses" # pull out Eric transfers

cat("\n\n*** Edit Transfers in Excel transactions file. All Transfers will be ignored.\n\n")
trans <- trans[trans$Category != "Transfer",]
l7 <- length(trans[,1])
cat("\nRemoved from transactions:  ",l6-l7," Transfer transactions leaving ",l7," transactions",sep="")

# Total spend this period

cat("\n\nTotal amount spent during selected time period is ",sum(trans$Amount),sep="")

# Separate State Farm Ins. payments from State Farm Bank Car Payments
cat("\n\n*** Change all State Farm Bank Ins payments from Auto Payment to Insurance: HO, Car, Umbrella in Excel Transactions spreadsheet")
trans$Category[substr(trans$Original.Description,1,14) == "STATE FARM INS" | substr(trans$Original.Description,1,13) == "STATE FARM RO"] <- "Insurance: HO, Car, Umbrella"

#
# How much spent on Eric?  Schwab?

#Remove all checks from tran data frame
trans <- trans[trans$Category != "Check",]

#add check transactions from edited check file
chks.df <- read.csv("Checks 2015 Edited.csv",header=TRUE)
chks.df <- subset(chks.df,select=c(1,2,3,4,5,6,7,8,9,10,11))
trans <- rbind(trans,chks.df)


# Input the checks from "Checks 2015 from R.csv" edited in Excel and written to "Checks 2015 Edited.csv" to incorporate Description and Category



# Read valid expense categories
cats <- read.csv("~/desktop/Budget/Categories.csv",stringsAsFactors = FALSE,header=FALSE)

# Change problematic categories
trans$Category[trans$Category == "Chase Epay"] <- "Credit Card Payment"
trans$Category[trans$Category == "Cash"] <- "Cash & ATM"
trans$Category[trans$Category == "Coffee Shops"] <- "Restaurants"
trans$Category[trans$Category == "Donation"] <- "Charity"
trans$Category[trans$Category == "Eric Expenses"] <- "Eric Support"
trans$Category[trans$Category == "Home Repairs"] <- "Home Repair"
trans$Category[trans$Category == "Hair"] <- "Personal Care"
trans$Category[trans$Category == "Leisure"] <- "Entertainment"
trans$Category[trans$Category == "Amusement"] <- "Entertainment"
trans$Category[trans$Category == "Exterminator"] <- "Home:Exterminator"
trans$Category[trans$Category == "Business Services"] <- "JDC Planning"
trans$Category[trans$Category == "Books"] <- "Reading"
trans$Category[trans$Category == "Fast Food"] <- "Groceries"
trans$Category[trans$Category == "Food and Dining"] <- "Restaurants"
trans$Category[trans$Category == "Printing"] <- "Office Supplies"
trans$Category[trans$Category == "Sports"] <- "Entertainment"
trans$Category[trans$Category == "Hobbies"] <- "Entertainment"
trans$Category[trans$Category == "Hotel"] <- "Vacation"
trans$Category[trans$Category == "Hide from Budgets & Trends"] <- "Transfers"
trans$Category[trans$Category == "Pet Food & Supplies"] <- "Misc"
trans$Category[trans$Category == "Fees & Charges"] <- "Bank Fee"
trans$Category[trans$Category == "Health & Fitness"] <- "Gym"
trans$Category[trans$Category == "Sporting Goods"] <- "Entertainment"
trans$Category[trans$Category == "Parking"] <- "Service & Parts"
trans$Category[trans$Category == "Eric Living Expenses"] <- "Eric Support"
trans$Category[trans$Category == "Food & Dining"] <- "Restaurants"
trans$Category[trans$Category == "Travel"] <- "Vacation"
trans$Category[trans$Category == "Exterminator"] <- "Home:Exterminator"
trans$Category[trans$Category == "Internet"] <- "CableTV/Internet"
trans$Category[trans$Category == "Gifts & Donations"] <- "Charity"
trans$Category[trans$Category == "Home:Exterminator"] <- "Household:Exterminator"
trans$Category[trans$Category == "Ali Misc"] <- "Ali Support"
trans$Category[trans$Category == "JDC PLANNING"] <- "JDC Planning"

unresol <- trans[is.na(match(tolower(trans$Category),tolower(cats$V1))),]
print("*** Unresolved categories written to Unresolved.csv")
print(unresol[,c(1,2,4,6)])

write.csv(unresol,"Unresolved.csv")
# How much spent on Alex?

alex <- trans[trans$Category == "Ali Rent" | trans$Category == "Ali Support" | trans$Category == "Ali Rent" | trans$Category == "Ali Water Bill"  | trans$Category == "Ali Parking"  |trans$Category == "Ali Education" ,]
alex$Original.Description <- vapply(alex$Original.Description, paste, collapse = ", ", character(1L))
# trans <- trans[trans$Account.Name != "Chase VISA" & trans$Account.Name != "COLLEGE CARD",]
l8 <- length(alex[,1])
cat("\nCopied from transactions to alex.csv:  ",l8," transactions leaving ",length(trans[,1])," transactions",sep="")
trans$Category[trans$Account.Name == "Chase VISA" | trans$Account.Name == "COLLEGE CARD" | trans$Account.Name == "PLATINUM CARD"] <- "Ali Support"
# write.csv(alex,"alex.csv",row.names = FALSE)

# How much spent on Eric?

eric <- trans[trans$Category == "Eric Support" | trans$Category == "Eric Rent" ,]
eric$Original.Description <- vapply(eric$Original.Description, paste, collapse = ", ", character(1L))
l9 <- length(eric[,1])
cat("\nRemoved from transactions to eric.csv:  ",l9," transactions leaving ",length(trans[,1])," transactions\n\n",sep="")
trans$Category[trans$Account.Name == "BankAmericard Cash Rewards Signature Visa"] <- "Eric Support"
# write.csv(eric,"eric.csv",row.names = FALSE)

print(" ")
print(paste("Total Alex Support:  ",sum(trans$Amount[substr(trans$Category,1,3) == "Ali" ]),sep=""))
print(paste("   Alex Support:  ",sum(trans$Amount[trans$Category == "Ali Support" ]),sep=""))
print(paste("   Alex Rent:  ",sum(trans$Amount[trans$Category == "Ali Rent" ]),sep=""))
print(paste("   Alex Education:  ",sum(trans$Amount[trans$Category == "Ali Education" ]),sep=""))
print(paste("   Alex Parking:  ",sum(trans$Amount[trans$Category == "Ali Parking" ]),sep=""))
print(paste("   Alex Water Bill:  ",sum(trans$Amount[trans$Category == "Ali Water Bill" ]),sep=""))
print(paste("   Alex Other:  ",round(sum(trans$Amount[substr(trans$Category,1,3) == "Ali" ]) - sum(trans$Amount[trans$Category == "Ali Support" ]) - sum(trans$Amount[trans$Category == "Ali Rent" ]) - sum(trans$Amount[trans$Category == "Ali Education" ]) - sum(trans$Amount[trans$Category == "Ali Parking"]) - sum(trans$Amount[trans$Category == "Ali Water Bill"])),sep=""))


print(" ")
print(paste("Total Eric Support:  ",sum(trans$Amount[substr(trans$Category,1,4) == "Eric" ]),sep=""))
print(paste("   Eric Support:  ",sum(trans$Amount[trans$Category == "Eric Support" ]),sep=""))
print(paste("   Eric Rent:  ",sum(trans$Amount[trans$Category == "Eric Rent" ]),sep=""))
print(paste("   Eric Other:  ",sum(trans$Amount[substr(trans$Category,1,4) == "Eric" ]) - sum(trans$Amount[trans$Category == "Eric Support" ]) - sum(trans$Amount[trans$Category == "Eric Rent" ]),sep=""))


# compare all spending by category to budget

# input cumulative monthly budget
### all numbers should be without commas, $ and should have 0 instead of blank

budget <- read.csv("2015 Cum Monthly Budget.csv",stringsAsFactors = FALSE)
budget
colsum <- aggregate(trans$Amount ~ trans$Category, data = trans, FUN = sum)

names(colsum) <- c("Category","Monthly Budget YTD")

#build monthly budget data frame
budgetmo <- data.frame(budget[,1],budget[,mo+1])
names(budgetmo) <- c("Category","Amount")

# build dataframe of expenses, YTD budget and delta
spndvbudget <- merge(budgetmo,colsum,all=TRUE)
spndvbudget[,2][is.na(spndvbudget[,2])] <- 0 # convert NA to zero
spndvbudget[,3][is.na(spndvbudget[,3])] <- 0 # convert NA to zero
spndvbudget$Delta <- spndvbudget$Amount - spndvbudget$`Monthly Budget YTD` 
names(spndvbudget) <- c("Category","Monthly Cum Budget","Spending","Delta")

write.csv(spndvbudget[order(spndvbudget[,1]),],"spndvbudget.csv",row.names=FALSE)

write.csv(trans,"Processed Mint Transactions from R.csv",row.names=FALSE)

