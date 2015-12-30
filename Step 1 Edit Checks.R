# Write all checks to a separate file so they can be edited to include Decsription and Category
trans <- read.csv("~/desktop/Mint Transactions Year Ending 12-20-2015.csv",stringsAsFactors = FALSE)
write.csv(trans[trans$Category == "Check",],"Checks 2015 from R.csv",row.names = FALSE)

### Run Input Account Transactions.R next 