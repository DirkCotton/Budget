yearnum <- c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
filings <- c(1300000,1500000,1550000,1600000,1450000,1400000,650000,800000,1200000,1538000,1417326,1219132,1072807,935420,805000)
data.df <- data.frame(yearnum, filings)
p <- ggplot(data.df, aes(x=yearnum,y=filings)) +
  geom_bar(stat="identity",colour = "red", fill = "red", width = 0.5) +
  xlab("Year") + ylab("Number of Bankruptcies") +
  #scale_linetype_discrete(name = "") +
  theme_gray() +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  theme(legend.title=element_blank()) + 
  theme(legend.position="none") +
  ggtitle(paste("Number of Consumer Bankruptcy Filings by Year\nU.S. Courts Data",sep="") )
print(p)
ggsave("~/desktop/bankruptcies.png",dpi=600) 