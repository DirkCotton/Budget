agebracket <- c("19-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60+")
Median <- c(4600,6750,7450,10120,10830,12020,15020,8540,22370)
Average <- c(7044,10525,13260,15043,17357,15765,25967,17117,35317)
data.df <- data.frame(agebracket, medianDebt,averageDebt)

data.df <- melt(data.df)

p <- ggplot(data.df,aes(x=agebracket,y=value,fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Age Range") + ylab("Debt ($)") +
  #scale_linetype_discrete(name = "") +
  scale_fill_manual(values=c("blue", "red"),labels=c("Median", "Average")) +
  theme_gray() +
  theme_set(theme_gray(base_size = 12)) +
  theme(text=element_text(family="Times")) +
  theme(legend.title=element_blank()) + 
#  theme(legend.position="none") +
  ggtitle(paste("Debt by Age Group\nU.S. Courts Data",sep=""))
print(p)
ggsave("~/desktop/elderdebt.png",dpi=600) 

