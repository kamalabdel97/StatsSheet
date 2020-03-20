library(ggplot2)
library(tidyverse)


StoreA <- rnorm(365, 4554.22, 332) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreB <- rnorm(365, 4387.76, 140) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332
StoreC <- rnorm(365, 4375.12, 664) # Creates 365 samples of occurences, with a mean of 3000.12 and standard deviation of 664

Revenue <- c(StoreA, StoreB, StoreC) #Combines values into one column. All 365 values of Store A will be combined along with all 365 values of Store B

Store <- rep(c("Store A", "Store B", "Store C"), each = 365) #Creates of column of Store A and Store B as the two levels. 365 iterations will be created for Store A and then Store \ 

Sales <- data.frame(Store, Revenue) #Creates a dataframe of the Revenue and their corresponding store 

ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Store)) + 
  stat_summary(fun.y = "mean", geom = "bar") + #
	 stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=6, vjust = -0.5) + #Plots the text of the average value for each level
  theme(panel.background=element_blank()) + #Creates a blank background for the plot's panel
  theme(plot.background=element_blank()) + #Creates a blank background for the plot 
  theme(legend.position="none") + # Gets rid of the legend
  labs(title = "Average Revenue per day Between Store A and Store B", #Adds a title for the plot
     x = NULL, y = "Average Revenue" #Labels for the x and y axis
     ) 


write.table(Sales, file="Kamal.csv", sep = ',')


bp_groups <- ggplot(Sales, aes(x= Store, y = Revenue))+ geom_boxplot()
bp_groups

bp_overall <- ggplot(Sales, aes(x = '', y = Revenue)) + geom_boxplot()
bp_overall


oneway.test(Revenue~Store, var.equal = FALSE) #oneway Anova test


aov.out = aov (Revenue~Store, data = Sales) # this allows to store the output of the ANOVA test, and use extraction function later on
summary(aov.out)

#Calculating paired T-test 3 times


