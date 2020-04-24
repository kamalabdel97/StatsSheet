set.seed(1)

library(ggplot2)
library(dplyr)

StoreA <- rnorm(365, 5000.21, 332) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreB <- rnorm(365, 1000.12, 332) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332

Revenue <- c(StoreA, StoreB) #Combines values into one column. All 365 values of Store A will be combined along with all 365 values of Store B

Store <- rep(c("Store A", "Store B"), each = 365) #Creates of column of Store A and Store B as the two levels. 365 iterations will be created for Store A and then Store \ 

Sales <- data.frame(Store, Revenue) #Creates a dataframe of the Revenue and their corresponding store 

TTestplot <- ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Store)) + 
	stat_summary(fun.y = "mean", geom = "bar") + #
	stat_summary(aes(label=round(..y..,2)), 
		fun.y=mean, geom="text", size=3.5, vjust = -0.5) + #Plots the text of the average value for each level
	theme(panel.background=element_blank()) + #Creates a blank background for the plot's panel
	theme(plot.background=element_blank()) + #Creates a blank background for the plot 
	theme(plot.title = element_text(hjust = 0.5))+ #Centers plot title
	theme(legend.position="none") + # Gets rid of the legend
	labs(title = "Average Revenue per day Between Store A and Store B \n (Dependent)", #Adds a title for the plot
		x = NULL, y = "Average Revenue" #Labels for the x and y axis
		)
   

ggsave("TTestPlot_Dep.jpeg", #Pathway to save plot to 
	TTestplot #Indicates to save the TTestplot 
)

write.csv(Sales, #Indicates to write the Sales dataframe
	"TestData_Dep.csv" #Indicates the filepath to save the data to
)

#performing dependent t-test (paired, because we used equal variance)
str(Sales)
x <- filter(Sales, Store == "Store B")
y <- filter(Sales, Store == "Store A")
t.test(x$Revenue, y$Revenue, paired = TRUE)

#p-value < 2.2e-16 is an extremely small
#There is a statistically signficiant difference in the means

#Similary, let us generate a new database with the same means as above, 
#(Indepenedent T-Test) but this time we use different variance as well

StoreA <- rnorm(365, 5000.21, 332) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreB <- rnorm(365, 1000.12, 66.4) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332

Revenue <- c(StoreA, StoreB) #Combines values into one column. All 365 values of Store A will be combined along with all 365 values of Store B

Store <- rep(c("Store A", "Store B"), each = 365) #Creates of column of Store A and Store B as the two levels. 365 iterations will be created for Store A and then Store \ 

Sales <- data.frame(Store, Revenue) #Creates a dataframe of the Revenue and their corresponding store 

TTestplot <- ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Store)) + 
  stat_summary(fun.y = "mean", geom = "bar") + #
  stat_summary(aes(label=round(..y..,2)), 
               fun.y=mean, geom="text", size=3.5, vjust = -0.5) + #Plots the text of the average value for each level
  theme(panel.background=element_blank()) + #Creates a blank background for the plot's panel
  theme(plot.background=element_blank()) + #Creates a blank background for the plot 
  theme(plot.title = element_text(hjust = 0.5))+ #Centers plot title
  theme(legend.position="none") + # Gets rid of the legend
  labs(title = "Average Revenue per day Between Store A and Store B \n (Independent)", #Adds a title for the plot
       x = NULL, y = "Average Revenue" #Labels for the x and y axis
  )

ggsave("TTestPlot_Indep.jpeg", #Pathway to save plot to 
      TTestplot #Indicates to save the TTestplot 
)

write.csv(Sales, #Indicates to write the Sales dataframe
          "TestData_Indep.csv" #Indicates the filepath to save the data to
)

#performing dependent t-test (paired, because we used equal variance)
str(Sales)
x <- filter(Sales, Store == "Store B")
y <- filter(Sales, Store == "Store A")
t.test(x$Revenue, y$Revenue, paired = FALSE)

# Again, the p-value is really small (<2.2e-16)
# The difference in means is statistically significant
 
#Additional Comments:
# 1 )We need to describe in brief what the difference between independent and dependent test is
# For independent, first of all we have to assume that the two stores have similar variance,
# After that, we are looking at the difference of EACH DAY's revenue in dependent
# where as for independent, as the name suggests, the variance can be different from each other
# Also, we are not concerened about the differences in each day's revenue
# we are concerned about the difference in AVERAGE REVENUE FOR THE ENTIRE YEAR

# 2) Wouldn't it be better if we used mean that were a bit closer to each other?
# The differenc between 5,000 and 1000 seems a bit large

# 3) I have not made any change to the InsigTTest.R script
# this is because I wanted to confirm the changes made in SigTTest.R 
# before copying them over to InsigTTest.R 

head(Sales)
