library(ggplot2)
library(tidyverse)
library(car)
#wanted to consolidate all the theme functions into one with multiple arguments to increase program efficiency, but realized code readability trumps efficiency for such light programs


# generated three lists with equal variances because
# We want to conduct a one way ANOVA, s.d. or variance has to be equal(or close)


StoreA <- rnorm(365, 4200.21, 332) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreB <- rnorm(365, 4700.12, 332) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332
StoreC <- rnorm(365, 5300.12, 332) # Creates 365 samples of occurences, with a mean of 3000.12 and standard deviation of 664

# Create two columns
Revenue <- c(StoreA, StoreB, StoreC) #Combines values into one column.365 values each of Store A, Store B and Store C will be combined in that order 
Store <- rep(c("Store A", "Store B", "Store C"), each = 365) #Creates of column with Store A, Store B and Store C as the three levels. 365 iterations will be created for each Store one after the other

# Combine the columns into a data frame
Sales <- data.frame(Store, Revenue) #Creates a dataframe of the Revenue and their corresponding store 
write.csv(Sales, file="OneWayAnovaData.csv") # Creates csv file of Sales Data that was used in this script

head(Sales)

# Start Plotting for one way Anova
# barchart with mean revenue for each store



avg_barplt <- ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Store)) + 
 stat_summary(fun.y = "mean", geom = "bar", width = 0.5) + #Computes the mean of each Store group to be plotted as a bar and adjust the width of each bar
 stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=5, vjust = -0.5) + #Plots the text of the average value for each level
 theme(panel.background=element_blank(),plot.background=element_blank()) + #Creates a blank background for the plot's panel
 theme(plot.background=element_blank()) + #Creates a blank background or the plot 
	theme(plot.title = element_text(hjust = 0.5)) + # Centers plot title 
	theme(legend.position="none") + # Gets rid of the legend
	theme(axis.text = element_text(size =18), axis.title = element_text(size =16)) + # adjust the size of the axis titles
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+ # create some space between the y-axis title and y-axis text
	theme(plot.title = element_text(size = 18)) + #adjust the size of the Plot Title
	ylim(0, 7000) + #adjust the range of y-axis
	labs(title = "Average Revenue per day \n Between \n Store A, Store B, and Store C", #Adds a title for the plot
													x = NULL, y = "Average Revenue" ) #Labels for the x and y axis

avg_barplt


sep_boxplt <- ggplot(Sales, aes(x= Store, y = Revenue, label = Revenue, fill = Store)) + 
	geom_boxplot(width = 0.3) +
	theme(panel.background=element_blank(),plot.background=element_blank()) + #Creates a blank background for the plot's panel
	theme(plot.background=element_blank()) + #Creates a blank background for the plot 
	theme(plot.title = element_text(hjust = 0.5)) + # Centers plot title 
	theme(legend.position="none") + # Gets rid of the legend
	theme(axis.text = element_text(size =18), axis.title = element_text(size =16)) + # adjust the size of the axis titles
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+ # create some space between the y-axis title and y-axis text
	theme(plot.title = element_text(size = 18)) + #adjust the size of the Plot Title
	labs(title = "Separate Boxplot of Store A, Store B, and Store C", #Adds a title for the plot
					x = NULL, y = "Revenue" )

sep_boxplt


com_boxplt <- ggplot(Sales, aes(x = '', y = Revenue)) +
	geom_boxplot(width = 0.3, fill = "yellow1") +
	theme(panel.background=element_blank(),plot.background=element_blank()) + #Creates a blank background for the plot's panel
	theme(plot.background=element_blank()) + #Creates a blank background for the plot 
	theme(plot.title = element_text(hjust = 0.5)) + # Centers plot title 
	theme(legend.position="none") + # Gets rid of the legend
	theme(axis.text = element_text(size =18), axis.title = element_text(size =16)) + # adjust the size of the axis titles
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+ # create some space between the y-axis title and y-axis text
	theme(plot.title = element_text(size = 18)) + #adjust the size of the Plot Title
	labs(title = "Combined Boxplot of Store A, Store B, and Store C", #Adds a title for the plot
					x= NULL, 	 y = "Revenue" )

com_boxplt


# Exporting plots as JPEGs

ggsave("OneWayAnovaPlot1.jpeg", avg_barplt)
ggsave("OneWayAnovaPlot2.jpeg", sep_boxplt)
ggsave("OneWayAnovaPlot3.jpeg", com_boxplt)


oneway.test(Revenue~Store, var.equal = FALSE) #oneway Anova test



aov.out = aov (Revenue~Store, data = Sales) # this allows to store the output of the ANOVA test, and use extraction function later on
summary(aov.out)

#since the p value is really small (or the F-Statistics is big enough),
#we can conclude that there is a significant difference between at least one pair

#the next step would be to conduct t-tests(Independent t-test for this case) among the combinations of the 3 stores


