library(ggplot2)


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
	labs(title = "Average Revenue per day Between Store A and Store B", #Adds a title for the plot
		x = NULL, y = "Average Revenue" #Labels for the x and y axis
		)
   

ggsave("/Users/kamal/Documents/GitHub/StatsSheet/VisualizationsRFiles/TTestPlot.jpeg", #Pathway to save plot to 
	TTestplot #Indicates to save the TTestplot 
)

write.csv(Sales, #Indicates to write the Sales dataframe
	"/Users/kamal/Documents/GitHub/StatsSheet/VisualizationsRFiles/TTestData.csv" #Indicates the filepath to save the data to
	)



