library(ggplot2)

##No Main Effect A (Store), Main Effect B (Gender), No Interaction
Store <- rep(c("Store A", "Store B"), each = 100)
Gender <- rep(rep(c("Male", "Female"), each = 50),
	2)

Plot 3: Yes, Yes, No
A1B1 = 15
A1B2 = 10

A2B1 = 20
A2B2 = 15

StoreAMale <- rnorm(50, 3000, 10) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreAFemale <- rnorm(50, 1000, 10) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332
StoreBAMale <- rnorm(50, 7000, 10) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreBFemale <- rnorm(50, 5000, 10) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332

Revenue <- c(StoreAMale, StoreAFemale, StoreBAMale, StoreBFemale)

Sales <- data.frame(Store, Gender, Revenue)

MainA_MainB_NoIntplot <-  ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Gender)) + 
	stat_summary(fun.y = "mean", geom = "bar", width = 0.5, position = "dodge") + #Computes the mean of each Store group to be plotted as a bar and adjust the width of each bar
	labs(title = "Main Effect for Store, \n Main Effect for Gender, \n No Interaction ", #Adds a title for the plot
		x = NULL, y = "Average Revenue" )

write.csv(Sales,
	file = "MainA_MainB_NoIntData.csv")

ggsave("MainA_MainB_NoIntDataPlot.jpeg",
	plot = MainA_MainB_NoIntplot)
