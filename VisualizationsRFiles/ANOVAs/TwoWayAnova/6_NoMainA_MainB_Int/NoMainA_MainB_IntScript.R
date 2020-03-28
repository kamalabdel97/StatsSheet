library(ggplot2)

#No Main Effect A (Store), Main Effect B (Gender), Interaction
Store <- rep(c("Store A", "Store B"), each = 100)
Gender <- rep(rep(c("Male", "Female"), each = 50),
	2)

StoreAMale <- rnorm(50, 4000, 10) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreAFemale <- rnorm(50, 4000, 10) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332
StoreBAMale <- rnorm(50, 1000, 10) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
StoreBFemale <- rnorm(50, 10000, 10) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332

Revenue <- c(StoreAMale, StoreAFemale, StoreBAMale, StoreBFemale)

Sales <- data.frame(Store, Gender, Revenue)

MainA_NoMainB_Intplot <- ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Gender)) + 
	stat_summary(fun.y = "mean", geom = "bar", width = 0.5, position = "dodge") + #Computes the mean of each Store group to be plotted as a bar and adjust the width of each bar
	labs(title = "No Main Effect for Store, \n Main Effect for Gender, \n Interaction ", #Adds a title for the plot
		x = NULL, y = "Average Revenue")

write.csv(Sales,
	file = "NoMainA_MainB_IntData.csv")

ggsave("NoMainA_MainB_IntPlot.jpeg",
	plot = MainA_NoMainB_Intplot)
