#The script was provided by TashiNyangmi through his "Correlation/Regression" pull request

library(ggplot2)

#Simulate Perfect positive correlation
n <- 100; m1 <- 8; m2 <- 2; s1 <- 1;s2 <- 3;p <- 1

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

PerfPosCor <- data.frame(x, y)

##################################################################
#Simulate Slightly Postive Correlation

n <- 100; m1 <-8; m2 <- 2; s1 <- 1;s2 <- 3;p <- .5

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

SlightPosCor <- data.frame(x, y)

##################################################################
#Simulate No Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p<- 0

x<- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

NoCor <- data.frame(x, y)

##################################################################
#Simulate Slightly Negative Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p<- -.5

x<- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

SlightNegCor <- data.frame(x, y)


##################################################################
#Simulate Perfect Negative Correlation

n <- 100; m1 <-8; m2<-2; s1<-1;s2<-3;p <- -1

x <- rnorm(n, m1, s1)

y <- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

PerfNegCor <- data.frame(x, y)

##################################################################

#Save all data.frame as csv file to working directory
write.csv(PerfPosCor, file="PerfPosCor.csv")    
write.csv(SlightPosCor, file="SlightPosCor.csv")    
write.csv(NoCor, file="NoCor.csv")    
write.csv(SlightNegCor, file="SlightNegCor.csv")    
write.csv(PerfNegCor, file="PerfNegCor.csv")    

##################################################################


#For all 5 dataframe's plots: Generate, and save scatterplots as JPEGs

#create a function, "cor_plt_fn" to: Generate, print and save scatterplots as JPEGs

cor_plt_fn <- function(df, title, filename){
	
	cor_plt <- ggplot(df, aes(x,y)) + 
		geom_point()+ 
		geom_hline(yintercept = mean(y)) + #creates a horizontal line at the mean of y
		geom_vline(xintercept = mean(x)) + #creates a vertical line at the mean of x
		ylim(-10,12) + # sets the range of y-axis
		xlim(3,12) + # sets the range of x-axis
		theme(plot.title = element_text(hjust = 0.5)) + #sets the horizontal alignment of title to center
		labs(title = title) #prints a title w/ the plot
	
	ggsave(paste(filename,".jpeg", sep = ""), cor_plt) # saves the plot, "cor_plt" with file name "<filename variable> + .jpeg" to working directory
	
}

#create 3 list for the three arguments to be passed to the UDF(user defined function), "cor_plt_fn".

dataset_list <- list(PerfPosCor, SlightPosCor, NoCor, SlightNegCor, PerfNegCor)

title_list <- list("Scatterplot : Perfect Postive Correlation",
	"Scatterplot : Slightly Postive Correlation",
	"Scatterplot : No Correlation",
	"Scatterplot : Slightly Negative Correlation",
	"Scatterplot : Perfect Negative Correlation")

plt_filename_list <- list("Scatterplot_Perfect_Postive_Correlation",
	"Scatterplot_Slightly_Postive_Correlation",
	"Scatterplot_No_Correlation",
	"Scatterplot_Slightly_Negative_Correlation",
	"Scatterplot_Perfect_Negative_Correlation" )

#call the function, "cor_plt_fn" within the loop to run it for all dataframes

for (i in 1:length(dataset_list)){
	cor_plt_fn(dataset_list[[i]], title_list[[i]], plt_filename_list[[i]]) #this function generates, prints, and saves scatterplots as JPEGs
}

##################################################################
