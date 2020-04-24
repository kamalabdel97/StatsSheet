set.seed(1)

library(ggplot2)
library(dplyr)
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
titlePanel("Visualizing t-tests"),

# taking inputs
# for 1st dataset
numericInput('mean1','For 1st dataset: mean =', min = 0, value = 1200),
numericInput('sd1','For 1st dataset: standard deviation =', min = 0, value = 200),

#for second dataset

numericInput('mean2','For 2nd dataset: mean =', min = 0, value = 1200),
numericInput('sd2','For 2nd dataset: standard deviation =', min = 0, value = 200)
  ),

# outputting plot
  mainPanel(
plotOutput('scatterPlot'),
  )
  )

)

server <- function(input, output, session) {
  
  output$scatterPlot <- renderPlot({

  StoreA <- rnorm(365, as.integer(input$mean1), input$sd1) # Creates 365 samples of occurences, with a mean of 5000.21 and standard deviation of 332
  StoreB <- rnorm(365, as.integer(input$mean2), input$sd2) # Creates 365 samples of occurences, with a mean of 1000.12 and standard deviation of 332

  Revenue <- c(StoreA, StoreB) #Combines values into one column. All 365 values of Store A will be combined along with all 365 values of Store B

  Store <- rep(c("Store A", "Store B"), each = 365) #Creates of column of Store A and Store B as the two levels. 365 iterations will be created for Store A and then Store \ 

  Sales <- data.frame(Store, Revenue) #Creates a dataframe of the Revenue and their corresponding store 

  ggplot(Sales, aes(x=Revenue)) +
    geom_histogram(binwidth = 20) + facet_wrap(~Store)
  
  #ggplot(Sales, aes(Store, Revenue, label = Revenue, fill = Store)) + 
	 # stat_summary(fun.y = "mean", geom = "bar") + #
	 # stat_summary(aes(label=round(..y..,2)), 
	 # fun.y=mean, geom="text", size=3.5, vjust = -0.5) 
  })

  
}

shinyApp (ui=ui, server = server)
