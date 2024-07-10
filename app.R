library(shiny)
# library(rsconnect)
alpha <- 1/3
delta <- 0.05
output <- function(k) {
  return(k^alpha)
}

find_k_ss <- function(savings,delta) {
  k_ss <- (savings/delta)^(1/(1-alpha))
  return(k_ss)
}



# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Steady State in the Solow Model"),
  
  # Sidebar with a slider input for the parameter s
  sidebarLayout(
    sidebarPanel(
      sliderInput("s",
                  "Savings Rate:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.05),
    sliderInput("delta",
                "Depreciation rate:",
                min = 0.01,
                max = 0.5,
                value = 0.05,
                step = 0.005)
  ),
    # Show a plot of the generated function
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  output$plot <- renderPlot({
    s <- input$s
    delta <- input$delta
    k_max <- 150
    k <- seq(0, k_max, by = 0.1)
    ymax <- k[length(k)]^alpha
    investment <- s * k^alpha
    depreciation <- delta*k
    y <- output(k)
    k_ss <- find_k_ss(input$s,delta)
    i_ss <- delta*k_ss
    y_ss <- output(k_ss)
    plot(k, investment, type = "l", col = "blue", lwd = 2,
         xlab = "k", ylim = c(0, ymax),xlim = c(0, k_max),ylab='y, i and depreciation',
        yaxs="i",xaxs='i',xaxt='n',yaxt='n')
    axis(1, at = c(k_ss),labels=c(paste('k*=',round(k_ss,2))))
    
    # axis(2, at = c(i_ss,y_ss),labels=c('i*','y*'),col=c('blue','black'))
    axis(2, at = c(i_ss,y_ss),labels=F)
    
    text(y = c(i_ss,y_ss), x = par("usr")[1] - 2, 
         labels =c(paste('i*=',round(i_ss,2)),paste('y*=',round(y_ss,2))), 
         col = c("blue", "black"), xpd = TRUE, srt = 90)
    
    lines(k, y , col = "red", lwd = 2)
    lines(k, depreciation , col = "gray", lwd = 2)
    
    segments(k_ss,0,k_ss,i_ss,col='blue',lty='dashed')
    # text(k_ss, i_ss/2, labels='i*', pos = 4, col = "blue")
    points(k_ss, i_ss, col = "blue", pch = 19, cex = 1.5)
    
    segments(k_ss,i_ss,k_ss,y_ss,col='red')
    arrows(k_ss, i_ss+0.2, k_ss, i_ss, col = "red",length = 0.15)
    arrows(k_ss, y_ss-0.2, k_ss, y_ss, col = "red",length = 0.15)
    
    text(k_ss, (y_ss-i_ss)/2+i_ss, labels=paste('c*=',round(y_ss-i_ss,2)), pos = 4, col = "red")
    
    segments(0,y_ss,k_ss,y_ss,col='black',lty='dashed')
    segments(0,i_ss,k_ss,i_ss,col='blue',lty='dashed')
    
    # text(k_ss, 1.1*y_ss, labels='y*', pos = 4, col = "black")
    points(k_ss, y_ss, col = "black", pch = 19, cex = 1.5)
    
    legend("topright", 
           legend = c("Output", "Investment",'Depreciation'), col = c("red", "blue",'gray'), 
           lwd = 2,bty='n')
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

