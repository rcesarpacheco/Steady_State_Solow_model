library(shiny)
# library(rsconnect)
library(latex2exp)
library(mathjaxr)
alpha <- 1/3
delta <- 0.05
s_max <- 1
A_max <- 1.4
A_min <- 0.7
delta_min <- 2/100
output <- function(k,A) {
  return(A*k^alpha)
}

find_k_ss <- function(savings,delta,A) {
  k_ss <- (A*savings/delta)^(1/(1-alpha))
  return(k_ss)
}



# Define UI for application
ui <- fluidPage(
  withMathJax(),
  # Application title
  titlePanel("Steady State in the Solow Model"),
  # Add notes
  fluidRow(
    column(12,
             p("This app calculates and visualizes the steady state in the Solow Model."),
            p("The production function is given by \\(Y = A K^{1/3} \\cdot L^{2/3}\\) where A is the productivity parameter, K is the capital stock, and L is the labor force"),
           p("Adjust the sliders to change the values and see the effects on investment, output, and depreciation.")
    )
  ),
  # Sidebar with a slider input for the parameter s
  sidebarLayout(
    sidebarPanel(
      sliderInput("s",
                  "Savings Rate:",
                  min = 0,
                  max = s_max,
                  value = 0.5,
                  step = 0.05),
    sliderInput("delta",
                "Depreciation rate:",
                min = delta_min,
                max = 0.15,
                value = 0.05,
                step = 0.005),
    sliderInput("A",
                "Productivity A:",
                min = A_min,
                max = A_max,
                value = 1,
                step = 0.005)
  ),
    # Show a plot of the generated function
    mainPanel(
      plotOutput("plot")
      # Display calculated results
      # textOutput("y_ss_text"),
      # textOutput("i_ss_text")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  output$plot <- renderPlot({
    s <- input$s
    delta <- input$delta
    A <- input$A
    k_ss_max <- find_k_ss(s_max,delta_min,A_max)
    k_max <- k_ss_max
    k <- seq(0, k_max, by = 0.1)
    ymax <- k[length(k)]^alpha
    depreciation <- delta*k
    y <- output(k,A)
    investment <- s * y
    k_ss <- find_k_ss(input$s,delta,A)
    i_ss <- delta*k_ss
    y_ss <- output(k_ss,A)
    plot(k, investment, type = "l", col = "blue", lwd = 2,
         xlab = "k", ylim = c(0, ymax),xlim = c(0, k_max),ylab='',
        yaxs="i",xaxs='i',xaxt='n',yaxt='n')
    axis(1, at = c(k_ss),labels=c(paste('k*=',round(k_ss,2))))
    
    # axis(2, at = c(i_ss,y_ss),labels=c('i*','y*'),col=c('blue','black'))
    axis(2, at = c(i_ss,y_ss),labels=F)
    
    text(y = c(i_ss,y_ss), x = par("usr")[1] - 6, 
         labels =c(paste0('i*= ',round(i_ss,2)),paste0('y*= ',round(y_ss,2))), 
         col = c("blue", "black"), xpd = TRUE, srt = 90,cex=0.7)
    
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
    
    
    y_ss_val <- reactive({y_ss})
    
    i_ss_val <- reactive({i_ss})
    
    # Display mean and standard deviation
    output$y_ss_text <- renderText({
      paste("Steady state output per capita =", round(y_ss_val(), 2))
    })
    
    output$i_ss_text <- renderText({
      paste("Steady state investment per capita =", round(i_ss_val(), 2))
    })
    
  })
}
shinyApp(ui = ui, server = server)

