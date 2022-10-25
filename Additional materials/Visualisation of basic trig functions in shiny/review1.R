library(shiny)

ui <- pageWithSidebar(
  
  headerPanel("Review of trigonometry 1"),
    
    fluidRow(
      column(5, plotOutput("plot", width = 400, height = 400)
      ),
      column(7, plotOutput("plot2", width = 500, height = 400)
      )
    ),
    fluidRow(
      column(4),
      column(8,
      sliderInput("x", "Theta", -2, 2, 0, step = 0.1, animate = animationOptions(interval = 100, loop = TRUE)))
    )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    t <- seq(0, 2*pi,length = 100) 
    coords <- t(rbind(sin(t)*1, cos(t)*1))
    plot(coords, type = "l", xlab = "x", ylab = "y")
    abline(h = 0, lty = 3, col = "black")
    abline(v = 0, lty = 3, col = "black")
    points(cos(input$x*pi), sin(input$x*pi), col = "red")
    segments(0, 0, cos(input$x*pi), sin(input$x*pi), col = "red")
    
  })
  
  output$plot2 <- renderPlot({
    
    plot(seq(-100, 100, 0.01), sin(seq(-100, 100, 0.01)),
         type = "l",
         col = "black",
         xlab = "x",
         ylab = "y",
         xlim = c(-2*pi, 2*pi),
         ylim = c(-2, 2)
    )
    lines(seq(-100, 100, 0.01), cos(seq(-100, 100, 0.01)),
                                    type = "l",
                                    col = "black")                      
    abline(h = 0, lty = 3, col = "black")
    abline(v = 0, lty = 3, col = "black")
    points(input$x*pi, sin(input$x*pi), col = "red")
    points(input$x*pi, cos(input$x*pi), col = "red")
  })
  
}

shinyApp(ui, server)