library(shiny)

ui <- pageWithSidebar(
  
  headerPanel("Review of trigonometry (2)"),

    sidebarPanel(
      
      selectInput(inputId = "trig_fun",
                  label = "Select function" ,
                  choices = c("Sin", 
                              "Cos")), 

      # A
      numericInput("A", "Amplitude (A)", 1),
      # B
      numericInput("B", "Period (B, 2pi/B)", 1),
      # C
      numericInput("C", "Phase shift (C)", 0),
      # D
      numericInput("D", "Vertical shift (D)", 0),
      # Input
      sliderInput("x1", "Input", -6, 6, 0, step = 0.1, animate = animationOptions(interval = 100, loop = TRUE))
      
    ),
    
    mainPanel(
      
      plotOutput("plot"),
      
      uiOutput("formula")
      
  )
)

server <- function(input, output) {
  
  x <- reactive({seq(-100, 100, 0.01)})
  
  trig <- reactive({
    switch(input$trig_fun,
           "Sin" = input$A * sin(input$B * (x() - input$C)) + input$D,
           "Cos" = input$A * cos(input$B * (x() - input$C)) + input$D)
  })
  
  trig2 <- reactive({
    switch(input$trig_fun,
           "Sin" = input$A * sin(input$B * (input$x1 - input$C)) + input$D,
           "Cos" = input$A * cos(input$B * (input$x1 - input$C)) + input$D)
  })
  
  output$plot <- renderPlot({
    
    plot(x(), trig(),
        type = "l",
        col = "black",
        xlab = "x",
        ylab = "y",
        xlim = c(-2*pi, 2*pi),
        ylim = c(-2, 2)
    )
    abline(h = 0, lty = 3, col = "red")
    abline(v = 0, lty = 3, col = "red")
    abline(h = 1, lty = 3)
    abline(h = -1, lty = 3)
    abline(v = pi, lty = 3)
    abline(v = -pi, lty = 3)
    abline(v = 2*pi, lty = 3)
    abline(v = 2*-pi, lty = 3)
    
    points(input$x1, trig2(), col = "red")

    })
  
  output$formula <- renderText({
    paste("f(x) = A * sin(B*x - C) + D and f(x) = A * cos(B*x - C) + D")
  })
  
}

shinyApp(ui, server)