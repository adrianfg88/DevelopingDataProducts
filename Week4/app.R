#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Predict Species by modifying other variables"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderSL", "What is the Petal Length?", min(iris$Sepal.Length), max(iris$Sepal.Length), value = mean(iris$Sepal.Length)),
            sliderInput("sliderSW", "What is the Petal Width?", min(iris$Sepal.Width), max(iris$Sepal.Width), value = mean(iris$Sepal.Width)),
            submitButton("Submit")
        ),
        mainPanel(
            plotOutput("plot1"),
            h3("According to you input the Sepcies is: "),
            textOutput("pred1"),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    gmodel <- train(as.factor(Species)~.,data=iris[,-c(3:4)], method="rf",prox=TRUE)
    
    
    model1pred <- reactive({
        SLInput <- input$sliderSL
        SWInput <- input$sliderSW
        predict(gmodel, newdata = data.frame(Sepal.Length=SLInput, Sepal.Width=SWInput))
    })
    
    
    output$plot1 <- renderPlot({
        
        plot(x=iris$Sepal.Length,y=iris$Sepal.Width, col=as.factor(iris$Species))
        points(input$sliderSL,input$sliderSW, col="black",pch=10)
        
    })
    
    output$pred1 <- renderText({
        as.character(model1pred())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
