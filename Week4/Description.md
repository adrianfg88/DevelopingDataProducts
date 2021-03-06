Overview
========================================================
author: Adrian Fabela
date: 28/01/2021
autosize: true

Description
========================================================
This Shiny App is to predict the Species of the iris data frame by selecting Sepal Length and Sepal Width.
How it works is the following. It uses the random forest method to predict the type of species using the sepal width and length.


Slide With UI Code
========================================================


```r
shinyUI(fluidPage(
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
))
```

Slide With Server Code
========================================================


```r
shinyServer(function(input, output) {
   
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
})
```


Slide With Server Code
========================================================
The match of the species has an R value of 95% which is quite good.
