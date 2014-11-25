library(shiny)
library(dplyr)
baseline.numbers <- matrix(nrow=100000)
enhanced.numbers <- matrix(nrow=100000)
shinyServer(
        function(input, output) {
                
                benefit.numbers.standard <- reactive({rnorm(n=100000,mean=((input$benefit.upper + input$benefit.lower)/2),sd=((input$benefit.upper - input$benefit.lower)/3.29))})
                benefit.numbers.certain <- reactive({rnorm(n=100000,mean=(input$benefit.upper + input$benefit.lower)/2,sd=(input$benefit.upper - input$benefit.lower)/(3.39*2))})
                benefit.numbers.better <- reactive({rnorm(n=100000,mean=(input$benefit.upper + input$benefit.lower)*1.2/2,sd=(input$benefit.upper - input$benefit.lower)/3.29)})
                benefit.numbers.certain.better <- reactive({rnorm(n=100000,mean=(input$benefit.upper + input$benefit.lower)*1.2/2,sd=(input$benefit.upper - input$benefit.lower)/(3.29*2))})
                
                capex.numbers.standard <- reactive({rnorm(n=100000,mean=(input$capex.upper + input$capex.lower)/2,sd=(input$capex.upper - input$capex.lower)/3.29)})
                capex.numbers.certain <- reactive({rnorm(n=100000,mean=(input$capex.upper + input$capex.lower)/2,sd=(input$capex.upper - input$capex.lower)/(3.29*2))})
                capex.numbers.better <- reactive({rnorm(n=100000,mean=(input$capex.upper + input$capex.lower)*0.8/2,sd=(input$capex.upper - input$capex.lower)/3.29)})
                capex.numbers.certain.better <- reactive({rnorm(n=100000,mean=(input$capex.upper + input$capex.lower)*0.8/2,sd=(input$capex.upper - input$capex.lower)/(3.29*2))})
                
                opex.numbers.standard <- reactive({rnorm(n=100000,mean=(input$opex.upper + input$opex.lower)/2,sd=(input$opex.upper - input$opex.lower)/3.29)})
                opex.numbers.certain <- reactive({rnorm(n=100000,mean=(input$opex.upper + input$opex.lower)/2,sd=(input$opex.upper - input$opex.lower)/(3.29*2))})
                opex.numbers.better <- reactive({rnorm(n=100000,mean=(input$opex.upper + input$opex.lower)*0.8/2,sd=(input$opex.upper - input$opex.lower)/3.29)})
                opex.numbers.certain.better <- reactive({rnorm(n=100000,mean=(input$opex.upper + input$opex.lower)*0.8/2,sd=(input$opex.upper - input$opex.lower)/(3.29*2))})
                
                        
                baseline.numbers$benefits <- reactive({sample(benefit.numbers.standard(),100000)})
                baseline.numbers$capex <- reactive({sample(capex.numbers.standard(),100000)})
                baseline.numbers$opex <- reactive({sample(opex.numbers.standard(),100000)})
                baseline.result <- reactive({baseline.numbers$benefits() - (baseline.numbers$capex() + baseline.numbers$opex())})
                
                enhanced.numbers$benefits <- reactive({
                        if (sum(input$benefits.check == 0)) {
                                sample(benefit.numbers.standard(),100000)
                        } else if (sum(input$benefits.check == 1)) {
                                sample(benefit.numbers.certain(),100000)
                        } else if (sum(input$benefits.check == 2)) {
                                sample(benefit.numbers.better(),100000)
                        } else if (sum(input$benefits.check == 3)) {
                                sample(benefit.numbers.certain.better(),100000)
                        } else {
                                sample(benefit.numbers.standard(),100000)
                        }
                        })
                enhanced.numbers$capex <- reactive({
                        if (sum(input$capex.check == 0)) {
                                sample(capex.numbers.standard(),100000)
                        } else if (sum(input$capex.check == 1)) {
                                sample(capex.numbers.certain(),100000)
                        } else if (sum(input$capex.check == 2)) {
                                sample(capex.numbers.better(),100000)
                        } else if (sum(input$capex.check == 3)) {
                                sample(capex.numbers.certain.better(),100000)
                        } else {
                                sample(capex.numbers.standard(),100000)
                        }
                        })
                enhanced.numbers$opex <- reactive({
                        if (sum(input$opex.check == 0)) {
                                sample(opex.numbers.standard(),100000)
                        } else if (sum(input$opex.check == 1)) {
                                sample(opex.numbers.certain(),100000)
                        } else if (sum(input$opex.check == 2)) {
                                sample(opex.numbers.better(),100000)
                        } else if (sum(input$opex.check == 3)) {
                                sample(opex.numbers.certain.better(),100000)
                        } else {
                                sample(opex.numbers.standard(),100000)
                        }
                        })
                enhanced.result <- reactive({enhanced.numbers$benefits() - (enhanced.numbers$capex() + enhanced.numbers$opex())})
                
                output$plot <- renderPlot({      
                        hist(baseline.result(),breaks=50,xlab= "Value", freq=FALSE,col="grey",main="Baseline Profit")
                        abline(v=mean(baseline.result()),col="blue",lwd=3)
                        abline(v=mean(baseline.result())-(1.645*sd(baseline.result())),col="red",lwd=2)
                        abline(v=mean(baseline.result())+(1.645*sd(baseline.result())),col="red",lwd=2)
                })
                
                output$plot2 <- renderPlot({      
                        hist(enhanced.result(),breaks=50,xlab= "Value",freq=FALSE,col="grey",main = "Adjusted Profit")
                        abline(v=mean(enhanced.result()),col="blue",lwd=3)
                        abline(v=mean(enhanced.result())-(1.645*sd(enhanced.result())),col="red",lwd=2)
                        abline(v=mean(enhanced.result())+(1.645*sd(enhanced.result())),col="red",lwd=2)
                })
        }
)