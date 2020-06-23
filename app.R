#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

ARL <- readRDS("/run/media/spencerrh/DataStore/Nextcloud/Project/ARL.rds")
tdata <- readRDS("/run/media/spencerrh/DataStore/Nextcloud/Project/fixedData.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bivariate Copula Results"),

    navlistPanel(
      tabPanel("Summary",
               # Sidebar with a slider input for number of bins
               sidebarLayout(
                 sidebarPanel(
                   selectInput("correlation",
                               "Bivariate Correlation:",
                               choices = c("0.6", "0.2", "-0.2", "-0.6")),
                   selectInput("chartType",
                               "Control Chart Type:",
                               choices = c("T2", "MEWMA", "MCUSUM"))
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("chartPlot"),
                   dataTableOutput("mytable")
                 )
               )
             ),
      tabPanel("Control Chart",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("correlation2",
                               "Bivariate Correlation:",
                               choices = c("0.6", "0.2", "-0.2", "-0.6")),
                   selectInput("copula",
                               "Copula:",
                               choices = c("Normal", "Frank", "Clayton", "Gumbel")),
                   sliderInput("simN",
                               "Simulation Number",
                               min = 1, max = 1000, value = 1),
                   selectInput("shiftVal",
                               "Mean shift amount",
                               choices = c("0/0", "0/0.5", "0/1", "0/2", "0/3")),
                   selectInput("chartType2",
                               "Control Chart Type:",
                               choices = c("t2", "mewma", "mcusum"))
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("dataPlot"),
                   plotOutput("controlPlot"),
                   dataTableOutput("simTable")
                 )
               ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$dataPlot <- renderPlot({
      tmptibble <- tdata %>%
        filter(rho == input$correlation2, copula == input$copula, iteration == input$simN, shift == input$shiftVal)
      p <- ggplot(tmptibble) +
        geom_point(mapping = aes(x = data1, y = data2))
      print(p)
    })

    output$controlPlot <- renderPlot({
      tmptibble <- tdata %>%
        filter(rho == input$correlation2, copula == input$copula, iteration == input$simN, shift == input$shiftVal)
      p <- ggplot(tmptibble) +
        geom_line(mapping = aes(x = N, y = !!sym(input$chartType2)), size = 0.2) +
        geom_point(mapping = aes(x = N, y = !!sym(input$chartType2), color = !!sym(input$chartType2) < 10), size = 0.5) +
        scale_colour_manual(name = 'In Control', values = setNames(c('black','red'),c(T, F))) +
        geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
        geom_text(aes(0,10,label = "ucl", vjust = -1))
      print(p)
    })

    output$simTable <- renderDataTable({
      tmptibble <- tdata %>%
        ungroup() %>%
        filter(rho == input$correlation2, copula == input$copula, iteration == input$simN, shift == input$shiftVal) %>%
        select(data1, data2, !!sym(input$chartType2))
      datatable(tmptibble, options = list(lengthMenu = c(10, 50, 100)))
    })

    output$chartPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      tmptibble <- ARL %>%
        filter(rho == input$correlation)
      if(input$chartType == "T2") {
        p <- tmptibble %>%
          ggplot(aes(x = copula, y = t2ARLmean, fill = shift)) +
          geom_bar(position = "dodge", stat = "identity") +
          geom_errorbar(aes(ymin = t2ARLlwr, ymax = t2ARLupr), width = .2, position = position_dodge(.9)) +
          ggtitle(paste("Hotelling T-squared, Correlation", input$correlation))
        print(p)
      } else if(input$chartType == "MEWMA") {
        p <- tmptibble %>%
          ggplot(aes(x = copula, y = meARLmean, fill = shift)) +
          geom_bar(position = "dodge", stat = "identity") +
          geom_errorbar(aes(ymin = meARLlwr, ymax = meARLupr), width = .2, position = position_dodge(.9)) +
          ggtitle(paste("MEWMA, Correlation", input$correlation))
        print(p)
      } else {
        p <- tmptibble %>%
          ggplot(aes(x = copula, y = mcARLmean, fill = shift)) +
          geom_bar(position = "dodge", stat = "identity") +
          geom_errorbar(aes(ymin = mcARLlwr, ymax = mcARLupr), width = .2, position = position_dodge(.9)) +
          ggtitle(paste("MCUSUM, Correlation", input$correlation))
        print(p)
      }
    })

    output$mytable <- renderDataTable({
      tmptibble <- ARL %>%
        filter(rho == input$correlation)
      if(input$chartType == "T2") {
        ptable <- tmptibble %>% ungroup() %>%
          mutate(t2 = paste(format(t2ARLmean, digits = 7), "±", format(as.numeric(format(t2ARLmean - t2ARLlwr, digits = 4)), scientific = FALSE))) %>%
          select(copula, shift, t2)
        datatable(ptable, options = list(lengthMenu = c(9, 18, 36), pageLength = 9))
      } else if(input$chartType == "MEWMA") {
        ptable <- tmptibble %>% ungroup() %>%
          mutate(mewma = paste(format(meARLmean, digits = 7), "±", format(as.numeric(format(meARLmean - meARLlwr, digits = 4)), scientific = FALSE))) %>%
          select(copula, shift, mewma)
        datatable(ptable, options = list(lengthMenu = c(9, 18, 36), pageLength = 9))
      } else {
        ptable <- tmptibble %>% ungroup() %>%
          mutate(mcusum = paste(format(mcARLmean, digits = 7), "±", format(as.numeric(format(mcARLmean - mcARLlwr, digits = 4)), scientific = FALSE))) %>%
          select(copula, shift, mcusum)
        datatable(ptable, options = list(lengthMenu = c(9, 18, 36), pageLength = 9))
      }
    })
}

# Run the application
options(shiny.host="0.0.0.0", shiny.port = "4614")
shinyApp(ui = ui, server = server)
