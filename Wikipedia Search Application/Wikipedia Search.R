library(plyr)
library(stringr)
library(lubridate)
library(jsonlite)
library(httr)
library(xml2)
library(rvest)
library(devtools)
library(ggplot2)
library(RSelenium)
library(pageviews)
library(aRxiv)
library(streamR)
library(scales)
library(shiny)

ui <- fluidPage(
  titlePanel("Number of Page Views on Wikipedia", windowTitle = "Wiki Page Views"),
  sidebarLayout(
    sidebarPanel(("Enter Name of Wikipedia Article to Query Here"),
      textInput("search1", "Article Title (Case Sensitive)", ""),
      textInput("search2", "Article Title (Case Sensitive)", ""),
      dateInput(inputId = "start",
                label = "Search Start Date",
                value = Sys.Date()-30),
      dateInput(inputId = "end",
                label = "Search End Date",
                value = Sys.Date()),
      sliderInput(inputId = "line", 
                  label = "Line Color Strength",
                  value = .4, min = 0, 
                  max = 1),
      sliderInput(inputId = "point", 
                  label = "Adjust Point Size",
                  value = 1, min = 0, 
                  max = 5),
      sliderInput(inputId = "smooth", 
                  label = "Overplotting Visual Aid",
                  value = 0, min = 0, 
                  max = 5),
      submitButton(text = "Refresh"),
      radioButtons(inputId = "var3",
                   label = "Select the file type",
                   choices = list("png","pdf")),
      downloadButton(outputId = "down",
                     label = "Export Plot",
                     class = NULL),
      width = 3),
    mainPanel((""),
              plotOutput("line"))
    )
       )


server <- function(input, output){
  output$line <- renderPlot(
                        ggplot(
                              data = 
                                    {df1 <- article_pageviews(project = "en.wikipedia", 
                                                               article = input$search1, 
                                                               user_type = "user", 
                                                               start = input$start, 
                                                               end = input$end)
                                      df2 <- article_pageviews(project = "en.wikipedia", 
                                                               article = input$search2, 
                                                               user_type = "user", 
                                                               start = input$start, 
                                                               end = input$end)
                                            df1$Page = input$search1
                                            df2$Page = input$search2
                                      df = rbind(df1, df2)
                                                                      },
                         aes(date, views, color=Page))
                            + geom_smooth(size=input$smooth*.5, se = TRUE,
                                          span = input$smooth*.2)
                            + geom_line(alpha=input$line)
                            + geom_point(size=input$point)
                            + scale_y_log10()
                            + ggtitle("Page Views on Wikipedia Over Time")
                            + labs(x = "Date", y = "Number of Views (Log 10)")
                            + theme(plot.title = element_text(color="black",
                                                              face="bold.italic",
                                                              size=24,
                                                              hjust=.5),
                                    axis.title = element_text(color="black",
                                                              face="italic",
                                                              size=18)),
                        height = 600)
  
  output$down <- downloadHandler(
    filename = function(){
      paste("plot",input$var3, sep = ".")
    },
      content = function(file){
              if(input$var3 =="png")
                  png(file)
              else
                  pdf(file)
      plot(ggplot(
            data = 
                  {
                    df1 <- article_pageviews(project = "en.wikipedia", 
                                  article = input$search1, 
                                  user_type = "user", 
                                  start = input$start, 
                                  end = input$end)
                   df2 <- article_pageviews(project = "en.wikipedia", 
                                 article = input$search2, 
                                 user_type = "user", 
                                 start = input$start, 
                                 end = input$end)
                       df1$Page = input$search1
                       df2$Page = input$search2
                   df = rbind(df1, df2)
                   },
            aes(date, views, color=Page))
                         + geom_smooth(size=input$smooth*.5, se = TRUE,
                             span = input$smooth*.2)
                         + geom_line(alpha=input$line)
                         + geom_point(size=input$point)
                         + scale_y_log10()
                         + ggtitle("Page Views on Wikipedia Over Time")
                         + labs(x = "Date", y = "Number of Views (Log 10)")
                         + theme(plot.title = element_text(color="black",
                                          face="bold.italic",
                                          size=24,
                                          hjust=.5))
                         + theme(axis.title = element_text(color="black",
                                          face="italic",
                                          size=18)),
                                          height = 700, 
                                          width = 1400) 
      dev.off()
    }
    ) 
}

shinyApp(ui = ui, server = server)