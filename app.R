#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
blm_google <- readRDS("blmgoogle.RDS")
blm_votes <- readRDS("blmvotes.RDS")
votes_clean <- readRDS("votesclean.RDS")

# Define UI for application that draws a histogram

ui <- navbarPage(
  "Trends of Social Movements and Bills Concerning Similar Issues",
  tabPanel("Bills in NY",
           fluidPage(
             titlePanel("Trends of Social Movements and Bills in NY"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("select",
                             "Bill Type: ",
                             choices = c("police_bill_prop", "hara_bill_prop",
                                         "assault_bill_prop")),
               ),
               mainPanel(plotOutput("billplot")))
           )),
  tabPanel("Google Trends",
           titlePanel("Google Trend Hashtag Data over Time"),
           fluidPage(
             titlePanel("Social Movements Popularity"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Trend",
                             "Hashtag",
                             choices = c("blm", "metoo")),
               ),
               mainPanel(plotOutput("GooglePlot"))),
             p("Note that theBlack Lives Matter Movement began to gain 
               traction in 2013 after the murder of Trayvon Martin. The 
               Me Too movement began to gain traction over social media
               after Alyssa Milano used the hashtag in a tweet in 
               October of 2017."),
           )),
  tabPanel("Google Trends and Bill Proportions",
           titlePanel("Google Trend Hashtag Data over Time"),
           fluidPage(
             titlePanel("Google Trends of Social Movements and Bills in NY"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("bill",
                             "Bill Type",
                             choices = c("police_bill_prop", "hara_bill_prop", "assault_bill_prop")),
                 # radioButtons(
                 #   inputId = "wage_type",
                 #   label = " ",
                 #   choices = list("adj" = "state_minimum_wage.adj", "non_adj" = "state_minimum_wage")
                 # )
               ),
               mainPanel(plotOutput("Comb_Plot"))),
             p("Note that the harassment and asssault bills are to be interpreted 
                 the #MeToo Movement in mind and police bills with the 
                 #BlackLivesMatter Movement"),
           )),
  tabPanel("Discussion",
           titlePanel("Do Hashtags Really Have an Impact on Legislation?"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them. Within this project I tried 
             to compare legislation in the state of NY and hashtag patters (via
             google trends data) to find correlation between the two phenomena. 
             While correlation does not imply causation, I wanted to see if public
             opinion or upset about something could have any influence on the 
             the bills we see passed. The trends shown on the Google Trends and Bill
             Proportions page look to answer this question. For the Black LIVes
             Matter plot, one can see a bit of correlation while looking at the 
             slope of the line, but there is lots of room for error as also 
             illustrated in the plot. CONTINUE FOR OTHER PLOT")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("This project looks to further understand the connection between 
           activism and advocacy on social media and legislation within the same
           timeframe. The main question is whether or not activism and movements
           over social media influence policies or policy makers. To explore 
           this question, I will be looking at google trends data from 2015 to
           2020 and comparing them to bills within  the NY legislature from 2019 to 2020. 
           With this I am looking to find trends between popular hashtags and bills
           and legislation to see if social media movements impact the law."),
           h3("About Me"),
           p("My name is Jasmine Hyppolite and I study Government and am part
           of the TechScience Program. You can reach me at
             jasminehyppolite@college.harvard.edu.")))

server <- function(input, output) {
  output$billplot <- renderPlot({
    
    votes_clean %>%
      filter(bill_type == input$select) %>%
      ggplot(aes(x = month_year, y = bill_prop)) + 
      geom_point(color = "deepskyblue3") + 
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) + 
      labs(x = "Month and Year",
           y = "Proportion of Bills Relating to Topic")
  })
  
  output$GooglePlot <- renderPlot({
    
    trends %>%
      filter(ID == input$Trend) %>%
      ggplot(aes(x = month_year, y = mean_score)) + 
      geom_point(color = "red") + 
      geom_line(color = "gray") + 
      theme(axis.text.x = element_text(angle = - 90, vjust = 0.5)) + 
      labs(title = "Google Trends for Frequency of # Searches by Month",
           x = "Month and Year",
           y = "Google Score")
  })
  
  output$Comb_Plot <- renderPlot({
    
    trends_w_vote %>%
      filter(bill_type == input$bill) %>%
      ggplot(aes(x = mean_score, y = bill_prop)) + geom_point() + 
      geom_smooth(method = "lm") + 
      labs(title = "Correlation Between Bills in NY Regarding Bill Type and Google Trend Scores",
           x = "Mean Google Trend Score for Movement",
           y = "Proportion of Bills per Week")

})

}
# Run the application 
shinyApp(ui = ui, server = server)
