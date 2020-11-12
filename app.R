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
#exclusive_terms_100 <- readRDS("exclusive_terms_100.RDS")
# engagement_terms_100 <- read_xlsx("clean_engagement.xlsx")
# exclusive_terms_100 <- read_xlsx("clean_exclusion.xlsx")
blm_google <- readRDS("blmgoogle.RDS")
blm_votes <- readRDS("blmvotes.RDS")
votes_clean <- readRDS("votesclean.RDS")
# Define UI for application that draws a histogram
ui <- navbarPage(
  "Trends of Social Movements and Bills Concerning Similar Issues",
  tabPanel("Model",
           fluidPage(
             titlePanel("Google Trends of Social Movements and Bills in NY"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("select",
                             "Bill Type: ",
                             choices = c("police_bill_prop", "hara_bill_prop", "assault_bill_prop")),
                 # radioButtons(
                 #   inputId = "wage_type",
                 #   label = " ",
                 #   choices = list("adj" = "state_minimum_wage.adj", "non_adj" = "state_minimum_wage")
                 # )
               ),
               mainPanel(plotOutput("billplot")))
           )),
  tabPanel("Google Trends",
           titlePanel("Discussion Title"),
           fluidPage(
             titlePanel("Google Trends of Social Movements and Bills in NY"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("select",
                             "Bill Type: ",
                             choices = c("police_bill_prop", "hara_bill_prop", "assault_bill_prop")),
                 # radioButtons(
                 #   inputId = "wage_type",
                 #   label = " ",
                 #   choices = list("adj" = "state_minimum_wage.adj", "non_adj" = "state_minimum_wage")
                 # )
               ),
               mainPanel(plotOutput("billplot")))
           )),
  tabPanel("Discussion",
           titlePanel("Discussion Title"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
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

# impact of social media 
# when it comes to altering legislation or influencing policy makers.
# Specifically, I wanted to look at the difference between advocacy through
# social media in the form of a hashtag versus policy change, passing of bills,
# etc. To do this, I look at two datasets, one consisting of all tweets and 
# facebook post between 2015 and 2020 posted by accounts managed by 
# any member of the US House of Representatives or Senate. The 
# collection of tweets spans from January 1, 2015 to May 31, 2020 and sums 
# to over 3 million tweets and 1,362 Facebook posts. This will be paired 

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$billplot <- renderPlot({
    
    votes_clean %>%
      filter(bill_type == input$select) %>%
      ggplot(aes(x = month_year, y = bill_prop)) + 
      geom_point(color = "deepskyblue3") + 
      theme(axis.text.x = element_text(angle = - 90, vjust = 0.5)) 
    # + geom_smooth(method = "lm")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
