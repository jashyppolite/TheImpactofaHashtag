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
metoo_votes <- readRDS("metoovotes.RDS")

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
           titlePanel(""),
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
  tabPanel("Analysis",
           titlePanel("Statistical Analysis"),
           p("Our statistical analysis for relationship between Google Trends Data
           regarding the Black Lives Matter and Me Too hashtags and bills brought up 
           within the NY Legislature consisted of a linear regression model. For this 
           project, three linear regression models were build: The first regresses the 
           proportion of bills in the NY legislature that have to do with policing on
           the mean Google Trends score for the Black Lives Matter hashtag per month. 
           This model's output indicated that for every 1 point increase in the google trend score for #Black Lives Matter,
           there is a .0006 increase in the proportion of bills discissed. This is a very small
           impact. The second regresses the proportion of bills in the NY legislature that have to do with assault on
           the mean Google Trends score for the Me Too hashtag per month. This second model's 
           output indicated that for every 1 point increase in the google trend score for #MeToo,
           there is a .0002 increase in the proportion of bills discissed. Similar to the first
           model, this is a very small impact. Finally, third regresses the 
           proportion of bills in the NY legislature that have to do with harassment on
           the mean Google Trends score for the Me Too hashtag per month.The final  model's 
           output indicated that for every 1 point increase in the google trend score for #Black Lives Matter,
           there is also a .0006 increase in the proportion of bills discissed. 
           Similar to the previous findings, this is a very small impact."),
           mainPanel(plotOutput("RegrPolicing")),
           mainPanel(plotOutput("RegrAssault")),
           mainPanel(plotOutput("RegrHarassment"))),
  tabPanel("Discussion",
           titlePanel("Do Hashtags Really Have an Impact on Legislation?"),
           p(" For this project, I attempted to understand the relationship 
           between hashtag popularity and legislation by using Google Trend scores 
           for specific hashtags and bills brought to the New York Legislature 
           as proxies, respectively. To understand this relationship and answer 
           the question of a hashtag's impact on laws and bills, I made three 
           linear regression models to find correlation between proportions of 
           bills having to do with either harassment, assault, or policing and 
           popularity of hashtags via Google Trends Data. Each of these models 
           noted a less than 0.001 correlation between bills and hashtag popularity
           (.0006, .0006, .0002). One could interpret this data to support the 
           statement that hashtags do not have a strong impact on legislation 
           that covers the issue that hashtags of social movements aim to highlight")),
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

  
  output$RegrPolicing <- renderPlot({
    
    blm_votes %>% 
      ggplot(aes(x = mean_score, y = police_bill_prop)) +
      geom_point() +
      geom_line(aes(y = fitted(police_pred)), color = "blue") + 
      geom_label(
        label = "Slope: .0006", 
        x= 30,
        y= .055,
        color = "black") + 
      labs(title = "Correlation between Google Trends Score Regarding #BlackLivesMatter
          and Legislation Regarding Policing",
           subtitle = "Description", 
           caption = "Source",
           x = "Average Google Trends Score per Month",
           y = "Police Bill Prop by Month") +
      scale_x_continuous(labels = scales::label_number()) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw()
    
  })
  
  output$RegrAssault <- renderPlot({
    
    metoo_votes %>% 
      ggplot(aes(x = mean_score, y = assault_bill_prop)) +
      geom_point() +
      geom_line(aes(y = fitted(assault_pred)), color = "blue") +
      geom_label(
        label = "Slope: .0002", 
        x= 20,
        y= .006,
        color = "black") + 
      labs(title = "Correlation between Google Trends Score Regarding #MeToo
          and Legislation Regarding Assault",
           subtitle = "Description", 
           caption = "Source",
           x = "Average Google Trends Score per Month",
           y = "Assault Bill Proportion by Month") +
      scale_x_continuous(labels = scales::label_number()) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw()
  })
  
  output$RegrHarassment <- renderPlot({
    
    metoo_votes %>% 
      ggplot(aes(x = mean_score, y = hara_bill_prop)) +
      geom_point() +
      geom_line(aes(y = fitted(hara_pred)), color = "blue") +
      geom_label(
        label = "Slope: .0006", 
        x= 22.5,
        y= .02,
        color = "black") + 
      labs(title = "Correlation between Google Trends Score Regarding #MeToo
          and Legislation Regarding Harassment",
           subtitle = "Description", 
           x = "Average Google Trends Score per Month",
           y = "Harassment Bill Proportion by Month") +
      scale_x_continuous(labels = scales::label_number()) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw()
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
