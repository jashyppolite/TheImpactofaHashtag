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
library(shinythemes)
library(huxtable)

blm_google <- readRDS("blmgoogle.RDS")
blm_votes <- readRDS("blmvotes.RDS")
votes_clean <- readRDS("votesclean.RDS")
metoo_votes <- readRDS("metoovotes.RDS")
trends <- readRDS("trends.RDS")
trends_w_vote <- readRDS("trendswvoteclean.RDS")
hara_pred <- readRDS("harapred.RDS")
assault_pred <- readRDS("assaultpred.RDS")
police_pred <- readRDS("policepred.RDS")
table <- readRDS("table.RDS")

# Define UI for application that draws a histogram

ui <- navbarPage(
  "Trends of Social Movements and Bills Concerning Similar Issues",
  tabPanel("Project Background and Motivations",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Social Movements, Hashtags, and Change"),
                     p("In building this project, I attempted to understand the relationship 
           between hashtag popularity and legislation by using Google Trends 
           for specific hashtags and bills brought to the New York Legislature 
           as proxies, respectively. In the age of digital social movements
           powered by hashtags, trending terms, and limited means of activism
           for some due to health concerns and covid-19, it is of incredible importance
           to quantify and understand the impact hashtags might have on legislation,
           whether it pushes positive change concerning laws, bills, and governmental
           responsibility, and whether it facilitates enough change to be the only
           thing many are doing to better our national community. Figuring out the impact
           of virtual movements, and consequently using that knowledge to find more ways to increase their impact, 
           can not only spread awareness of important social phenomena such as police brutality and sexual assault
           ans harassment, but prevent the loss of innocent life due to racism and sexism 
                       
            In an ideal world with more time and resources, this project would go further to 
            information regarding the popularity of hashtags on Twitter as well as Google, and 
            span farther in time. Additionaly, it would be great to expand the data sets regarding 
            bills in legislatures beyond New York State and compile data from legislatures across
                       the entire nation."),
           )),
  
  tabPanel("Bill Proportions",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Proportion of Bill Types in NY Legislature"),
                     p("The data regarding New York Legislature bills comes from several combined
             LegiScan datasets of bills brought up within the Legislature across several years. 
               The data represented here spans from 2015 to 2020. Bills were codified to be
               related to assault, harassment, or police using word detection capabilities."),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("select",
                                     "Bill Type: ",
                                     choices = c("police_bill_prop", "hara_bill_prop",
                                                 "assault_bill_prop")),
                       ),
                       mainPanel(plotOutput("billplot"))),
           )),

  tabPanel("Google Trends",
           titlePanel("Google Trends Hashtag Data over Time"),
           fluidPage(
             titlePanel(""),
             p("To interpret the first spikes for each movement, it is helful to 
             know that the The Black Lives Matter Movement began to gain 
               traction in 2013 after the murder of Trayvon Martin. The 
               MeToo movement gained traction over social media
               after Alyssa Milano tweeted the hashtag in 
               October of 2017."),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Trend",
                             "Hashtag",
                             choices = c("blm", "metoo")),
               ),
               mainPanel(plotOutput("GooglePlot"))),
           )),
  tabPanel("Google Trends and Bill Proportions",
           titlePanel(""),
           fluidPage(
             titlePanel("Google Trends of Social Movements and Bills in NY"),
             p("Note that the harassment and asssault bills are to be interpreted 
                 with the #MeToo Movement in mind and police bills with the 
                 #BlackLivesMatter Movement in mind."),
             sidebarLayout(
               sidebarPanel(
                 selectInput("bill",
                             "Bill Type",
                             choices = c("police_bill_prop", "hara_bill_prop", "assault_bill_prop")),
               ),
               mainPanel(plotOutput("Comb_Plot"))),
             p("Note that the harassment and asssault bills are to be interpreted 
                 with the #MeToo Movement in mind and police bills with the 
                 #BlackLivesMatter Movement in mind."),
           )),
  tabPanel("Models & Analysis",
           titlePanel("Statistical Analysis: Do Hashtags Really Impact Legislation?"),
           p("Our statistical analysis for relationship between Google Trends Data
           regarding the Black Lives Matter and Me Too hashtags and bills brought up 
           within the NY Legislature consisted of a linear regression model. For this 
           project, three linear regression models were build: The first regresses the 
           proportion of bills in the NY legislature that have to do with policing on
           the mean Google Trends Score for the Black Lives Matter hashtag per month. 
           This model's output indicated that for every 1 point increase in the Google Trend Score for #Black Lives Matter,
           there is a .0007 increase in the proportion of bills discussed regarding policing. This is a very small
           impact. To better understand these values, I also scaled the model to standard deviations
           and found a .271 standard deviation in the police bill proportion variable to 
           be associated with a 1 standard deviation increase in the Google Trend Score variable. 
           
           The second regresses the proportion of bills in the NY legislature that have to do with assault on
           the mean Google Trend Score for the MeToo hashtag per month. This second model's 
           output indicated that for every 1 point increase in the Google Trend Score for #MeToo,
           there is a .0002 increase in the proportion of bills discussed regarding assault. Similar to the first
           model, this is a very small impact. To make this result a bit more interesting, I also 
           scaled the model to standard deviations. This highlighted a .294 standard deviation increase the in 
           my harassment bill proportion variable associated with a 1 standard deviation increase in
           Google Trend Score variable. 
           
           Finally, the third model regresses theproportion of bills in the NY legislature that have to do with harassment on
           the mean Google Trends score for the Me Too hashtag per month. The final model's 
           output indicated that for every 1 point increase in the Google Trend Score for #Black Lives Matter,
           there is also a .0006 increase in the proportion of bills discussed regarding harassment Similar to the previous findings,
          this is a very small impact, however I still wanted to scale the model to get more information. In scaling
          this model, I found a .435 standard deviation increase associated with a 1 standard deviation increase
          in the Google Trend Score variable.
           
           In conclusion, to further understand this relationship and answer 
           the question of a hashtag's impact on laws and bills, I made three 
           linear regression models to find correlation between proportions of 
           bills having to do with either harassment, assault, or policing and 
           popularity of hashtags via Google Trends Data. Each of these models 
           noted a less than 0.001 correlation between bills and hashtag popularity
           (.0006, .0006, .0002). While still a positive correlation, such a small 
           magnitude of impact should warrant activists and those intersted in advocacy 
           to find other means of virtual activism and movements to supplement hashtags. 
           Overall, one could interpret this data to support the statement that hashtags 
           do not have a strong impact on legislation, but the relationship is nevertheless positive." 
           ),
           mainPanel(plotOutput("RegTable")),
           mainPanel(plotOutput("RegrPolicing")),
           mainPanel(plotOutput("RegrAssault")),
           mainPanel(plotOutput("RegrHarassment"))),
  tabPanel("Discussion",
           titlePanel("Do Hashtags Really Have an Impact on Legislation?"),
           p(" For this project, I attempted to understand the relationship 
           between hashtag popularity and legislation by using Google Trend Scores 
           for specific hashtags and bills brought to the New York Legislature 
           as proxies, respectively. In the age of digital social movements
           powered by hashtags and, trending terms, and limited means of activism
           for some due to health concerns and covid-19, it is of incredible importance
           to quantify and understand the impact hashtags might have on legislation,
           whether is pushes positive change concerning laws, bills, and governmental
           responsibility, and whether it facilitates enough change to be the only
           thing many are doing to create change in our national community.
           To further understand this relationship and answer 
           the question of a hashtag's impact on laws and bills, I made three 
           linear regression models to find correlation between proportions of 
           bills having to do with either harassment, assault, or policing and 
           popularity of hashtags via Google Trends Data. Each of these models 
           noted a less than 0.001 correlation between bills and hashtag popularity
           (.0006, .0006, .0002). While still a positive correlation, such a small 
           magnitude of impact should warrant activists and those intersted in advocacy 
           to find other means of virtual activism and movements to supplement hashtags. 
           Overall, one could interpret this data to support the statement that hashtags 
           do not have a strong impact on legislation, but the relationship is nevertheless positive. 
           ")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p(""),
           h3("About Me"),
           p("My name is Jasmine Hyppolite and I study Government and am part
           of the TechScience Program. You can reach me at
             jasminehyppolite@college.harvard.edu. Other projects can be found 
             at https://github.com/jashyppolite. ")))

server <- function(input, output) {
  output$billplot <- renderPlot({
    
    votes_clean %>%
      filter(bill_type == input$select) %>%
      ggplot(aes(x = month_year, y = bill_prop)) + 
      geom_point(color = "deepskyblue3") + 
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, size = 4)) + 
      labs(x = "Month and Year \n (January 2009 - October 2020)",
           y = "Proportion of Bills Relating to Topic",
           title = "Proportions of Bills Relating to Specific Topics Over Time in the New York Legislature")
  })
  
  output$GooglePlot <- renderPlot({
    
    trends %>%
      filter(ID == input$Trend) %>%
      ggplot(aes(x = month_year, y = mean_score)) + 
      geom_point(color = "cornflowerblue") + 
      geom_line(color = "gray") + 
      theme(axis.text.x = element_text(angle = - 90, vjust = 0.5, size = 6)) + 
      labs(title = "Google Trends for Frequency of # Searches by Month",
           x = "Month and Year \n (October 2015 - October 2020)",
           y = "Google Trend Score")
  })
  
  output$Comb_Plot <- renderPlot({
    marker <- ifelse(input$bill == "police_bill_prop", 
                     "blm", "metoo")
    trends_w_vote %>%
      filter(bill_type == input$bill & ID == marker) %>%
      ggplot(aes(x = mean_score, y = bill_prop)) + geom_point() + 
      geom_smooth(method = "lm", formula = y ~ x) + 
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
        label = "Slope: .0007", 
        x= 30,
        y= .055,
        color = "black") + 
      labs(title = "Correlation between Google Trends Score Regarding #BlackLivesMatter
          and Legislation Regarding Policing",
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
           x = "Average Google Trends Score per Month",
           y = "Harassment Bill Proportion by Month") +
      scale_x_continuous(labels = scales::label_number()) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_bw()
    
  })
  
  output$RegTable <- renderPlot({
    
    table <- huxreg("Police Model"  = police_pred,
           "Harassment Model" = hara_pred,
           "Assault Model" = assault_pred,
           coefs = c("Intercept" = "(Intercept)",
                     "Mean Google Trend Change" = "mean_score"),
           number_format = 5,
           statistics = c("Number of Observations" = "nobs"))
    HTML(huxtable::to_html(table))
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
