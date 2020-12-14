#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries 
library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)
library(huxtable)

# files needed for site (images, datasets for graphing, etc )
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

# Define UI for application

ui <- navbarPage(
  "Trends of Social Movements and Bills Concerning Similar Issues",
  tabPanel("About",
           
           # Here I add a theme to the website
           
           fluidPage(theme = shinytheme("flatly"),
                     
                     # Next I set the panel title and also use h3 to make 
                     # another title under that to basically title the 
                     # paragraph 
                     
                     titlePanel("Project Background and Motivations"),
                     h3("Social Movements, Hashtags, and Change"),
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
           and harassment, but prevent the loss of innocent life due to racism and sexism.",
                       
           # After trial and error with "\n," I went online and found this 
           # function which helps me line break 
           
           tags$br(),
           tags$br(),
                       
            "In an ideal world with more time and resources, this project would go further to 
            information regarding the popularity of hashtags on Twitter as well as Google, and 
            span farther in time. Additionaly, it would be great to expand the data sets regarding 
            bills in legislatures beyond New York State and compile data from legislatures across
                       the entire nation.",
           
           # I use these line breaks to make a separate paragraph and help 
           # the page look less messy 
           
           tags$br(),
           tags$br(),
           "My name is Jasmine Hyppolite and I study Government and am part
           of the TechScience Program. You can reach me at
             jasminehyppolite@college.harvard.edu. Other projects can be found 
             at https://github.com/jashyppolite. "),
           )),
  
  # Here I make another panel on the page to dissect the bill data 
  tabPanel("Bill Proportions",
           fluidPage(
                     titlePanel("Proportion of Bill Types in NY Legislature"),
                     p("The data regarding New York Legislature bills comes from several combined
             LegiScan datasets of bills brought up within the Legislature across several years. 
               The data represented here spans from 2009 to 2020. Bills were codified to be
               related to assault, harassment, or police using word detection capabilities."),
                     sidebarLayout(
                       sidebarPanel(
                         
                         # Because I want to allow the user to interact with
                         # this page and data, I needed to pivot longer my 
                         # original data set and change the way the graph is 
                         # built such that what the user chooses is what the
                         # plot initially filters by. The choices here are the
                         # bill types analyzed througout this study
                         
                         selectInput("select",
                                     "Bill Type: ",
                                     choices = c("police_bill_prop", "hara_bill_prop",
                                                 "assault_bill_prop")),
                       ),
                       
                       # Here I insert the plot into the page such that it is 
                       # visible
                       
                       mainPanel(plotOutput("billplot"))),
           )),

  # Here an additional page is made to dissect the Google Trends Data
  
  tabPanel("Google Trends",
           
           # Here the title is given (alternate to the tab panel) to give a
           # bit more information 
           
           titlePanel("Google Trends Hashtag Data over Time"),
           fluidPage(
             p("To interpret the first spikes for each movement, it is helful to 
             know that the Black Lives Matter Movement began to gain 
               traction in 2013 after the murder of Trayvon Martin. The 
               MeToo movement gained traction over social media
               after Alyssa Milano tweeted the hashtag in 
               October of 2017."),
             sidebarLayout(
               sidebarPanel(
                 
                 # aAain, I use this select input function such that the user
                 # can interact with the page and change what they see. 
                 selectInput("Trend",
                             "Hashtag",
                             
                            # the choices here correlate with the data set
                            # which also had to be pivoted longer
                            
                             choices = c("blm", "metoo")),
               ),
               
               # I also make sure that the plot shows up and changes with the
               # input the user selects
               
               mainPanel(plotOutput("GooglePlot"))),
           )),
  
  # The next page pulls the data from the two previous datas sets together
  
  tabPanel("Google Trends and Bill Proportions",
           
           # I decided to test this page without a title and felt it looked 
           # much more organized, so I left it blank
           
           titlePanel(""),
           fluidPage(
             titlePanel("Google Trends of Social Movements and Bills in NY"),
             p("Note that the harassment and asssault bills are to be interpreted 
                 with the #MeToo Movement in mind and police bills with the 
                 #BlackLivesMatter Movement in mind."),
             sidebarLayout(
               sidebarPanel(
                 
                 # Here the user can choose which relationship they would like
                 # to be plotted (linear model). The bill type in my original
                 # data set also had to be pivoted longer
                 
                 selectInput("bill",
                             "Bill Type",
                             choices = c("police_bill_prop", "hara_bill_prop", "assault_bill_prop")),
               ),
               
               # Again, the plot is pasted here such that it can be seen on 
               # the page
               
               mainPanel(plotOutput("Comb_Plot"))),
           )),
  
  # This page is mostly writing and for analysis
  
  tabPanel("Models & Analysis",
           titlePanel("Statistical Analysis: Do Hashtags Really Impact Legislation?"),
           p("Our statistical analysis for relationship between Google Trends Data
           regarding the Black Lives Matter and Me Too hashtags and bills brought up 
           within the NY Legislature consisted of a linear regression model. For this 
           project, three linear regression models were built: The first regresses the 
           proportion of bills in the NY legislature that have to do with policing on
           the mean Google Trends Score for the Black Lives Matter hashtag per month. 
           This model's output indicated that for every 1 point increase in the Google Trend Score for #Black Lives Matter,
           there is a .0007 increase in the proportion of bills discussed regarding policing. This is a very small
           impact. To better understand these values, I also scaled the model to standard deviations
           and found a .271 standard deviation in the police bill proportion variable to 
           be associated with a 1 standard deviation increase in the Google Trend Score variable. Additionally,
           the 95% confidence interval for this model lies within the range of .00004360374 to .001273162, 
           not only includes the coefficient output from this study, but also does not include zero.",
           
             # The line break function is used here again to separate 
             # discussions of different models
             
           tags$br(),
           tags$br(),
           
           "The second regresses the proportion of bills in the NY legislature that have to do with assault on
           the mean Google Trend Score for the MeToo hashtag per month. This second model's 
           output indicated that for every 1 point increase in the Google Trend Score for #MeToo,
           there is a .0002 increase in the proportion of bills discussed regarding assault. Similar to the first
           model, this is a very small impact. To make this result a bit more interesting, I also 
           scaled the model to standard deviations. This highlighted a .294 standard deviation increase the in 
           my assault bill proportion variable associated with a 1 standard deviation increase in
           Google Trend Score variable. Additionally,
           the 95% confidence interval for this model lies within the range of .0003057542 - .0009817151. While it is
           important that the confidence interval does not include zero, it is also important to note that 
           it includes the output coefficient from the model from this study.", 
           
           tags$br(),
           tags$br(),
           
           "Finally, the third model regresses the proportion of bills in the NY legislature that have to do with harassment on
           the mean Google Trends score for the Me Too hashtag per month. The final model's 
           output indicated that for every 1 point increase in the Google Trend Score for #Black Lives Matter,
           there is also a .0006 increase in the proportion of bills discussed regarding harassment Similar to the previous findings,
          this is a very small impact, however I still wanted to scale the model to get more information. In scaling
          this model, I found a .435 standard deviation increase associated with a 1 standard deviation increase
          in the Google Trend Score variable. The 95% confidence interval for this model falls between the values
           of .00002425167 - .0002911265, which is a range that excludes the coefficient output received from the model 
           used in this study.", 
           
           tags$br(),
           tags$br(),
           
           "In conclusion, to further understand this relationship and answer 
           the question of a hashtag's impact on laws and bills, I made three 
           linear regression models to find correlation between proportions of 
           bills having to do with either harassment, assault, or policing and 
           popularity of hashtags via Google Trends Data. Each of these models 
           noted a less than 0.001 correlation between bills and hashtag popularity
           (.0006, .0006, .0002). While still a positive correlation, such a small 
           magnitude of impact should warrant activists and those intersted in advocacy 
           to find other means of virtual activism and movements to supplement hashtags. It is 
           also important to note that only the police and assault model values and fell in the 95% confidence interval.
           Overall, one could interpret this data to support the statement that hashtags 
           do not have a strong impact on legislation, but the relationship is nevertheless positive."
         
           ),
           
           # during office hours, I worked with Ishan to figure out how to get 
           # a huxreg table to show, as I have multiple models to show, not 
           # just one, and the table we learned about in the class could not 
           # give me that outcome. So we worked to actually take a photo of 
           # how the graph nicely showed up in the console and use it by 
           # converting it to HTML and have it show on the page
           
           includeHTML("table.html"),
           mainPanel(plotOutput("RegrPolicing")),
           mainPanel(plotOutput("RegrAssault")),
           mainPanel(plotOutput("RegrHarassment")),
           
           # Unfortunately, while calculating confidence intervals, I wasn't
           # able to add it to the previous table as that function did not have
           # something in the statistic argument for confidence intervals. So
           # thanks so what I learned in office hours, I was able to 
           # troubleshoot myself
           
           includeHTML("CITable.html")),
  
  # This final page interprets the findings of this study with a "big picture"
  # perspective and is just text
  
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
           
           My understanding of this relationship and answer to 
           the question of a hashtag's impact on laws and bills, is based on three 
           linear regression models providing information regarding the relationship between proportions of 
           bills having to do with either harassment, assault, or policing and 
           popularity of hashtags via Google Trends Data. Each of these models 
           noted a less than 0.001 correlation between bills and hashtag popularity
           (.0006, .0002, .0007). While a nevertheless positive correlation, such a small 
           magnitude of impact should warrant activists and those interested in advocacy 
           to find other means of virtual activism and movements to supplement hashtags.
           There is no universal scale or rule of thumb for what quantity of magnitude an 
           effort towards change for a social movement should have, and the effects of
           physically-based social movements have not been quantified in a similar fashion.
           However, one ten-thousandth of a unit of change per millions of voices of social media
           seems too small to be find comfort in being completely dependent on a hashtag for social
           justice.
           "))
  )

server <- function(input, output) {
  output$billplot <- renderPlot({
    
    # To begin plotting this first graph, I pipe from the appropriate data set
    # and filter by whichever condition the user chooses
    
    votes_clean %>%
      filter(bill_type == input$select) %>%
      ggplot(aes(x = month_year, y = bill_prop)) + 
      geom_point(color = "deepskyblue3") + 
      
      # Because there are several dates to represent in this plot, I turned 
      # the elements on the x axis sideways and made it smaller
      # to make more space
      
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, size = 4)) + 
      labs(x = "Month and Year \n (January 2009 - October 2020)",
           y = "Proportion of Bills Relating to Topic",
           title = "Proportions of Bills Relating to Specific Topics 
           Over Time in the New York Legislature")
  })
  
  output$GooglePlot <- renderPlot({
    
    # To begin plotting this = graph, I pipe from the appropriate data set
    # and filter by whichever condition the user chooses
    
    trends %>%
      filter(ID == input$Trend) %>%
      ggplot(aes(x = month_year, y = mean_score)) + 
      geom_point(color = "cornflowerblue") + 
      geom_line(color = "gray") +
      
      # Again, the elements on the x axis are made smaller and turned sideways
      # such that it looks less cluttered
      
      theme(axis.text.x = element_text(angle = - 90, vjust = 0.5, size = 6)) + 
      labs(title = "Google Trends for Frequency of # Searches by Month",
           x = "Month and Year \n (October 2015 - October 2020)",
           y = "Google Trend Score")
  })
  
  output$Comb_Plot <- renderPlot({
 
    # this plot was a bit more tricky because I needed each bill type to 
    # correlate with either black lives matter or me too. With Mitchell, I 
    # created a marker object that assigned the input of the user to either
    # blm or metoo
    
       marker <- ifelse(input$bill == "police_bill_prop", 
                     "blm", "metoo")
       
    # As usual, we begin by piping from the appropriate data set
       
    trends_w_vote %>%
      
      # I then filter by the input the user selects AND the ID marker such that
      # the plot shows up accurately
      
      filter(bill_type == input$bill & ID == marker) %>%
      ggplot(aes(x = mean_score, y = bill_prop)) + geom_point() + 
      
      # This addition of geom_smooth allows me to visualize the linear model
      # which I would like the user to see as well
      
      geom_smooth(method = "lm", formula = y ~ x) + 
      labs(title = "Correlation Between Bills in NY Regarding Bill Type
           and Google Trend Scores",
           x = "Mean Google Trend Score for Movement",
           y = "Proportion of Bills Per Month")

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

  output$RegTable <- renderTable({

    table <- huxreg("Police Model"  = police_pred,
           "Harassment Model" = hara_pred,
           "Assault Model" = assault_pred,
           coefs = c("Intercept" = "(Intercept)",
                     "Mean Google Trend Change" = "mean_score"),
           number_format = 5,
           statistics = c("Number of Observations" = "nobs"))
    left_border(table)[,1] <- 0.4
    right_border(table)[,4] <- .4

    #table
    HTML(huxtable::to_html(table))
  
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
