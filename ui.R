#get required libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)
library(sentimentr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(wordcloud)
library(reshape2)

## loading review Data set
data<-read.csv("Data/review.csv",stringsAsFactors = FALSE)


#tidying data
reviewdata <- data %>%
  select(name,categories,brand,reviews.rating,reviews.text,reviews.title,reviews.date) %>%
  filter(!is.na(reviews.date)) 
names(reviewdata) <- c("productName","categories","brand","reviewRating","reviewText","reviewTitle","reviewDate")
reviewdata$reviewRating<- as.numeric(reviewdata$reviewRating)

Products <- data_frame(unique(reviewdata$productName))
Brand<-data_frame(unique(reviewdata$brand))
Categories<-data_frame(unique(reviewdata$categories))

reviews_All <- data_frame(reviewdata$reviewText)
reviews_All <- reviews_All[!reviews_All$`reviewdata$reviewText`=="",]
names(reviews_All) <- "word"

reviews_All <- reviews_All %>%
  unnest_tokens(word,word)

data(stop_words)

#remove certain words from being seen as negative (funny, plot, silly)
c_stop_words <- bind_rows(stop_words)

#Remove stop words ("the","of", "to", etc)
reviews_All <- reviews_All %>%
  anti_join(c_stop_words)

sentReviews <- reviewdata %>%
  group_by(productName) %>%
  mutate(
    product = productName) %>% 
  ungroup() %>%
  unnest_tokens(word,reviewText)




#Compare positive and negative words
bing_word_counts <- sentReviews %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()
Products <- data_frame(unique(reviewdata$productName))
Brand<-data_frame(unique(reviewdata$brand))
Categories<-data_frame(unique(reviewdata$categories))


#Header
header <- dashboardHeader(title = "Review_Dashboard ")


#side Bar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("ProductReviews", tabName = "ProductReviews", icon = icon("balance-scale",lib = "font-awesome"))
    
    
    
  )
)


#body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Reviews for Brands ", solidHeader = TRUE,collapsible = TRUE,background = "blue",
                plotOutput("brandRw")
              ),
              box(
                title = "Contribution to sentiment", solidHeader = TRUE,collapsible = TRUE,background = "navy",
                plotOutput("Consent")
              )
              
              
            ),
            fluidRow(
              box(
                title = "WordCloud", solidHeader = TRUE,height ="500px",collapsible = TRUE,background = "green",
                plotOutput("wordCloud_All")
              )
              
            )
            
            
    ),
    tabItem(tabName = "ProductReviews",
            fluidRow(
              tabBox(
                title = "Product Selection",
                id = "tabset1", height = "150px",width = "1000px",
                tabPanel("Product Name", 
                         selectInput("pname","Select Item",choices =Products$`unique(reviewdata$productName)`,width = "750px"),
                         fluidRow(
                           tabBox(
                             title = "Analysis",
                             id = "analise", height = "550px",width = "1000px",
                             tabPanel("Sentiment" ,plotOutput("plotOneProduct")
                                      
                             ),
                             tabPanel("Word Cloud", 
                                      tabBox(
                                        title = "",
                                        id = "coudType", height = "150px",width = "1000px",
                                        tabPanel("Positive",plotOutput("wordColudPosforsp")
                                                 
                                        ),
                                        tabPanel("Negative",plotOutput("wordColudNegforsp")
                                                 
                                        )
                                        
                                      )
                             )
                           )
                         ) 
                ),
                tabPanel("Brand", 
                         selectInput("pbrand","Select Item",choices =Brand$`unique(reviewdata$brand)`,width = "750px"),
                         fluidRow(
                           tabBox(
                             title = "Analysis",
                             id = "analise", height = "550px",width = "1000px",
                             tabPanel("Sentiment" ,plotOutput("plotOneBrand")
                                      
                             ),
                             tabPanel("Word Cloud", 
                                      tabBox(
                                        title = "",
                                        id = "coudType", height = "150px",width = "1000px",
                                        tabPanel("Positive",plotOutput("wordColudPosforbd")
                                                 
                                        ),
                                        tabPanel("Negative",plotOutput("wordColudNegforbd")
                                                 
                                        )
                                        
                                      )
                             )
                           )
                         ) 
                ),
                tabPanel("Categories", 
                         selectInput("pcat","Select Item",choices =Categories$`unique(reviewdata$categories)`,width = "750px"),
                         fluidRow(
                           tabBox(
                             title = "Analysis",
                             id = "analise", height = "550px",width = "1000px",
                             tabPanel("Sentiment" ,plotOutput("plotOneCat")
                                      
                             ),
                             tabPanel("Word Cloud", 
                                      tabBox(
                                        title = "",
                                        id = "coudType", height = "150px",width = "1000px",
                                        tabPanel("Positive",plotOutput("wordColudPosforcat")
                                                 
                                        ),
                                        tabPanel("Negative",plotOutput("wordColudNegforcat")
                                                 
                                        )
                                        
                                      )
                             )
                           )
                         ) 
                )
              )
            )
            
    )
  )
)



ui <- dashboardPage(header, sidebar, body)