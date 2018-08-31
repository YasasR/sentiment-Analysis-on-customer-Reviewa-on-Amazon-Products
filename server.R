
#get required libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)
library(sentimentr)
library(shiny)
library(shinydashboard)
library(ggplot2);
library(wordcloud);
library(reshape2);
library(rsconnect)

## set Working Directory
#setwd("E:/cloud/ProjectR/AmazonReviews/ProductReviews")

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Functions
  analizeP <- function(productN) {
    reviewForProduct<- filter(sentReviews,productName==productN) 
    return(reviewForProduct)
  }
  
  analizeB <- function(brandN) {
    reviewForProduct<- filter(sentReviews,brand==brandN) 
    return(reviewForProduct)
  }
  analizeC <- function(CategoryN) {
    reviewForProduct<- filter(sentReviews,categories==CategoryN) 
    return(reviewForProduct)
  }
  
  
  
  plot_review_Products<- function(dataFrame){
    dataFrame %>%
      group_by(productName) %>%
      mutate(word_count = 1:n(),
             index = word_count %/% 50 + 1) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(productName, index = index , sentiment) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(index, sentiment, fill = productName)
      ) +
      geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ productName , ncol = 2, scales = "free_x")
    
  }
  plot_review_Brands<- function(dataFrame){
    dataFrame %>%
      group_by(brand) %>%
      mutate(word_count = 1:n(),
             index = word_count %/% 50 + 1) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(brand, index = index , sentiment) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(index, sentiment, fill = brand)
      ) +
      geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ brand , ncol = 2, scales = "free_x")
    
  }
  
  plot_review_Categories<- function(dataFrame){
    dataFrame %>%
      group_by(categories) %>%
      mutate(word_count = 1:n(),
             index = word_count %/% 50 + 1) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(categories, index = index , sentiment) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(index, sentiment, fill = categories)
      ) +
      geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ categories , ncol = 2, scales = "free_x")
    
  }
  plot_wordCloudpos<-function(dataframe){
    dataframe%>%
      inner_join(get_sentiments("bing")%>%
                   filter(sentiment == "positive")) %>%
      count(word, sentiment, sort = TRUE) %>%
      with(wordcloud(word, n, max.words = 100,colors = c("#40DF1D","#16EAC7","#4A16EA"),random.color=TRUE,scale = c(4,0.5),fixed.asp=TRUE,use.r.layout=TRUE))
    
  }
  
  plot_wordCloudneg<-function(dataframe){
    dataframe%>%
      inner_join(get_sentiments("bing")%>%
                   filter(sentiment == "negative")) %>%
      count(word, sentiment, sort = TRUE) %>%
      with(wordcloud(word, n, max.words = 100,colors = c("#EA3616","#F1A50D","#F10D0D"),random.color=TRUE,scale = c(4,0.5),fixed.asp=TRUE,use.r.layout=TRUE))
    
  }
  
  
  ##Output Settings 
  
  output$brandRw<-renderPlot(
    sentReviews %>%
      group_by(brand) %>% 
      mutate(word_count = 1:n(),
             index = word_count %/% 100 + 1) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(brand, index = index , sentiment) %>%
      ungroup() %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(index, sentiment, fill = brand),
             brand = factor(brand, levels = brand)) +
      geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ brand, ncol = 2, scales = "free_x")
  )
  
  output$Consent<-renderPlot(
    bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  )
  output$wordCloud_All<-renderPlot(
    sentReviews%>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#DF431D", "#0A2EBF"),
                       max.words = 100,scale = c(4,0.5))
    
  )
  # Products
  output$plotOneProduct<-renderPlot(
    plot_review_Products(analizeP(input$pname))
    
  )
  output$wordColudPosforsp<-renderPlot(
    plot_wordCloudpos(analizeP(input$pname))
    
  )
  output$wordColudNegforsp<-renderPlot(
    plot_wordCloudneg(analizeP(input$pname))
    
  )
  
  # Brands
  output$plotOneBrand<-renderPlot(
    plot_review_Brands(analizeB(input$pbrand))
    
  )
  output$wordColudPosforbd<-renderPlot(
    plot_wordCloudpos(analizeB(input$pbrand))
    
  )
  output$wordColudNegforbd<-renderPlot(
    plot_wordCloudneg(analizeB(input$pbrand))
    
  )
  
  # Categories
  output$plotOneCat<-renderPlot(
    plot_review_Brands(analizeC(input$pcat))
    
  )
  output$wordColudPosforcat<-renderPlot(
    plot_wordCloudpos(analizeC(input$pcat))
    
  )
  output$wordColudNegforcat<-renderPlot(
    plot_wordCloudneg(analizeC(input$pcat))
    
  )
 
  
})
