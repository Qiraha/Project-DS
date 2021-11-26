# Title     : TODO
# Objective : TODO
# Created by: Qiraha
# Created on: 21/11/2021

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(tidytext)
library(stringr)
library(bslib)
library(ggthemes)
library(thematic)
thematic::thematic_shiny()


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "darkly"),
  title = "Sentiment Analysis about Tweets Surrounding Ted Cruz and A.O.C",
  headerPanel(""),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h3("Sentiment Analysis about Tweets Surrounding Ted Cruz and A.O.C")
      ),
      br(),
      fluidRow(
        column(width = 12, "Risky Aulia Nugraha / 123190002")
      ),
      fluidRow(
        column(width = 12, "Afifrendra Rifqi Nugraha / 123190012")
      ),
      br(), br(),
      sliderInput(inputId = "data_n",
                  label = "Total Data",
                  value = 50,
                  min = 20,
                  max = 100
      ),
      actionButton("refresh_button", "Refresh"
      )
    ),
    mainPanel(
      fluidRow(
        column(width = 6, plotOutput("pie_plot", height = "210px", width = "390px")),
        column(width = 6, plotlyOutput("d_plot", height = "220px", width = "400px"))
      ),
      fluidRow(
        tabsetPanel(type = "tab",
                    id = "tabset",
                    tabPanel("All Data", dataTableOutput("table_all")),
                    tabPanel("Cleaned Data", dataTableOutput("cleaning_data")),
                    tabPanel("Sentiment Result", dataTableOutput("sentiment")),
                    tabPanel("Negative WordCloud", wordcloud2Output("wordcloud_neg", height = "400px", width = "800px")),
                    tabPanel("Positive WordCloud", wordcloud2Output("wordcloud_pos", height = "400px", width = "800px"))
        )
      ),
      fluidRow(
        column(width = 6, h3("Negative WordCloud")),
        column(width = 5, h3("Positive WordCloud"))
      ),
      fluidRow(
        column(width = 6, plotlyOutput("word_neg", height = "300px", width = "380px")),
        column(width = 5, plotlyOutput("word_pos", height = "300px", width = "380px")),
      ),
    )
  )
)

#server
server <- function(input, output, session) {
  
  observeEvent(input$refresh_button, {
    source("App.R")
  })

  d_view <- reactive({
    total_count = input$data_n
    return(total_count)
  })

  dp <- reactiveFileReader(1000, session, "data-raw/predict_data_all.csv", read.csv)
  result_predict <- reactiveFileReader(1000, session, "data-raw/predict_result.csv", read.csv)
  dp_clean <- reactiveFileReader(1000, session, "data-raw/predict_data_clean.csv", read.csv)



  positive_wc <- reactive({
    dp_clean() %>%
      top_n(d_view()) %>%
      left_join(result_predict(), by = "id") %>%
      filter(sentiment == "Positive") %>%
      select(id, text.x, sentiment) %>%
      unnest_tokens(word, text.x) %>%
      count(word, sort = TRUE) %>%
      head(20)
  })

  wordplot_pos <- reactive({
    positive_wc() %>%
      head(10) %>%
      ggplot(aes(x = word, y = n, fill = word)) +
      geom_col() +
      geom_text(aes(label = n), color = "red") +
      labs(
        x = "",
        y = "Total"
      ) +
      coord_flip()
  })

  negative_wc <- reactive({
    dp_clean() %>%
      top_n(d_view()) %>%
      left_join(result_predict(), by = "id") %>%
      filter(sentiment == "Negative") %>%
      select(id, text.x, sentiment) %>%
      unnest_tokens(word, text.x) %>%
      count(word, sort = TRUE) %>%
      head(20)
    
  })

  wordplot_neg <- reactive({
    negative_wc() %>%
      head(10) %>%
      ggplot(aes(x = word, y = n, fill = word)) +
      geom_col() +
      geom_text(aes(label = n), color = "red") +
      labs(
        x = "",
        y = "Total"
      ) +
      coord_flip() 
  })


  output$pie_plot <- renderPlot({
    result_predict_count <- result_predict() %>%
      head(d_view()) %>%
      count(sentiment)
    prop <- round(result_predict_count$n * 100 / sum(result_predict_count$n), 1)
    ggplot(result_predict_count, aes(x = "", y = prop, fill = sentiment)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = cumsum(prop) - 0.99 * prop, label = paste0(prop, "%")), color = "yellow", size = 5) +
      labs(
        x = "",
        y = "Percentage"
      )
  })
  
  output$d_plot <- renderPlotly({
      result_predict_count <- result_predict() %>%
        head(d_view()) %>%
        count(sentiment)
      result_predict_count %>%
        ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
        geom_col() +
        geom_text(aes(label = n), color = "red") +
        labs(
          x = "",
          y = "Total"
        ) 
      
  })


  output$table_all <- renderDataTable({
    dp() %>% head(d_view()) %>% select(-sentiment)
  })

  output$cleaning_data <- renderDataTable({
    dp_clean() %>% head(d_view())
  })

  output$sentiment <- renderDataTable({
    result_predict() %>% head(d_view())
  })

  output$wordcloud_neg <- renderWordcloud2({
    wordcloud2(negative_wc(), size = 0.6, backgroundColor = "#222222")
  })

  output$wordcloud_pos <- renderWordcloud2({
    wordcloud2(positive_wc(), size = 0.6, backgroundColor = "#222222")
  })

  output$word_neg <- renderPlotly({
    ggplotly(wordplot_neg())
  })

  output$word_pos <- renderPlotly({
    ggplotly(wordplot_pos())
  })
}

#call shiny app
shinyApp(ui = ui, server = server, option = list(height = "500px"))
