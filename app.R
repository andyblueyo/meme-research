## app.R ##
library(plotly)
library(dplyr)
library(shiny)

uwMemes <- read.csv("data/uwmemesboundlessteens_facebook_statuses.csv", stringsAsFactors = FALSE)
ucMemes <- read.csv("data/ucberkeleymemes_facebook_statuses.csv", stringsAsFactors = FALSE)


server <- function(input, output) {
  

  output$memePlot <- renderPlotly({
    maxYVal <- paste0(input$memeYvar, ">=", input$memeReactionSlide[1])
    minYVal <- paste0(input$memeYvar, "<=", input$memeReactionSlide[2])
    
    memeData <- get(input$memeFile) %>% filter_(maxYVal) %>% filter_(minYVal)
    
    x <- list(
      title = paste(input$memeXvar, " "),
      tickangle = 45,
      zeroline = FALSE
    )
    y <- list(
      title = paste(input$memeYvar, " "),
      zeroline = FALSE
    )
    m <- list(
      b = 160,
      t = 50
    )
    
    plot_ly(memeData, x = ~get(input$memeXvar), y = ~get(input$memeYvar), type = "scatter", mode = "markers") %>% 
      layout(xaxis = x, yaxis = y, title = paste("Memes from ", input$memeFile), margin = m)
    
  })


}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("memeFile", label = ("Select facebook meme group"), 
                  choices = list("UW Memes" = "uwMemes", "UC Memes" = "ucMemes")),
      sliderInput("memeReactionSlide", label = ("Range of Y Values:"), min = 0, 
                  max = 12000, value = c(300, 800)),
      selectInput("memeXvar", label = ("Select X Variable:"), 
                  choices = list("Date" = 'status_published', "Number of Reactions" = 'num_reactions', "Number of Comments" = 'num_comments',
                                 "Number of Shares" = 'num_shares', "Number of Likes" = 'num_likes', "Number of Loves" = 'num_loves', "Number of Wows" = 'num_wows',
                                 "Number of Hahas" = 'num_hahas', "Number of Sads" = 'num_sads', "Number of Angrys" = 'num_angrys'),
                  selected = "status_published"),
      selectInput("memeYvar", label = ("Select Y Variable:"), 
                  choices = list("Number of Reactions" = 'num_reactions', "Number of Comments" = 'num_comments',
                                 "Number of Shares" = 'num_shares', "Number of Likes" = 'num_likes', "Number of Loves" = 'num_loves', "Number of Wows" = 'num_wows',
                                 "Number of Hahas" = 'num_hahas', "Number of Sads" = 'num_sads', "Number of Angrys" = 'num_angrys'),
                  selected = "num_reactions")

    ),
    mainPanel(plotlyOutput("memePlot"))
  )
)

shinyApp(ui = ui, server = server)