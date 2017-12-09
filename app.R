## app.R ##
library(plotly)
library(dplyr)
library(shiny)

uwMemes <- read.csv("data/uwmemesboundlessteens_facebook_statuses.csv", stringsAsFactors = FALSE)
ucMemes <- read.csv("data/ucberkeleymemes_facebook_statuses.csv", stringsAsFactors = FALSE)
ucMemes <- head(ucMemes)

server <- function(input, output) {

  output$memePlot <- renderPlotly({
    
    plot_ly(get(input$memeFile), x = ~get(input$memeXvar), y = ~get(input$memeYvar), type = "scatter", mode = "markers")
  })


}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("memeFile", label = ("Select facebook meme group"), 
                  choices = list("UW Memes" = "uwMemes", "UC Memes" = "ucMemes")),
      sliderInput("memeReactionSlide", label = ("Range of X Values:"), min = 0, 
                  max = 3000, value = 80),
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