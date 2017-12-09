## app.R ##
library(plotly)
library(dplyr)
library(shiny)
library(htmlwidgets)

uwMemes <- read.csv("data/uwmemes1209_facebook_statuses.csv", stringsAsFactors = FALSE)
ucMemes <- read.csv("data/ucmemes1209_facebook_statuses.csv", stringsAsFactors = FALSE)
harvardMemes <- read.csv("data/harvardelitist1209_facebook_statuses.csv", stringsAsFactors = FALSE)

############## COMMENT OUT WHEN PUBLISHING ##################
#ucMemes <- head(ucMemes)
#uwMemes <- head(uwMemes)
#harvardMemes <- head(harvardMemes)
##############################################################

renderPlotly2 <- function (expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {
    expr <- substitute(expr)
  }
  shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}

addHoverBehavior <- "function(el, x){
el.on('plotly_hover', function(data){
var infotext = data.points.map(function(d){
console.log(d)
return (' type: '+d.data.stype[d.pointNumber] + ' Author: '+d.data.sauth[d.pointNumber]+' Message: '+d.data.smsg[d.pointNumber]+
' Perma Link: '+ d.data.sperma[d.pointNumber]);
});
console.log(infotext)
Shiny.onInputChange('hover_data', infotext)
})
}"

server <- function(input, output) {
  output$tabUi <- renderUI({
    if (input$tab == "scatter") {
      uiList <- list(
        selectInput("memeFile", label = ("Select facebook meme group"), 
                    choices = list("UW Memes" = "uwMemes", "UC Memes" = "ucMemes", "Harvard Memes" = "harvardMemes")),
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
      )
    } else if (input$tab == "box") {
      uiList <- list(
        selectInput("memeFile", label = ("Select facebook meme group"), 
                    choices = list("UW Memes" = "uwMemes", "UC Memes" = "ucMemes", "Harvard Memes" = "harvardMemes"))
      )
    } else if (input$tab == "histogram") {
      uiList <- list(
        selectInput("memeFile", label = ("Select facebook meme group"), 
                    choices = list("UW Memes" = "uwMemes", "UC Memes" = "ucMemes", "Harvard Memes" = "harvardMemes")),
        selectInput("histCount", label = "Select histogram count", 
                    choices = list("Post Author" = 'status_author', "Date" = 'status_published', "Media Type" = 'status_type'))
      )
    }
    return(uiList)
  })
  
  output$hover <- renderText({
    input$hover_data
  })


  output$memePlot <- renderPlotly2({
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
    
    p <- plot_ly(memeData, x = ~get(input$memeXvar), y = ~get(input$memeYvar), type = "scatter", mode = "markers", 
           stype = ~status_type, sauth = ~status_author, smsg = ~status_message, sperma = ~permalink_url) %>% 
      layout(xaxis = x, yaxis = y, title = paste("Memes from", input$memeFile), margin = m) 
    as.widget(p) %>% onRender(addHoverBehavior)
    
  })
  
  output$boxReaction <- renderPlotly({
    plot_ly(get(input$memeFile), x = ~num_reactions, type = "box", name = "total reactions") %>% 
      add_trace(x = ~num_wows, name = "wow") %>% 
      add_trace(x = ~num_loves, name = "love") %>% 
      add_trace(x = ~num_angrys, name = "angry") %>% 
      add_trace(x = ~num_hahas, name = "hahas") %>% 
      add_trace(x = ~num_likes, name = "like") %>% 
      add_trace(x = ~num_sads, name = "sad") %>% 
      layout(title = paste("Memes from", input$memeFile)) 
  })
  
  output$histPlot <- renderPlotly({
    x <- list(
      title = paste(input$histCount, " "),
      tickangle = 45,
      zeroline = FALSE
    )
    y <- list(
      title = paste("Number of Frequncy"),
      zeroline = FALSE
    )
    m <- list(
      b = 160,
      t = 50
    )
    
    memeData <- get(input$memeFile) 
    memeData$status_published <- as.Date(memeData$status_published, "%Y-%m-%d")
    plot_ly(memeData, x = ~get(input$histCount), type = "histogram") %>% 
      layout(xaxis = x, yaxis = y, title = paste("Memes from", input$memeFile), margin = m) 
  })
}

ui <- fluidPage(
  titlePanel("Meme Research"),
  h3("Data from Facebook groups were last scrapped on 12-9-2017."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("tabUi")

    ),
    mainPanel(
      tabsetPanel(id = "tab",
        tabPanel(title = "Scatter Plot", value = "scatter", plotlyOutput("memePlot"),
                 textOutput("hover")),
        tabPanel(title = "Box Plot", value = "box", plotlyOutput("boxReaction")),
        tabPanel(title = "Histogram Chart", value = "histogram", plotlyOutput("histPlot"))
      ))
  )
)

shinyApp(ui = ui, server = server)