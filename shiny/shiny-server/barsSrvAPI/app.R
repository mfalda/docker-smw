library(ggplot2)
library(shiny)
library(stringr)

source('/srv/shiny-server/prop_analysis.R')
source('/srv/shiny-server/mwapi.R')


plotBars <- function(d, title, facet, xlab, bins)
{
  p <- ggplot(d, aes(x=Values)) +
    geom_histogram(aes(y=..density..), bins=bins, colour="white", fill="darkgray") +
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title=title, x=xlab) +
    geom_vline(aes(xintercept=mean(Values)),
               color="blue", linetype="dashed", size=1) # Add mean line

  return(p)
}

ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("distPlot")),
          tabPanel("Counts", verbatimTextOutput("summary")),
          id = "tabs"
        )
      )
   )
)

server <- function(input, output, session) {
  MAX_ROWS <- 1000
  count.data <- c()

  output$distPlot <- renderPlot({
      query <- parseQueryString(session$clientData$url_search)

      title <- query[['title']]
      query[['address']] <- 'http://smw'
      query[['path']] <- ''
      query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')

      x <- c()

      if (!is.null(query[['prop1']]) && query[['prop1']] == '(none)') {
        query[['prop1']] = NULL
      }
      if (!is.null(query[['prop2']]) && query[['prop2']] == '(none)') {
        query[['prop2']] = NULL
      }

      if (!is.null(query[['prop1']]) || !is.null(query[['prop2']])) {
        if (is.null(query[['prop2']])) {
          d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']]), MAX_ROWS, 0)
          shiny::validate(need(!is.na(d), "Not logged in"))
        } else if (is.null(query[['prop1']])) {
          d <- query_smw_TSV(query[['address']], query[['path']], 'Samplings', c(query[['prop2']]), MAX_ROWS, 0)
          shiny::validate(need(!is.na(d), "Not logged in"))
        } else {
          #d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
          shiny::validate(need(False, "Not yet implemented"))
        }

        names(d)[1] <- 'ID'
        names(d)[2] <- 'Values'
        x <- as.numeric(d$Values)

        shiny::validate(need(length(x) > 1, "Not enough data"))
        if (!is.numeric(x))
          shiny::validate(need(F, "Data is not numeric"))
        bins <- as.numeric(input$bins) + 1

        if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
          count.data <<- x
          plotBars(d, title, '', query[['prop1']], bins)
        } else {
          P <- list()
          factors <- unique(count.data$Factors)
          for (factor in factors){
            p <- plotBars(d, title, factor, query[['prop1']], bins)
            P <- c(P, list(p))
          }

          do.call(grid.arrange, c(P, nrow = length(factors), top=title))
        }
      }
   })

   output$summary <- renderPrint({
       h <- hist(count.data, breaks = input$bins, plot=FALSE)
       data.frame('Bin' = levels(cut(count.data, breaks=h$breaks)),
                  'Density' = h$density
       )
   })
}

shinyApp(ui = ui, server = server)

