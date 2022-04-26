library(shiny)
#library(shinyalert)
library(timevis)
library(ggplot2)
library(dplyr)
library(stringr)

source('/srv/shiny-server/mwapi.R')


plotTimeline <- function(d, title, facet)
{
   if (facet != '')
      d <- d %>% filter(FactorsP == facet)
   else
      d <- d

   timevis(d, options = list(editable=FALSE, multiselect=FALSE, align="center"))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    #useShinyalert(),

    mainPanel(
     timevisOutput("timeLine")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   MAX_ROWS <- 1000
   count.data <- c()

   output$timeLine <- renderTimevis({
      query <- parseQueryString(session$clientData$url_search)
      #query <- list()

      #query[['title']] <- 'Timeline'
      title <- query[['title']]
      query[['address']] <- 'http://smw'
      query[['path']] <- ''
      #query[['prop1']] <- 'Death date'
      query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')
      #query[['linkProp']] <- '-Has Patient'
      #query[['prop2']] <- 'Result'
      #title <- 'Testo molto ...................................................................................... molto lungo, direi lunghissimo'

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
            d[, 1] <- paste(d[,1], d[,2], sep='\n')
            names(d)[1] <- 'title'
            names(d)[2] <- 'start'
         } else if (is.null(query[['prop1']])) {
            d <- query_smw_TSV(query[['address']], query[['db']], 'Samplings', c(query[['prop2']]), MAX_ROWS, 0)
            varname <- query[['prop2']]
            names(d)[1] <- 'ID'
            names(d)[2] <- 'Dates'
         } else {
            d <- query_smw_TSV(query[['address']], query[['db']], 'Patients', c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
            varname <- query[['prop1']]
            names(d)[1] <- 'ID'
            names(d)[2] <- 'FactorsP'
            names(d)[3] <- 'Factors'
         }

         d <- d[d$start != '',]
         #View(d)
         shiny::validate(need(nrow(d) > 1, "Not enough data"))

         if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
            plotTimeline(d, title, '')
         } else {
            count.data <<- d %>%
               count(Factors, FactorsP) %>%
               arrange(desc(Factors))

            P <- list()
            factors <- unique(d$FactorsP)
            for (factor in factors){
               p <- plotPie(d, title, factor)
               P <- c(P, list(p))
            }

            do.call(grid.arrange, c(P, nrow = length(factors), top=title))
         }
      } else {
         shiny::validate(need(F, "At least a property must be specified"))
      }

      #        print(ggplotly(p, 600, 600))
      #    scale_fill_manual(values=c(rgb(1, 0, 0), rgb(1, 1/3, 0), rgb(1, 2/3, 0), # red
      #                               rgb(1, 1, 0), rgb(2/3, 1, 0), rgb(1/3, 1, 0), # yellow
      #                               rgb(0, 1, 0), rgb(0, 1, 1/3), rgb(0, 1, 2/3), # green
      #                               rgb(0, 1, 1), rgb(0, 2/3, 1), rgb(0, 1/3, 1), # cyan
      #                               rgb(0, 0, 1), rgb(1/3, 0, 1), rgb(2/3, 0, 1), # blue
      #                               rgb(1, 0, 1), rgb(1, 0, 2/3), rgb(1, 0, 1/3), # purple
      #                               rgb(1, 0, 0), rgb(1, 1/3, 0)))
   })
}

# Run the application
shinyApp(ui = ui, server = server)
