library(shiny)
#library(shinyalert)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

source('/srv/shiny-server/mwapi.R')


fit <- c()

plotScatter <- function(x, y, title, x.lab, y.lab, facet)
{
  data <- data.frame(x=x, y=y)
  plot(y ~ x)
  fit <<- lm(y ~ x)
  abline(fit, cex=2, col='red')
  m <- summary(fit)$coefficients[,1][2]
  q <- summary(fit)$coefficients[,1][1]
  r2 <- summary(fit)$r.squared
  #cat(file=stderr(), "DATA: '", x, "' rows: ", length(x), "\n", sep="")
  
  #shinyalert("Information", paste("DATA: '", x, "' rows: ", length(x), sep=""), type = "info")
  
  p <- ggplot(data, aes(x=x, y=y, fill=x)) + 
    geom_point() +
    geom_rug() + 
    geom_smooth(method=lm, color="red", fill="blue") +
    labs(x = x.lab,
         y = y.lab,
         title = sprintf('%s vs %s (%.3G x + %.3G, R^2 = %.3G)', x.lab, y.lab, m, q, r2))
  
  return(p)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    #useShinyalert(),
    mainPanel(
     tabsetPanel(
       tabPanel("Plot", plotOutput("scatterPlot")),
       tabPanel("Model Summary", verbatimTextOutput("summary")),
       id = "tabs"
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  MAX_ROWS <- 1000
  
  output$scatterPlot <- renderPlot({
     query <- parseQueryString(session$clientData$url_search)
     #query <- list()
     
     query[['address']] <- 'http://smw'
     query[['path']] <- '' # '/wiki'
     #query[['prop1']] <- 'Age group'
     query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')
     #query[['prop2']] <- 'Age group'
     query[['prop2']] <- str_replace_all(query[['prop2']], ' ', '_')
     #query[['linkProp']] <- '-Has Patient'
     #query[['prop3']] <- 'Result'

     #title <- 'Testo molto ...................................................................................... molto lungo, direi lunghissimo'
     
     if (!is.null(query[['prop1']]) && query[['prop1']] == '(none)') {
       query[['prop1']] = NULL
     }
     if (!is.null(query[['prop2']]) && query[['prop2']] == '(none)') {
       query[['prop2']] = NULL
     }
     
     if (!is.null(query[['prop1']]) || !is.null(query[['prop2']])) {
       if (is.null(query[['prop3']])) {
         d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], query[['prop2']]), MAX_ROWS, 0)
         shiny::validate(need(!is.na(d), "Not logged in"))
         names(d)[1] <- 'ID'
         names(d)[2] <- 'X'
         names(d)[3] <- 'Y'
       } else {
         d <- query_smw_TSV(query[['address']], query[['db']], 'Patients', c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
         names(d)[1] <- 'ID'
         names(d)[2] <- 'X'
         names(d)[3] <- 'Y'
         names(d)[4] <- 'Factors'
         View(d)
       }
       
       shiny::validate(need(nrow(d) > 10, "Not enough data"))
       
       x <- as.numeric(d$X)
       y <- as.numeric(d$Y)
       #shinyalert("Information", paste("DATA: ", x, ", rows: ", length(x), sep=""), type = "info")
       if (!is.numeric(x))
         shiny::validate(need(F, "Data in x is not numeric"))
       if (!is.numeric(y))
         shiny::validate(need(F, "Data in y is not numeric"))
       
       if (is.null(query[['prop3']])) {
         plotScatter(x, y, title, query[['prop1']], query[['prop2']], '')
       } else {
         P <- list()
         factors <- unique(d$Factors)
         for (factor in factors){
           p <- plotScatter(d$X, d$Y, title, query[['prop1']], query[['prop2']], factor)
           P <- c(P, list(p))
         }
         
         do.call(grid.arrange, c(P, nrow = length(factors), top=title))
       }
     } else {
       shiny::validate(need(F, "At least two properties must be specified"))
     }             
   })
  
  output$summary <- renderPrint({
    summary(fit)
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

