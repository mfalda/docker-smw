library(shiny)
library(ggplot2)
library(dplyr) 
library(stringr)
library(scales)

source('/srv/shiny-server/prop_analysis.R')
source('/srv/shiny-server/mwapi.R')


collect_others <- function(d, num)
{
    id <- names(d)[1]
    d %>% 
        group_by_(id) %>%
        summarise(n_rows = length(id)) %>%
        arrange(desc(n_rows)) %>%
        mutate(label = case_when(row_number() <=  num ~ id, 
                             row_number() > num ~ 'all other'))
    
    return(d)
}

plotHBars <- function(count.data, title)
{
  unif <- rep(sum(count.data$n) / length(count.data$n), length(count.data$n))
  pa <- prop_analysis(count.data$n, unif)
  count.data$props <- pa[[1]]
  count.data$p.values <- pa[[2]]
  
  p <- ggplot(count.data, aes(x="", y=n, fill=factor(label))) + 
    geom_bar(position = "dodge", stat = "identity", color = "white") +
    geom_text(aes(label=n, y = lab.ypos), position=position_dodge(0.9), color = "white") +
    theme_classic() +
    theme(plot.title = element_text(color="black", hjust = 0, size=14, face="bold")) + 
    labs(fill = "label",
         x = "label",
         y = "Counts",
         title = title) + 
    coord_flip() +
    scale_fill_discrete(labels=sprintf("%s: %s (p-value=%s)", count.data$label, 
                                       count.data$props, count.data$p.values))
  
  return(p)
}

ui <- fluidPage(
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("hbarsPlot")),
        tabPanel("Model Summary", verbatimTextOutput("summary")),
        id = "tabs"
      )
    )
)

server <- function(input, output, session) {
  MAX_ROWS <- 1000
  MAX_CATS <- 3
  count.data <- NA

  output$hbarsPlot <- renderPlot({
        query <- parseQueryString(session$clientData$url_search)

        title <- query[['title']]
        query[['address']] <- 'http://smw'
        query[['path']] <- ''
        query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')

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
            names(d)[1] <- 'ID'
            names(d)[2] <- 'Factors'
          } else if (is.null(query[['prop1']])) {
            d <- query_smw_TSV(query[['address']], query[['path']], 'Samplings', c(query[['prop2']]), MAX_ROWS, 0)
            shiny::validate(need(!is.na(d), "Not logged in"))
            names(d)[1] <- 'ID'
            names(d)[2] <- 'Factors'
          } else {
            #d <- query_smw_TSV(query[['address']], query[['path']], 'Patients', c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
            shiny::validate(need(!is.na(d), "Not yet implemented"))
          }
          
          d <- d[!is.na(d$Factors) & d$Factors != '',]

          shiny::validate(need(nrow(d) > 1, "Not enough data"))
          
          if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
            count.data <<- d %>%
              count(Factors) %>%
              arrange(desc(Factors)) %>%
              mutate(label = case_when(row_number() <  MAX_CATS ~ as.character(Factors), 
                                         row_number() >= MAX_CATS ~ 'all other'))
            
            count.data <<- count.data %>%
              count(label) %>%
              arrange(label) %>%
              mutate(lab.ypos = 0.5*n)

            plotHBars(count.data, title)
          }
        } else {
          shiny::validate(need(F, "At least a property must be specified"))
        } 
    })
    
    output$summary <- renderPrint({
      count.data
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)
