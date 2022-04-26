library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)
library(scales)

source('/srv/shiny-server/prop_analysis.R')
source('/srv/shiny-server/mwapi.R')


plotPie <- function(d, title, facet)
{
    if (facet != '')
        d <- d %>% filter(FactorsP == facet)
    else
        d <- d

    count.data <- d %>%
        count(Factors) %>%
        arrange(desc(Factors)) %>%
        mutate(lab.ypos = cumsum(n) - 0.5*n)

    unif <- rep(sum(count.data$n) / length(count.data$n), length(count.data$n))
    pa <- prop_analysis(count.data$n, unif)
    count.data$props <- pa[[1]]
    count.data$p.values <- pa[[2]]

    p <- ggplot(count.data, aes(x="", y=n, fill=factor(Factors))) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        geom_text(aes(y = lab.ypos, label = paste(round(n / sum(n) * 100, 2), '%')), color = "white") +
        #geom_col(position = 'stack', width = 1) +
        #geom_text(aes(label = paste(round(values / sum(values) * 100, 1), "%"), x = 1.3),
        #          position = position_stack(vjust = 0.5)) +
        theme_classic() +
        theme(plot.title = element_text(color="black", hjust = 0, size=14, face="bold"),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(fill = ifelse(facet != '', paste("Factors for ", facet), 'Factors'),
             x = NULL,
             y = NULL,
             title = '') +
        guides(fill = guide_legend(reverse=T)) +
        coord_polar("y") +
        scale_fill_discrete(labels=sort(sprintf("%s: %s (p-value=%s)", count.data$Factors,
                                                count.data$props, count.data$p.values),
                                        decreasing=F))

    return(p)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    #useShinyalert(),
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("piePlot")),
            tabPanel("Model Summary", verbatimTextOutput("summary")),
            id = "tabs"
        )
    )
)

# Define server logic required to draw a plot
server <- function(input, output, session) {
    MAX_ROWS <- 1000
    count.data <- c()

    output$piePlot <- renderPlot({
        query <- parseQueryString(session$clientData$url_search)
        #query <- list()

        title <- query[['title']]
        query[['address']] <- 'http://smw'
        query[['path']] <- '' # '/wiki'
        #query[['prop1']] <- 'Gender'
        query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')
        #query[['linkProp']] <- '-Has Patient'
        #query[['prop2']] <- 'Result'

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
                # cat1 to be fixed
                d <- query_smw_TSV(query[['address']], query[['path']], query[['cat1']], c(query[['prop2']]), MAX_ROWS, 0)
                varname <- query[['prop2']]
                names(d)[1] <- 'ID'
                names(d)[2] <- 'Factors'
            } else {
                d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
                varname <- query[['prop1']]
                names(d)[1] <- 'ID'
                names(d)[2] <- 'FactorsP'
                names(d)[3] <- 'Factors'
                View(d)
            }

            shiny::validate(need(nrow(d) > 1, "Not enough data"))

            if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
                count.data <<- d %>%
                    count(Factors) %>%
                    arrange(desc(Factors))

                plotPie(d, title, '')
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
    })

    output$summary <- renderPrint({
       count.data
    })
}

# Run the application
shinyApp(ui = ui, server = server)

