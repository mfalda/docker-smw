library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(RMariaDB)
library(stringr)
library(leaflet)
library(leaflet.extras) # heatmaps

source('/srv/shiny-server/mwapi.R')


plotMap <- function(d, coords, title, facet)
{
    if (facet != '')
        d <- d %>% filter(FactorsP == facet)
    else
        d <- d

    data <- d %>%
        inner_join(coords)
       
    df = data.frame(
        lat = data$lat,
        lng = data$long,
        color = 'red',
        label = paste(data$name, data$n, sep=': '),
        info = sprintf('<b>%s</b><br/>: %s', 
                       data$name, data$n),
        weight = as.numeric(data$n) / sum(as.numeric(data$n))
    )
    
    tile_url <- "http://localhost:8081/images/WomanAnatShape/{z}/woman_anat_{z}_{y}_{x}.png"
    tile_attrib <- "&copy; <a href='https://www.pngitem.com/middle/bmwJRJ_human-body-integumentary-system-diagram-hd-png-download/'>Lena Liskova</a>"
    
    cat(as.integer(df$weight * 50))
    m <- leaflet(df) %>%
        setView(-64.57031428813936, 39.81528571537707, 2) %>%
        addTiles(urlTemplate = tile_url,
                 attribution = tile_attrib,
                 options = tileOptions(minZoom = 2, maxZoom = 4, continuousWorld = T)
        ) %>%
        addHeatmap(lng=df$lng, lat=df$lat, 
                   radius=as.integer(df$weight * 50),
                   max=1, minOpacity=0.1, blur=20) %>%
        addCircleMarkers(
            lng = df$lng, # jitter(df$lng, factor = 10),
            lat = df$lat,
            radius = as.integer(df$weight * 50),
            color = ~color,
            label = ~label,
            popup = ~info
            #clusterOptions = markerClusterOptions()
        )
    m
}

ui <- fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", leafletOutput("map")),
            tabPanel("Model Summary", dataTableOutput("summary")),
            id = "tabs"
        )
    )
)

server <- function(input, output, session) {
    MAX_ROWS <- 1000
    count.data <- c()

    output$map <- renderLeaflet({
        query <- parseQueryString(session$clientData$url_search)

        title <- query[['title']]
        query[['address']] <- 'http://smw'
        query[['path']] <- ''
        query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')

        # load pseudo-coordinates (in production they are from a real DB)
        coords <- data.frame(
            name=c('Category:Conjunctivitis', 'Category:Cough', 'Category:Diarrhea', 'Category:Headache', 'Category:Rhinitis', 'Category:Sore throat'),
            lat=c(73.173, 45.205, -24.057, 79.779, 70.173, 66.438),
            long=c(-94.778, -72.275, -85.636, -86.692, -86.692, -86.339)
        )

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
                names(d)[2] <- 'name'
            } else if (is.null(query[['prop1']])) {
                d <- query_smw_TSV(query[['address']], query[['path']], 'Samplings', c(query[['prop2']]), MAX_ROWS, 0)
                names(d)[1] <- 'ID'
                names(d)[2] <- 'Factors'
            } else {
                #d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
                shiny::validate(need(!is.na(d), "Not yet implemented"))
            }

            d <- na.omit(d)
            shiny::validate(need(nrow(d) > 1, "Not enough data"))
            
            if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
                d <- d %>%
                    mutate(name = strsplit(as.character(name), ",")) %>% 
                    unnest(name)
                count.data <<- d %>%
                    count(name)
                plotMap(count.data, coords, title, '')
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

    output$summary <- renderDataTable({
       count.data
    })
}

shinyApp(ui = ui, server = server)
