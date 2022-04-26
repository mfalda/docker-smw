library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(RMariaDB)
library(stringr)
library(leaflet)
library(leaflet.extras) # for heatmaps
library(sp)

source('/srv/shiny-server/mwapi.R')


plotMap <- function(d, title, facet)
{
    if (facet != '')
        d <- d %>% filter(FactorsP == facet)
    else
        d <- d
        
    df = data.frame(
        lat = as.numeric(char2dms(d$latitude, chd="°", chm="'", chs="\"")),
        lng = as.numeric(char2dms(d$longitude, chd="°", chm="'", chs="\"")),
        color = 'red',
        label = paste(d$location, d$n, sep=': '),
        info = sprintf('<b>%s</b>: %s', d$location, d$n),
        weight = as.numeric(d$n) / sum(as.numeric(d$n))
    )

    m <- leaflet(df) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addHeatmap(lng=df$lng, lat=df$lat, 
                   max=1, radius=15,
                   intensity=df$n,
                   minOpacity=0.01, blur=20) %>%
        addCircleMarkers(
            lng = df$lng, # jitter(df$lng, factor = 10),
            lat = df$lat,
            radius = ~weight,
            color = ~color,
            opacity = 0.0,
            fillOpacity = 0.0,
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
            tabPanel("Locations", dataTableOutput("summary")),
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
        query[['cat']] <- 'Locations'
        query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')
 
        if (!is.null(query[['prop1']]) && query[['prop1']] == '(none)') {
            query[['prop1']] = NULL
        }
        if (!is.null(query[['prop2']]) && query[['prop2']] == '(none)') {
            query[['prop2']] = NULL
        }

        if (!is.null(query[['prop1']]) || !is.null(query[['prop2']])) {
            if (is.null(query[['prop2']])) {
                d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], paste(query[['prop1']], 'Coordinates', sep='.')), MAX_ROWS, 0)
                d <- separate(d, 'Coordinates', c('latitude', 'longitude'), sep=',')
                shiny::validate(need(!is.na(d), "Not logged in"))
                names(d)[1] <- 'ID'
                names(d)[2] <- 'location'
            } else if (is.null(query[['prop1']])) {
                d <- query_smw_TSV(query[['address']], query[['path']], 'Samplings', c(query[['prop2']]), MAX_ROWS, 0)
                d <- separate(d, 'Coordinates', c('latitude', 'longitude'), sep=',')
                shiny::validate(need(!is.na(d), "Not logged in"))
            } else {
                #d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
                shiny::validate(need(!is.na(d), "Not yet implemented"))
            }

            d <- na.omit(d)
            shiny::validate(need(nrow(d) > 1, "Not enough data"))
            
            if (is.null(query[['prop1']]) || is.null(query[['prop2']])) {
                d <- d %>%
                    mutate(location = strsplit(as.character(location), ",")) %>% 
                    unnest(location)
                count.data <<- d %>%
                    count(location, latitude, longitude)
                plotMap(count.data, title, '')
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
