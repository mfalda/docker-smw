library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)
library(scales)
library(lmtest)
library(car)

source('/srv/shiny-server/prop_analysis.R')
source('/srv/shiny-server/mwapi.R')


data <- data.frame()
res_aov <- c()

asplit <- function (x, MARGIN)
{
    dl <- length(dim(x))
    if (!dl)
        stop("dim(x) must have a positive length")
    if (is.object(x))
        x <- if (dl == 2L)
            as.matrix(x)
        else as.array(x)
    d <- dim(x)
    dn <- dimnames(x)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn)))
            stop("'x' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    newx <- aperm(x, c(s.call, s.ans))
    dim(newx) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    for (i in seq_len(d2)) {
        ans[[i]] <- array(newx[, i], d.call, dn.call)
    }
    array(ans, d.ans, dn.ans)
}

plotBox <- function(x, y, title, facet, xlab, ylab, test.type)
{
   data <<- data.frame(Factor=x, Property=y)

   res_aov <<- aov(y ~ x)
   p.value <- res_aov
   method1 <- "ANOVA"
   method2 <- "t.test"

   if (test.type == 'o') {
      p.value <- unlist(summary(res_aov))["Pr(>F)1"]
   } else if (test.type == 'e') {
      res <- oneway.test(
         y ~ x, var.equal = FALSE # Welch ANOVA
      )
      p.value <- res$p.value
   } else if (test.type == 'n') {
      method1 <- "Kruskal-Wallis"
      res <- kruskal.test(y ~ x)
      method2 <- "wilcox.test"
      p.value <- res$p.value
   }

   p <- ggplot(data, aes(x=Factor, y=Property, fill=Factor)) +
      geom_boxplot(notch=T, outlier.colour="red", outlier.shape=8,
                   outlier.size=2, color="black") +
      geom_jitter(shape=16, alpha = 0.25, width = 0.2) +
      labs(x = xlab,
           y = ylab,
           title = sprintf('%s (%s, p-value: %.3G)', title, method1, p.value))
   x <- which(names(data) == 'Factor')
   y <- which(
      names(data) == 'Property'
   )

   cmb <- t(combn(unique(data$Factor), 2))
   my_comparisons <- asplit(cmb, 1) # comparisons for post-hoc tests

   for (i in y) {
      for (j in x) {
         print(
            p + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
         )
      }
   }

   return(p)
}

ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         radioButtons("test", "Test:",
                      c("Omoscedastic" = "o",
                        "Eteroscedastic" = "e",
                        "Non parametric" = "n")
         )
      ),
      mainPanel(
         tabsetPanel(
            tabPanel("Plot", plotOutput("boxPlot")),
            tabPanel("Density plot", plotOutput("dPlot")),
            tabPanel("QQ-Plot", plotOutput("qqPlot")),
            tabPanel("Statistics",
                     dataTableOutput("summary"),
                     verbatimTextOutput("normTest"),
                     verbatimTextOutput("dwTest"),
                     verbatimTextOutput("leveneTest"),
                     verbatimTextOutput("pairwiseTest")),
            id = "tabs"
         )
      )
   )
)

server <- function(input, output, session) {
   MAX_ROWS <- 1000

   output$boxPlot <- renderPlot({

     test.type <- input$test

     query <- parseQueryString(session$clientData$url_search)
     title <- query[['title']]
     query[['address']] <- 'http://smw'
     query[['path']] <- ''
     query[['prop1']] <- str_replace_all(query[['prop1']], ' ', '_')
     query[['prop2']] <- str_replace_all(query[['prop2']], ' ', '_')

     if (!is.null(query[['prop1']]) && query[['prop1']] == '(none)') {
        query[['prop1']] = NULL
     }
     if (!is.null(query[['prop2']]) && query[['prop2']] == '(none)') {
        query[['prop2']] = NULL
     }

     if (!is.null(query[['prop1']]) && !is.null(query[['prop2']])) {
        if (is.null(query[['prop3']])) {
           d <- query_smw_TSV(query[['address']], query[['path']], query[['cat']], c(query[['prop1']], query[['prop2']]), MAX_ROWS, 0)
           shiny::validate(need(!is.na(d), "Not logged in"))
           names(d)[1] <- 'ID'
           names(d)[2] <- 'Values'
           names(d)[3] <- 'Factors'
        } else {
           #d <- query_smw_TSV(query[['address']], query[['path']], 'Samplings', c(query[['prop1']], query[['prop2']], paste(query[['linkProp']], query[['prop2']], sep='.')), MAX_ROWS, 0)
           shiny::validate(False, "Not yet implemented"))
        }

        shiny::validate(need(nrow(d) > 1, "Not enough data"))

        x <- d$Factors
        y <- as.numeric(d$Values)
        if (!is.numeric(y))
           shiny::validate(need(F, "Data in y is not numeric"))

        if (is.null(query[['prop3']])) {
           plotBox(x, y, title, '', query[['prop1']], query[['prop2']], test.type)
        }
     } else {
        shiny::validate(need(F, "At least a property must be specified"))
     }
   })

   output$dPlot <- renderPlot({
      hist(res_aov$residuals, main='Distribution of the residuals', xlab='Residuals', breaks=15)
   })

   output$qqPlot <- renderPlot({
      ggqqplot(res_aov$residuals)
   })

   output$summary <- renderDataTable({
      data %>%
         group_by(Factor) %>%
         summarise(
            mean = mean(Property, na.rm = TRUE),
            sd = sd(Property, na.rm = TRUE)
         )
   })

   output$normTest <- renderPrint({
      shapiro.test(res_aov$residuals)
   })
   output$dwTest <- renderPrint({
      dwtest(data$Property ~ data$Factor)
   })
   output$pairwiseTest <- renderPrint({
      test.type <- input$test
      if (test.type == 'o') {
         TukeyHSD(res_aov)
      } else if (test.type == 'e') {
         pairwise.t.test(data$Property, data$Factor,
                      p.adjust.method = "BH", pool.sd = FALSE)
      } else if (test.type == 'n') {
         pairwise.wilcox.test(data$Property, data$Factor,
                           p.adjust.method = "BH")
      }
   })
}

shinyApp(ui = ui, server = server)

