library(WikipediR)
source('/srv/shiny-server/credentials.R')


get_all_properties <- function(host, wiki, limit=5000)
{
  logged.in <- login(paste(host, wiki, 'api.php?action=ask&query=&format=json', sep='/'), username, password)
  if (!logged.in) {
    return(NA)
  }
  
  url <- sprintf("%s/%s/api.php?action=browsebyproperty&limit=%d&format=json", host, wiki, limit)

  response <- httr::GET(url)
  
  #Check the validity of the response
  httr::stop_for_status(response)
  # Convert the response into a character vector, check for API errors
  response_text <- httr::content(x = response, as = "text")
  
  result <- fromJSON(response_text)
  
  props <- lapply(result$query, function(x) x$label)
  
  return(props)
}

query_smw_TSV <- function(host, wiki, category, printouts, limit=5000, offset=0, verbose=F)
{
  logged.in <- login(paste(host, wiki, 'api.php?action=ask&query=&format=json', sep='/'), username, password)
  if (!logged.in) {
    return(NA)
  }

  # http://smw/wiki/index.php?title=Special:Ask/-5B-5BCategory:Samplings-5D-5D/-3FHas-20Patient%3DParent/-3FNumber%3Dnumber/-3FDate/-3FInterval/-3FResult/-3FRandom-20score/-3FDelta-20from-20last-20score/-3FRandom-20symptom-20locations-20at-20sampling/limit%3D50/offset%3D0/format%3Dcsv
  # http://smw/wiki/index.php?title=Special:Ask/[[Category:Positives]]/?Has Patient=Parent/?Contact/?Type/?Random location of contact/limit=50/format=csv
  category <- URLencode(category)
  printouts <- URLencode(paste(printouts, collapse='/?'), reserved=F)
  # "URLencode" question marks in printouts
  printouts <- gsub("?", "-3F", printouts, fixed=T)
  url <- sprintf("%s/%s/index.php?title=Special:Ask/-5B-5BCategory:%s-5D-5D/-3F%s/format=csv/offset=%d/limit=%d",
                           host, wiki, category, printouts, offset, limit)
  if (verbose) {
    cat(url, "\n")
  }
  response <- httr::GET(url)

  #Check the validity of the response
  httr::stop_for_status(response)
  # Convert the response into a character vector, check for API errors
  response_text <- httr::content(x = response, as = "text")

  return(read.csv(text=response_text))
}

test_url <- function()
{
  csv <- query_smw_TSV('http://smw', '', 'Positives', c('Has Patient', 'Contact', 'Type', 'Random location of contact'), 50, 0)

  return(csv)
}

# from https://raw.githubusercontent.com/jocelynpender/fna-query/master/R/src/query.R
# to be tested
run_query <- function(host, wiki, category, printouts, limit, verbose=F)
{
  # Run a query against the Semantic MediaWiki http api URL and obtain results back in R
  offset <- 0
  results <- data.frame()
  limit1 <- min(limit, 250)
  while (!is.null(offset) == TRUE) { # While there continues to be an offset...
    login(url, username, password)
    results1 <- query_smw_TSV(host, wiki, category, printouts, limit1, offset)
    if (nrow(results1) <= 1)
      break
    if (nrow(results) == 0) {
      results <- results1
    } else {
      results <- rbind.data.frame(results, results1)
    }
    if (verbose) {
      print(paste("Appending batch", offset, "to query results")) # Give an indication of progress of the download
    }
  }

  return(results)
}
