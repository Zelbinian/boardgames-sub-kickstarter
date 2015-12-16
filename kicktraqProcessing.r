require(rvest)
require(data.table)

# -------- functions -------------------------
processProjectInfo <- function(projects) {
    
    backers <- vector()
    funding <- vector()
    startDates <- vector()
    endDates <- vector()
    remaining <- vector()
    
    for (prj in projects) {
        prj <- prj[2:6]
        prj <- gsub('\t','', prj)
        
        splitData <- strsplit(prj, ": ")
        dates <- unlist(strsplit(splitData[[4]][2], " -> ")) # their date entry is "special"
        
        # the data is in the form Name: Value, so to get the value after we split it
        # we need to select the 2nd entry in each list
        backers <- c(backers, splitData[[1]][2])
        funding <- c(funding, splitData[[2]][2])
        startDates <- c(startDates, dates[1])
        endDates <- c(endDates, dates[2])
        remaining <- c(remaining, splitData[[5]][2])
    }
    
    return(list("backers"=backers, "funding"=funding, "startDates"=startDates, "endDates"=endDates, "remaining"=remaining))
}

ktrq_ending_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=end"
ktrq_new_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=new"
ktrq_page_mod <- "&page="
cur_page <- 1

ktrq_ending <- read_html(ktrq_ending_url) %>% html_nodes(".project-infobox")

# The project details, annoyingly, are just a text blob
prj_details <- ktrq_ending %>%                      #data source
                html_node(".project-details") %>%   #selects the div with the project details in it
                html_text() %>%                     #pulling the text out
                strsplit('\n')                      #storing each peice of data separately

prj_info <- processProjectInfo(prj_details)