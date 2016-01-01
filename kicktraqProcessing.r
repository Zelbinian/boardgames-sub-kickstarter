require(rvest)
require(magrittr)
require(lubridate)

# -------- functions -------------------------
processProjectInfo <- function(projects) {
    
    backers <- vector()
    funding <- vector()
    avgPledges <- vector()
    endDates <- as.Date(vector())   # yeah, I know
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
        avgPledges <- c(avgPledges, splitData[[3]][2])
        # using lubridate to make the date stuff less onerous
        # but we need just a regular Date class because otherwise the timezone
        # stuff gets really weird, and we don't know the timezone so we shouldn't store it
        endDates <- c(endDates, as.Date(mdy(dates[2])))
        remaining <- c(remaining, splitData[[5]][2])
    }
    
    return(list("backers"=backers, "funding"=funding, "startDates"=startDates, 
                "endDates"=endDates, "remaining"=remaining))
}

scrape <- function(url) {
    # we're scraping from paginated data, so we these variables will help traverse that
    currentUrl <- url
    pageMod <- "&page="
    page <- 1
    
    # data frame the function will return
    output <- data.frame("Title"=character(),"Description"=character(),
                         "Backers"=numeric(),"Funding Status"=character(),
                         "Project Start"=numeric(),"Project End"=numeric(),
                         "Time Remaining"=character())
    
    repeat{
        webdata <- read_html(currentUrl) %>% html_nodes(".project-infobox")
        
        # The project details, annoyingly, are just a text blob
        prj_details <- webdata %>%                      #data source
            html_node(".project-details") %>%   #selects the div with the project details in it
            html_text() %>%                     #pulling the text out
            strsplit('\n')                      #storing each peice of data separately
        
        prj_info <- processProjectInfo(prj_details)
        
        output <- rbind(output, 
                        data.frame("Title"=webdata %>% html_node("a") %>% html_text(),
                                "Description"=webdata %>% html_node("div") %>% html_text(),
                                "Backers"=prj_info$backers,
                                "Funding Status"=prj_info$funding,
                                "Project Start"=prj_info$startDates,
                                "Project End"=prj_info$endDates,
                                "Time Remaining"=prj_info$remaining))
        
        # we only need 7 days worth of data, so if we've got that we're done
        if (max(output$Project.End) > today() + days(7)) {
            break;
        } else {
            # assemble new url for scraping
            page <- page + 1
            currentUrl <- paste0(url, pageMod, page)
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(1)
        }
    }
    
    return(output[output$Project.End <= (today() + days(7)),])
}

# wrapping the script with a function because that seems right
scrapeKicktraq <- function(type) {
    
    url = "http://www.kicktraq.com/categories/games/tabletop%20games?sort="
    
    # yeah, this is shit, but i'm not publishing this, i'm just protecting
    # against typos
    type <- tolower(gsub(" ", "", type, fixed = TRUE))
    if (!(type %in% c("end","new"))) break;
    
    return(scrape(paste0(url, type)))
}

# -------- processing boardgame kickstarter projects --------------
#kicktraqEnding <- scrapeKicktraq("end")
#kicktraqNew <- scrapeKicktraq("new")

cat("Game|Status|Project Ends|Extra\n:--|:--|:--|:--", file = "kspost.md", append = TRUE)
for(i in 1:nrow(kicktraqEnd)) {
    with(kicktraqEnd[i,],
         # to make it easy to read, each line below is a column in the table
         cat("**[",as.character(Title),"](http://blank)** ",as.character(Description),"|",
         Funding.Status,"|",
         Project.End,"|",
         "extra  \n",sep = "",
         file = "kspost.md", append = TRUE)
    )
}