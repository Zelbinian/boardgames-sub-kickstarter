# -------- setup procedures ------------------

# checking for required packages, installing if necessary
reqPackages <- c("rvest", "magrittr", "lubridate")
newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

library(rvest)
library(magrittr)
library(lubridate)

# -------- functions -------------------------
parseStartDate <- function(asIsDate) {
    startDate <- as.Date(parse_date_time(asIsDate, "Bd"))
    curDate <- Sys.Date()
    
    if (month(startDate)==12 && month(curDate)==1) {
        year(startDate) <- year(curDate) - 1
    }
    
    return(startDate)
}

processProjectInfo <- function(projects, ktURLs) {
    
    backers <- vector()
    funding <- vector()
    avgPledge <- vector()
    startDates <- as.Date(vector())
    endDates <- as.Date(vector())   # yeah, I know
    remaining <- vector()
    ksURLs <- vector()
    
    for (prj in projects) {
        prj <- prj[2:6]
        prj <- gsub('\t','', prj)
        
        splitData <- strsplit(prj, ": ")
        dates <- unlist(strsplit(splitData[[4]][2], " -> ")) # their date entry is "special"
        
        # the data is in the form Name: Value, so to get the value after we split it
        # we need to select the 2nd entry in each list
        backers <- c(backers, splitData[[1]][2])
        funding <- c(funding, splitData[[2]][2])
        fundingAmt <- trimws(sub("\\(.*\\)", "", funding))
        fundingPcnt <- trimws(sub("[^(]+\\(([^)]+)\\).*", "\\1", funding))
        # using lubridate to make the date stuff less onerous
        # but we need just a regular Date class because otherwise the timezone
        # stuff gets really weird, and we don't know the timezone so we shouldn't store it
        startDates <- c(startDates, parseStartDate(dates[1]))
        endDates <- c(endDates, as.Date(mdy(dates[2])))
        remaining <- c(remaining, splitData[[5]][2])
    }
    
    
    
    # this is based on the assumption that the urls and the projects come in the same order...
    # ... and they do!
    for(url in ktURLs) {
        
        projectPage <- read_html(paste0("http://www.kicktraq.com",url))
        
        # occasionally project pages get removed, so we have to protect against that
        # using html_nodes instead of html_node helps with this because it doesn't return errors
        thisKsUrl <- projectPage %>% html_nodes("#button-backthis") %>% html_attr("href")
        
        if (length(thisKsUrl) > 0) {
            ksURLs <- c(ksURLs, thisKsUrl)
            
            projectPageInfo <- projectPage %>%  
                html_nodes("#project-info-text") %>%   #selects the div with the project details in it
                html_text() %>%                     #pulling the text out
                strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
                unlist(.)
            
            # the pledge is listed in the 9th line
            # if we substring on the location of the colon + 2, that will reliably get the dollar value
            avgPledge <- c(avgPledge, 
                           projectPageInfo[9] %>% 
                               substring(., gregexpr(pattern = ':',.) %>% unlist(.) + 2))
            
        } else {
            ksURLs <- c(ksURLs, NA)
            avgPledge <- c(avgPledge, NA)
        }
        
        Sys.sleep(1) # try not to hammer their server
    }
    
    return(list("url"=ksURLs,"backers"=backers, "fundingAmt"=fundingAmt, "fundingPcnt"=fundingPcnt,
                "avgPledge"=avgPledge, "startDates"=startDates, "endDates"=endDates,
                "remaining"=remaining))
}

scrapeKicktraqPage <- function(url) {
    webdata <- read_html(url) %>% html_nodes(".project-infobox")
    
    # The project details, annoyingly, are just a text blob, so need to parse them out
    prj_details <- webdata %>%                      #data source
        html_node(".project-details") %>%   #selects the div with the project details in it
        html_text() %>%                     #pulling the text out
        strsplit('\n')                      #storing each peice of data separately
    
    # this is the meaty function, the thing that actually processes the scraped data
    ktURLs <- webdata %>% html_node("h2 a") %>% html_attr("href")
    prj_info <- processProjectInfo(prj_details, ktURLs)
    
    return(data.frame("Title"=webdata %>% html_node("h2 a") %>% html_text(),
               "URL"=prj_info$url,
               "Description"=webdata %>% html_node("div") %>% html_text(),
               "Backers"=prj_info$backers,
               "Funding Amount"=prj_info$fundingAmt,
               "Funding Percent"=prj_info$fundingPcnt,
               "Average Pledge"=prj_info$avgPledge,
               "Project Start"=prj_info$startDates,
               "Project End"=prj_info$endDates,
               "Time Remaining"=prj_info$remaining,
               "Kicktraq URL"=ktURLs))
}

createPostHeader <- function(outputFile) {
    cat("## What this is:\n\nThis is a weekly, curated listing of Kickstarter tabletop games projects",
        "that are either:\n\n- **newly posted in the past 7 days**, or\n- **ending in the next 7 days (starting tomorrow)**",
        "and have at least a fighting chance of being funded.\n\nAll board game projects meeting",
        "those criteria will automatically be included, no need to ask. (But the occasional non-board game project may also sneak in!)\n\n",
        "Expect new lists each Sunday sometime between 12:00am and 12:00pm PST.\n*****\n", file = outputFile, append = FALSE)
}

createPostBody <- function(section, outputFile, data, sort = F) {
    section <- tolower(section)
    acceptableSections <- c('new','end')
    if(!(section %in% acceptableSections)) stop(paste(section,"is an invalid specifier."))
    
    # sorting data, if required
    if(sort) data <- data[with(data, order(as.character(Title))),]
    
    # write the appropriate section header
    if(section == 'new') {
        cat("## New This Week\n", file = outputFile, append = TRUE)
    } else {
        cat("## Ending Soon\n", file = outputFile, append = TRUE)
    }
    
    # posts a formatted version of the passed in data to the output file 
    cat("Project Info|Status|Backers|Avg Pledge|Ending|Comments\n:--|:--|:--|:--|:--|:--\n", file = outputFile, append = TRUE)
    for(i in 1:nrow(data)) {
        with(data[i,],
             # to make it easy to read, each line below is a column in the table
            cat("**[",as.character(Title),"](",as.character(URL),")** ",as.character(Description)," *(Has currently earned ",as.character(Funding.Amount),")*","|",
                as.character(Funding.Percent),"|",
                as.character(Backers),"|",
                as.character(Average.Pledge),"|",
                as.character(strftime(Project.End, format = "%m-%d")),"|",
                sep = "", file = outputFile, append = TRUE)
        )
        
        if (section == 'end') {
            cat("[kicktraq](",as.character(paste0("http://www.kicktraq.com",data[i,]$Kicktraq.URL)),")", 
                sep = "", file = outputFile, append = TRUE)
        }
        
        cat("\n", file = outputFile, append = TRUE)
    }
    
}
    
createPostFooter <- function(outputFile) {
    cat("*****\n", file = outputFile, append = TRUE)
    cat("Looking for more comprehensive Kickstarter gaming information? ",
        "Check out [the meta listings on BGG](https://boardgamegeek.com/geeklist/166152/kickstarter-project-metalist),",
        "explore [Kicktraq's data-driven views](https://www.kicktraq.com/categories/games/tabletop%20games/),", 
        "or, of course, [Kickstater's Tabletop Category](https://www.kickstarter.com/discover/categories/games/tabletop%20games?ref=category).\n",
        file = outputFile, append = TRUE)
    cat("*****\n", file = outputFile, append = TRUE)
    cat("## Footnotes\n", file = outputFile, append = TRUE)
    cat("* `#hmm` means that something about the project seems a little off. Buyer beware kinda thing.\n", file = outputFile, append = TRUE)
    cat("* `#lolwut` is reserved for projects that seem like trainwrecks. Check 'em out for amusement.\n", file = outputFile, append = TRUE)
    cat("* `#take` tags are for projects that have been restarted for some reason, with the number indicating what iteration we're currently on.\n", file = outputFile, append = TRUE)
    cat("* Did I miss something? Particularly stuff that might go in the Comments column? Let me know and I'll add it in.", file = outputFile, append = TRUE)
}

integerTest <- function(toTest){
    
    if(class(toTest) == "numeric" && toTest%%1 == 0 && toTest > -1) {
        TRUE
    } else {
        FALSE
    }
}

createKsPost <- function(type="both", begDate = today(), outputFile="kspost.md",
                           baseUrl="http://www.kicktraq.com/categories/games/tabletop%20games?sort=",
                           startPage=1, newWindow=7, endWindow=8, saveData = T) {
    
    # argument validation
    # type
    type <- tolower(gsub(" ", "", type, fixed = TRUE))
    if (!(type %in% c("end","new", "both"))) stop("Type argument must be one of 'end', 'new', or 'both' (case insensitive).");
    
    # startPage
    if(!integerTest(startPage)) stop("startPage must be a non-negative integer")
    
    # newWindow
    if(!integerTest(newWindow)) stop("newWindow must be a non-negative integer")
    
    # endWindow
    if(!integerTest(endWindow)) stop("endWindow must be a non-negative integer")
    
    # we'll let read_html validate the url for us
    pageMod <- "&page="
    
    createPostHeader(outputFile)
    
    # because we want to iteratively build a data frame, it's helpful to start with an
    # empty shell version of it such that we can write one test that is guaranteed to
    # fail the first time
    endData <- newData <- data.frame("Title"=character(),
                                     "URL"=character(),
                                     "Description"=character(),
                                     "Backers"=numeric(),
                                     "Funding Amount"=character(),
                                     "Funding Percent"=character(),
                                     "Average Pledge"=character(),
                                     "Project Start"=numeric(),
                                     "Project End"=numeric(),
                                     "Time Remaining"=character(),
                                     "Kicktraq URL"=character())
    
    # put together the 'ending this week' data and dumping it to a file
    if (type %in% c('e','end','both')) {
        page <- startPage
        
        # grab more data as long as we don't have enough!
        while(nrow(endData) == 0 || max(endData$Project.End, na.rm = TRUE) <= begDate + days(endWindow)) {
            currentUrl <- paste0(baseUrl, 'end', pageMod, page)
            endData <- rbind(endData, scrapeKicktraqPage(currentUrl))
            page <- page + 1
            
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(1)
        }
        
        # subset the data, because, ironically, now we'll have too much
        endData <- endData[endData$Project.End <= (begDate + days(endWindow)),]
        
        # now dump it to the file
        createPostBody('end', outputFile, endData)
    }
    
    # put together the 'new this week' data and dumping it to a file
    if (type %in% c('n','new','both')) {
        page <- startPage
        
        # grab more data as long as we don't have enough!
        while(nrow(newData) == 0 || min(newData$Project.Start) >= begDate - days(newWindow)) {
            currentUrl <- paste0(baseUrl, 'new', pageMod, page)
            newData <- rbind(newData, scrapeKicktraqPage(currentUrl))
            page <- page + 1
            
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(1)
        }
        
        # subset the data, because, ironically, now we'll have too much
        newData <- newData[newData$Project.Start >= (begDate - days(newWindow)),]
        
        # now dump it to the file
        createPostBody('new', outputFile, newData, sort = T)
    }
    
    createPostFooter(outputFile)
    
    return(list("end" = endData, "new" = newData))
}