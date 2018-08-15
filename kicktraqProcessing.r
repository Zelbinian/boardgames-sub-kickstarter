# -------- setup procedures ------------------

# checking for required packages, installing if necessary
reqPackages <- c("rvest", "magrittr", "lubridate", "stringr", "tibble", "dplyr")
newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

library(rvest)
library(magrittr)
library(lubridate)
library(R.utils)
library(stringr)
library(tibble)
library(dplyr)

sleeptime__ <- 5

# -------- functions -------------------------
parseStartDate <- function(asIsDate) {
    startDate <- as.Date(parse_date_time(asIsDate, "bd"))
    curDate <- Sys.Date()
    
    if (month(startDate)==12 && month(curDate)==1) {
        year(startDate) <- year(curDate) - 1
    } else {
        year(startDate) <- year(curDate)
    }
    
    return(startDate)
}

parseEndDate <- function(asIsDate) {
    endDate <- str_match(asIsDate, "^[\\sa-zA-Z0-9]+")[1,] %>% parse_date_time("bd") %>% as.Date()
    curDate <- Sys.Date()
    
    if (month(endDate)==1 && month(curDate)==12) {
        year(endDate) <- year(curDate) + 1
    } else {
        year(endDate) <- year(curDate)
    }
    
    return(endDate)
}

# This function makes use of grep to find data in the text blob

extractProjectInfo <- function(textblob, toExtract) {
    extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
        strsplit(":") %>% unlist() %>% .[2] %>% trimws()
    
    if (length(extracted) == 0) return(NA)
    else return(extracted)
}

scrapeProjectInfo <- function(ktURLs) {
    
    backers <- integer(0)
    fundingPct <- character(0)
    fundingAmt <- character(0)
    avgPledge <- character(0)
    startDates <- ymd()
    endDates <- ymd()
    ksURLs <- character(0)
    
    for(url in ktURLs) {
      
      repeat{
        projectPage <- withTimeout(
          read_html(paste0("http://www.kicktraq.com",url)), timeout = sleeptime__ * 10
        )
        
        logMessage(paste("Currently processing", url))
        
        if(!is.null(projectPage)) break;
      }
        
        # first, grab the url for the actual Kickstarter project
        thisKsUrl <- projectPage %>% html_node("#button-backthis") %>% html_attr("href")
        
        # On occasion, the project page does disappear between grabbing the reference to
        # it on the project listing and trying to access it directly. It's bizarre.
        # When this happens, Kicktraq does not return a 404. Instead they generate
        # some dynamic placeholder page. These placeholder pages have none of the 
        # elements we're looking for, so the way we figure out if this happens is if
        # one of the attempts to grab them yeilds an empty list.
        if (length(thisKsUrl) > 0) {
            # yay! page exists! 
            logMessage("The page exists.")
            
            projectPageInfo <- projectPage %>%  
                html_node("#project-info-text") %>%   #selects the div with the project details in it
                html_text() %>%                     #pulling the text out
                strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
                unlist() %>%
                trimws()                            # Trimming white space to make life easier later
            
            # adding new data to the vectors
            backers <- c(backers, extractProjectInfo(projectPageInfo, "Backers:") %>% as.integer())
            fundingPct <- c(fundingPct, 
                            projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title"))
            fundingAmt <- c(fundingAmt, extractProjectInfo(projectPageInfo, "Funding:"))
            avgPledge <- c(avgPledge, extractProjectInfo(projectPageInfo, "Average Pledge Per Backer:"))
            
            # the dates are in a unique format so the processing here is a bit special and 
            # we need a helper variable and some helper functions
            datesStrs <- extractProjectInfo(projectPageInfo, "Dates:") %>% strsplit(" -> ") %>% unlist()
            startDates <- c(startDates, parseStartDate(datesStrs[1]))
            endDates <- c(endDates, parseEndDate(datesStrs[2]))
            ksURLs <- c(ksURLs, thisKsUrl)
            
            logMessage(paste("There are now",length(ksURLs),"items processed."))
        } 
        
        Sys.sleep(sleeptime__) # try not to hammer their server
    }
    
    return(list("url"=ksURLs,"backers"=backers, "fundingAmt"=fundingAmt, "fundingPcnt"=fundingPct,
                "avgPledge"=avgPledge, "startDates"=startDates, "endDates"=endDates))
}

fetchProjectsData <- function(url, data) {
  webdata <- read_html(url)
  logMessage(paste(url,"has been read."))
  
  # # The project details, annoyingly, are just a text blob, so need to parse them out
  # prj_details <- webdata %>%                      #data source
  #     html_nodes(".project-details") %>%   #selects the div with the project details in it
  #     html_text() %>%                     #pulling the text out
  #     strsplit('\n')                      #storing each peice of data separately
  
  # this is the meaty function, the thing that actually processes the scraped data
  ktURLs <- webdata %>% html_nodes("h2 a") %>% html_attr("href")
  prj_info <- scrapeProjectInfo(ktURLs)
  
  add_row(data,
          "Title"=webdata %>% html_nodes("h2 a") %>% html_text(),
          "URL"=prj_info$url,
          "Description"=webdata %>% html_nodes(".project-infobox > div:nth-child(2)") %>% html_text() %>%
              gsub("[\r\n]", "", .),
          "Backers"=prj_info$backers,
          "Funding Amount"=prj_info$fundingAmt,
          "Funding Percent"=prj_info$fundingPcnt,
          "Average Pledge"=prj_info$avgPledge,
          "Project Start"=prj_info$startDates,
          "Project End"=prj_info$endDates,
          "Kicktraq URL"=ktURLs) %>% 
      return()
}

writePostTable <- function(data, kicktraq = F) {
    
    # posts a formatted version of the passed in data to the output file 
    cat("Project Info|Status|Backers|Avg Pledge|Ending|Comments\n:--|:--|:--|:--|:--|:--\n")
    for(i in 1:nrow(data)) {
        with(data[i,],
             # to make it easy to read, each line below is a column in the table
            cat("**[",as.character(Title),"](",as.character(URL),")** ",as.character(Description)," *(Has currently earned ",as.character(`Funding Amount`),")*","|",
                as.character(`Funding Percent`),"|",
                as.character(Backers),"|",
                as.character(`Average Pledge`),"|",
                as.character(strftime(`Project End`, format = "%m-%d")),"|", sep="")
        )
        
        if (kicktraq) {
            cat("[kicktraq](",as.character(paste0("http://www.kicktraq.com",data[i,]$`Kicktraq URL`)),")")
        }
        
        cat("\n")
    }
    
}

logMessage <- function(message, logfile="kspostlog.txt") {
  paste(date(),">",message) %>% cat(file = logfile, sep = "\n", append = TRUE)
}

# this function is effectively the script

createKsPost <- function(begDate = today()) {
  
  # settings w/ defaults
  outputFile <-"kspost.txt"
  baseUrl <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort="
  startPage <- 1
  newWindow <- 7 # these are the number of days from today to look for games
  endWindow <- 8 # end window is beyond 7 just to make sure everything is captured
  pageMod <- "&page="
    
  # open the file for writing and create the header for the post
  sink(outputFile)
  cat("## What this is:\n\n",
      "This is a weekly, curated listing of Kickstarter tabletop games projects that are either:\n\n",
      "- **newly posted in the past 7 days**, or\n",
      "- **ending in the next 7 days (starting tomorrow)**",
      "and have at least a fighting chance of being funded.\n\n",
      "All board game projects meeting those criteria will automatically be included, no need to ask. (But the occasional non-board game project may also sneak in!)\n\n",
      "Expect new lists each Sunday sometime between 12:00am and 12:00pm PST.\n*****\n",
      "## Ending Soon\n")
    
  # because we want to iteratively build a data frame, it's helpful to start with an
  # empty shell version of it such that we can write one test that is guaranteed to
  # fail the first time
  endData <- newData <- tibble("Title"=character(0),
                                   "URL"=character(0),
                                   "Description"=character(0),
                                   "Backers"=numeric(0),
                                   "Funding Amount"=character(0),
                                   "Funding Percent"=character(0),
                                   "Average Pledge"=character(0),
                                   "Project Start"=ymd(0),
                                   "Project End"=ymd(0),
                                   "Kicktraq URL"=character(0))
  
  # put together the 'ending this week' data and dumping it to a file
  
  page <- startPage
  
  logMessage("Now processing projects ending soon")
  
  # grab more data as long as we don't have enough!
  while(nrow(endData) == 0 || max(endData$`Project End`, na.rm = TRUE) <= begDate + days(endWindow)) {
    logMessage(paste("Page", page, "of ending soon projects. Max date:",max(endData$`Project End`, na.rm = TRUE)))
    currentUrl <- paste0(baseUrl, 'end', pageMod, page)
    endData <- fetchProjectsData(currentUrl, endData)
    page <- page + 1
    
    # throw in some wait time so we don't bludgeon their server
    Sys.sleep(sleeptime__)
  }
  
  # subset the data, because, ironically, now we'll have too much
  endData <- endData %>% filter(`Project End` <= (begDate + days(endWindow)))
  
  # now dump it to the file
  writePostTable(endData, kicktraq = T)
  
  # put together the 'new this week' data and dumping it to a file
  
  page <- startPage
  logMessage("Processing new projects.")
  
  # grab more data as long as we don't have enough!
  while(nrow(newData) == 0 || min(newData$`Project Start`, na.rm = TRUE) >= begDate - days(newWindow)) {
    logMessage(paste("Page", page, "of new projects. Min date:",min(newData$`Project Start`, na.rm = TRUE)))
    currentUrl <- paste0(baseUrl, 'new', pageMod, page)
    newData <- fetchProjectsData(currentUrl, newData)
    page <- page + 1
    
    # throw in some wait time so we don't bludgeon their server
    Sys.sleep(sleeptime__)
  }
  
  # subset the data, because, ironically, now we'll have too much
  newData <- newData %>% filter(`Project Start` >= (begDate - days(newWindow))) %>% arrange(Title)
  
  cat("\n*****\n",
      "## New This Week\n")
    
  # now dump it to the file
  writePostTable(newData, kicktraq = T) 
  
  # write the post footer and then close the file stream
  cat("*****\n",
      "Looking for more comprehensive Kickstarter gaming information? ",
      "Check out [the meta listings on BGG](https://boardgamegeek.com/geeklist/166152/kickstarter-project-metalist), ",
      "explore [Kicktraq's data-driven views](https://www.kicktraq.com/categories/games/tabletop%20games/), or, ", 
      "of course, [Kickstater's Tabletop Category](https://www.kickstarter.com/discover/categories/games/tabletop%20games?ref=category).\n",
      "*****\n", 
      "## Footnotes\n", 
      "- `#hmm` means that something about the project seems a little off. Buyer beware kinda thing.\n", 
      "- `#lolwut` is reserved for projects that seem like trainwrecks. Check 'em out for amusement.\n", 
      "- `#take` tags are for projects that have been restarted for some reason, with the number indicating what iteration we're currently on.\n", 
      "- Did I miss something? Particularly something **new in the last 7 days** or **ending in the next 7 days**? Let me know in the comments and I'll add it in.\n\n", 
      "****\n", 
      "[Tip Jar](https://www.paypal.me/Zelbinian/1) - Keep me in Kickstarter money.")
  sink()
  
  return(list("end" = endData, "new" = newData))
}