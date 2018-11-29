# -------- setup procedures ------------------

# checking for required packages, installing if necessary
reqPackages <- c("httr", "rvest", "magrittr", "lubridate", "stringr", "tibble", "dplyr", "jsonlite")
newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

library(httr)
library(magrittr)
library(lubridate)
library(R.utils)
library(jsonlite)
library(tibble)
library(stringr)
library(dplyr)

# sleeptime__ <- 5

# -------- functions -------------------------
# parseStartDate <- function(asIsDate) {
#     startDate <- as.Date(parse_date_time(asIsDate, "bd"))
#     curDate <- Sys.Date()
#     
#     if (month(startDate)==12 && month(curDate)==1) {
#         year(startDate) <- year(curDate) - 1
#     } else {
#         year(startDate) <- year(curDate)
#     }
#     
#     return(startDate)
# }
# 
# parseEndDate <- function(asIsDate) {
#     endDate <- str_match(asIsDate, "^[\\sa-zA-Z0-9]+")[1,] %>% parse_date_time("bd") %>% as.Date()
#     curDate <- Sys.Date()
#     
#     if (month(endDate)==1 && month(curDate)==12) {
#         year(endDate) <- year(curDate) + 1
#     } else {
#         year(endDate) <- year(curDate)
#     }
#     
#     return(endDate)
# }

# This function makes use of grep to find data in the text blob

# extractProjectInfo <- function(textblob, toExtract) {
#     extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
#         strsplit(":") %>% unlist() %>% .[2] %>% trimws()
#     
#     if (length(extracted) == 0) return(NA)
#     else return(extracted)
# }
# 
# scrapeProjectInfo <- function(ktURLs) {
#   
#   backers <- integer(0)
#   fundingPct <- character(0)
#   fundingAmt <- character(0)
#   avgPledge <- character(0)
#   startDates <- ymd()
#   endDates <- ymd()
#   ksURLs <- character(0)
#   
#   for(url in ktURLs) {
#     
#     ktResp <- RETRY(verb = "GET",
#                     url = paste0("https://www.kicktraq.com",url),
#                     body = FALSE,
#                     times = 5) 
#     
#     # if we got a good response, keep going, otherwise harmlessly return the empty lists
#     
#     if (ktResp$status_code == 200) {
#       
#       projectPage <- content(ktResp)
#       
#       # first, grab the url for the actual Kickstarter project
#       thisKsUrl <- projectPage %>% html_node("#button-backthis") %>% html_attr("href")
#       
#       # On occasion, the project page does disappear between grabbing the reference to
#       # it on the project listing and trying to access it directly. It's bizarre.
#       # When this happens, Kicktraq does not return a 404. Instead they generate
#       # some dynamic placeholder page. These placeholder pages have none of the 
#       # elements we're looking for, so the way we figure out if this happens is if
#       # one of the attempts to grab them yeilds an empty list.
#       if (length(thisKsUrl) > 0) {
#         # yay! page exists! 
#         logMessage("The page exists.")
#         
#         projectPageInfo <- projectPage %>%  
#           html_node("#project-info-text") %>%   #selects the div with the project details in it
#           html_text() %>%                     #pulling the text out
#           strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
#           unlist() %>%
#           trimws()                            # Trimming white space to make life easier later
#         
#         # adding new data to the vectors
#         backers <- c(backers, extractProjectInfo(projectPageInfo, "Backers:") %>% as.integer())
#         fundingPct <- c(fundingPct, 
#                         projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title"))
#         fundingAmt <- c(fundingAmt, extractProjectInfo(projectPageInfo, "Funding:"))
#         avgPledge <- c(avgPledge, extractProjectInfo(projectPageInfo, "Average Pledge Per Backer:"))
#         
#         # the dates are in a unique format so the processing here is a bit special and 
#         # we need a helper variable and some helper functions
#         datesStrs <- extractProjectInfo(projectPageInfo, "Dates:") %>% strsplit(" -> ") %>% unlist()
#         startDates <- c(startDates, parseStartDate(datesStrs[1]))
#         endDates <- c(endDates, parseEndDate(datesStrs[2]))
#         ksURLs <- c(ksURLs, thisKsUrl)
#         
#         logMessage(paste("There are now",length(ksURLs),"items processed."))
#       } 
#     } else {
#       message_for_status(ktResp, paste("retrieve",url,"from Kicktraq, processing skipped."))
#     }
#     
#     Sys.sleep(sleeptime__) # try not to hammer their server
#   }
#   
#   return(list("url"=ksURLs,"backers"=backers, "fundingAmt"=fundingAmt, "fundingPcnt"=fundingPct,
#               "avgPledge"=avgPledge, "startDates"=startDates, "endDates"=endDates))
# }
# 
# fetchProjectsData <- function(url, data) {
#   ktResp <- RETRY(verb = "GET",
#                    url = url,
#                    body = FALSE,
#                    times = 5)
#   
#   stop_for_status(x = ktResp,                       # If we don't get a 200, stop execution
#                   task = paste("read",url))
#   
#   webdata <- content(ktResp)
#   logMessage(paste(url,"has been read."))
#   
#   # # The project details, annoyingly, are just a text blob, so need to parse them out
#   # prj_details <- webdata %>%                      #data source
#   #     html_nodes(".project-details") %>%   #selects the div with the project details in it
#   #     html_text() %>%                     #pulling the text out
#   #     strsplit('\n')                      #storing each peice of data separately
#   
#   # this is the meaty function, the thing that actually processes the scraped data
#   ktURLs <- webdata %>% html_nodes("h2 a") %>% html_attr("href")
#   prj_info <- scrapeProjectInfo(ktURLs)
#   
#   add_row(data,
#           "Title"=webdata %>% html_nodes("h2 a") %>% html_text(),
#           "URL"=prj_info$url,
#           "Description"=webdata %>% html_nodes(".project-infobox > div:nth-child(2)") %>% html_text() %>%
#               gsub("[\r\n]", "", .),
#           "Backers"=prj_info$backers,
#           "Funding Amount"=prj_info$fundingAmt,
#           "Funding Percent"=prj_info$fundingPcnt,
#           "Average Pledge"=prj_info$avgPledge,
#           "Project Start"=prj_info$startDates,
#           "Project End"=prj_info$endDates,
#           "Kicktraq URL"=ktURLs) %>% 
#       return()
# }

queryAirtable <- function(viewChoice = "", apiKey) {
  
  # building a blank Tibble to add rows to later
  atData <- tibble("ID"=character(),  
                   "Name"=character(),
                   "Description"=character(),
                   "Metadata"=character(),
                   "Campaign Link"=character(),
                   "BGG Link"=character(),
                   "Launch Date"=ymd(),
                   "End Date"=ymd(),
                   "Min Players"=integer(),
                   "Max Players"=integer(),
                   "Funded"=logical(),
                   "Min Pledge"=numeric(),
                   "Avg Pledge"=character(),
                   "Backers"=numeric(),
                   "Current Funding"=character(),
                   "Funding Percent"=numeric(),
                   "Total Funding"=numeric(),
                   "Cancelled"=logical())
  
  curOffset <- NULL
  
  while(nrow(atData) == 0 || !is.null(curOffset)) {
    
    atResp <- RETRY(verb = "GET", 
                    url = "https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List",
                    query = list(view = viewChoice, api_key = apiKey, offset = curOffset), 
                    body = FALSE,
                    times = 5)
    
    stop_for_status(atResp, paste("retrieve AirTable data from", atResp$request$url))
    
    atJSON <- content(atResp, "text") %>% fromJSON()
    
    # AirTable is a little quirky with logical fields; the value will be TRUE if it's true but NA if FALSE.
    # This gets extra cumbersome to deal with when you aren't sure if the column will be returned at all.
    # Nested ifelse calls are hard to read and are error prone, so performing these ops here.
    
    if(is.null(atJSON$records$fields$Funded)) {
      atJSON$records$fields$Funded <- NA
    }
    
    if(is.null(atJSON$records$fields$Cancelled)) {
      atJSON$records$fields$Cancelled <- NA
    }
    
    if(is.null(atJSON$records$fields$`Total Funding`)) {
      atJSON$records$fields$`Total Funding` <- NA
    }
    
    if(is.null(atJSON$records$fields$Metadata)) {
      atJSON$records$fields$Metadata <- NA
    }
    
    atData %<>% add_row("ID"=atJSON$records$id, 
                        "Name"=atJSON$records$fields$Name,
                        "Description"=atJSON$records$fields$Description,
                        "Metadata"=atJSON$records$fields$Metadata,
                        "Campaign Link"=atJSON$records$fields$`Campaign Link`,
                        "BGG Link"=atJSON$records$fields$`BGG Link`,
                        "Launch Date"=atJSON$records$fields$`Launch Date` %>% ymd(),
                        "End Date"=atJSON$records$fields$`End Date` %>% ymd(),
                        "Min Players"=atJSON$records$fields$`Min Players`,
                        "Max Players"=atJSON$records$fields$`Max Players`,
                        "Funded"=ifelse(is.na(atJSON$records$fields$Funded), FALSE, TRUE),
                        "Min Pledge"=atJSON$records$fields$`Min Pledge (USD)`,
                        "Avg Pledge"=atJSON$records$fields$`Avg Pledge`,
                        "Backers"=atJSON$records$fields$Backers,
                        "Current Funding"=atJSON$records$fields$`Current Funding`,
                        "Funding Percent"=atJSON$records$fields$`Funding Percent`,
                        "Total Funding"=atJSON$records$fields$`Total Funding`,
                        "Cancelled"=ifelse(is.na(atJSON$records$fields$Cancelled), FALSE, TRUE))
    
    curOffset <- atJSON$offset
    
    Sys.sleep(.25)
  }
  
  return(atData)
}

# produces "hashtags" from a vector of strings, dolled up in markdown code formatting 
hashtagify <- function(x) {
  
  x %<>% tolower() %>% gsub(" ", "", .) %>% paste0("`#",.,"`") %>% paste(., collapse = " ")
  
  return(x)
}

# we want to include a project if it has enough funding to possibly succeed, which depends
# on how much time it has left to secure backers
fundingPossible <- function(fundingAmount, endDate, startDate = today(), thMax = 70, perDayPenalty = 3.58) {

  daysUntilEnd <- (startDate %--% endDate) / days()
  threshold <- thMax - (daysUntilEnd * perDayPenalty)
  
  return (fundingAmount >= threshold) 
}

writePostTable <- function(data, kicktraq = F) {
    
    # posts a formatted version of the passed in data to the output file 
    cat("Project Info|Players|Backers|Min / Avg Pledge|Ends|Comments\n:--|:--|:--|:--|:--|:--\n")
  
    checkmark <- intToUtf8("9745")
  
    for(i in 1:nrow(data)) {
      curRecord <- data[i,]
        
      # Project info is the most complicated column as it's calculated from many columns from the source data
      projectInfo <- paste0("**[",curRecord$Name,"](",curRecord$`Campaign Link`,")** ",
                            curRecord$Description,
                            " // *Has raised ",curRecord$`Current Funding`, " so far.",
                            ifelse(curRecord$Funded == TRUE, 
                                   paste0("* **", checkmark, "**"), # if funded, add a neat little checkmark
                                   paste0(" (~", curRecord$`Funding Percent`, "%)*"))) # if not display percentage
      
      # comments are too complicated to attempt in-place in a cat statement, this will stitch together a comment string
      # if certain conditions are met
      comments <- ""
      
      if (kicktraq) {
        comments <- c(comments, 
                      paste0("[kicktraq](",curRecord$`Campaign Link` %>% sub("starter", "traq", .), ")"))
      }
      
      if(!is.na(curRecord$`BGG Link`)) {
        comments <- c(comments, 
                      paste0("[bgg](",curRecord$`BGG Link`, ")"))
      }
      
      if(!is.na(curRecord$Metadata)) {
        # unlist
        metadata <- curRecord$Metadata %>% unlist() 
        # check if null
        if (!is.null(metadata)) {
          
        # for each element in the vector, hashtagify
        comments <- c(comments, hashtagify(metadata))
        }
      }
        
      # to make it easy to read, each line below is a column in the table
      cat(projectInfo,                                                            # Project Info                          
          paste(ifelse(is.na(curRecord$`Min Players`), "?", curRecord$`Min Players`), 
                ifelse(is.na(curRecord$`Max Players`), "?", curRecord$`Max Players`), 
                sep = " - "),                                                     # Players
          curRecord$Backers,                                                      # Backers
          paste0("$", curRecord$`Min Pledge`, " / ",curRecord$`Avg Pledge`),      # Pledges
          strftime(curRecord$`End Date`, format = "%b %d"),                       # Ends
          paste(comments, collapse = ' '),                                        # Comments
          sep="|")
        
        cat("\n", sep="")
    }
    
}

# logMessage <- function(message, logfile="kspostlog.txt") {
#   paste(date(),">",message) %>% cat(file = logfile, sep = "\n", append = TRUE)
# }

# this function is effectively the script

createKsPost <- function(data, begDate = today(), outputFile = "kspost.txt") {
  
  # baseUrl <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort="
  # startPage <- 1
  endInterval <- interval(begDate + days(1), begDate + weeks(1)) # the "ending soon" list consists of projects that will end within 7 days after the posting date of the list
  newInterval <- interval(begDate - weeks(1), begDate - days(1)) # the "new" list consists of projects with startdates within the 7 days prior to the posting date of the list
  # pageMod <- "&page="
    
  # open the file for writing and create the header for the post
  sink(outputFile)
  cat("## What this is:\n\n",
      "This is a weekly, curated listing of Kickstarter board game projects that are either:\n\n",
      "- **newly posted in the past 7 days**, or\n",
      "- **ending in the next 7 days (starting ", strftime(begDate + days(1), format = "%b %d"), ")**",
      " and have at least a fighting chance of being funded.\n\n",
      "All board game projects meeting those criteria will automatically be included, **no need to ask.** (The occasional non-board game project may also sneak in!)\n\n",
      "Expect new lists each Sunday sometime between midnight and noon PST.\n*****\n",
      "## Ending Soon\n", sep="")
    
  # because we want to iteratively build a data frame, it's helpful to start with an
  # empty shell version of it such that we can write one test that is guaranteed to
  # fail the first time
  # endData <- newData <- tibble("Title"=character(0),
  #                                  "URL"=character(0),
  #                                  "Description"=character(0),
  #                                  "Backers"=numeric(0),
  #                                  "Funding Amount"=character(0),
  #                                  "Funding Percent"=character(0),
  #                                  "Average Pledge"=character(0),
  #                                  "Project Start"=ymd(0),
  #                                  "Project End"=ymd(0),
  #                                  "Kicktraq URL"=character(0))
  
  # put together the 'ending this week' data and dumping it to a file
  
  # page <- startPage
  # 
  # logMessage("Now processing projects ending soon")
  # 
  # # grab more data as long as we don't have enough!
  # while(nrow(endData) == 0 || max(endData$`Project End`, na.rm = TRUE) <= begDate + days(endWindow)) {
  #   logMessage(paste("Page", page, "of ending soon projects. Max date:",max(endData$`Project End`, na.rm = TRUE)))
  #   currentUrl <- paste0(baseUrl, 'end', pageMod, page)
  #   endData <- fetchProjectsData(currentUrl, endData)
  #   page <- page + 1
  #   
  #   # throw in some wait time so we don't bludgeon their server
  #   Sys.sleep(sleeptime__)
  # }
  
  # write the projects that end within the endInterval out to the file in Markdown formatm in chronological order
  writePostTable(data = data %>% filter(`End Date` %within% endInterval, `Funding Percent` >= 35) %>% arrange(`End Date`), 
                 kicktraq = T)
  
  # grab more data as long as we don't have enough!
  # while(nrow(newData) == 0 || min(newData$`Project Start`, na.rm = TRUE) >= begDate - days(newWindow)) {
  #   logMessage(paste("Page", page, "of new projects. Min date:",min(newData$`Project Start`, na.rm = TRUE)))
  #   currentUrl <- paste0(baseUrl, 'new', pageMod, page)
  #   newData <- fetchProjectsData(currentUrl, newData)
  #   page <- page + 1
  #   
  #   # throw in some wait time so we don't bludgeon their server
  #   Sys.sleep(sleeptime__)
  # }

  cat("## New This Week\n", sep="")
    
  # write the projects that launched within the newInterval out to the file in Markdown format, in alphabetical order
  writePostTable(data = data %>% filter(`Launch Date` %within% newInterval) %>% arrange(Name),
                 kicktraq = F) 
  
  # write the post footer and then close the file stream
  cat("## Need moar Kickstarter goodness?\n",
      "Check out... \n\n",
      "- My [Calendar of Announced Kickstarters](https://airtable.com/shrioIkpOb33jjrcw)", 
      " ([Also available in iCal flavor](https://airtable.com/shrioIkpOb33jjrcw/iCal?timeZone=America%2FLos_Angeles&userLocale=en))\n",
      "- My interactive AirTable view of [all active board game kickstarter campaigns](https://airtable.com/shrL0m86v0xDjRKbe).\n",
      "- BoardGameGeek's variety of [Kickstarter-oriented Geeklists](https://boardgamegeek.com/geeklist/166152/kickstarter-project-metalist)\n",
      "- [Kicktraq's data-driven views](https://www.kicktraq.com/categories/games/tabletop%20games/)\n\n",
      "## Footnotes\n", 
      "- `#hmm` means that something about the project seems a little off. Buyer beware kinda thing.\n", 
      "- `#lolwut` is reserved for projects that seem like copycat games or campaigns put together with little thought. Check 'em out for amusement.\n", 
      "- `#take` tags are for projects that have been restarted for some reason, with the number indicating what iteration we're currently on.\n",
      "- Did I miss something? Particularly something **new in the last 7 days** or **ending in the next 7 days**?", 
      " Let me know in the comments and I'll add it in.\n\n", 
      "## Help Keep This Running\n",
      "These lists take time and money to put together. Not a lot, but a little.", 
      " If you enjoy them, maybe [toss me a buck](https://www.paypal.me/Zelbinian/1) now and then. 50% of after-expenses costs will be",
      " forwarded along to the [Jack Vasel Memorial Fund](http://www.jackvasel.org/).\n\n",
      "[Signing up for a free AirTable account](https://airtable.com/invite/r/wJL1rj8U) via my referral link",
      " can help, too. Plus, it's swell!", 
      sep="")
  sink()
  
  return(data)
}

# gather the data
atData <- queryAirtable("Data Entry", "") %>% createKsPost()