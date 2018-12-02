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
    writeLines("Project Info|Players|Backers|Min / Avg Pledge|Ends|Comments\n:--|:--|:--|:--|:--|:--", outputFile, useBytes = T)
  
    checkmark <- "\u2611"
  
    for(i in 1:nrow(data)) {
      curRecord <- data[i,]
        
      # Project info is the most complicated column as it's calculated from many columns from the source data
      projectInfo <- paste0("**[",curRecord$Name,"](",curRecord$`Campaign Link`,")** ",
                            curRecord$Description,
                            " // *",
                            ifelse(curRecord$Funded == TRUE, 
                                   # if funded, bold the funding info add a neat little checkmark
                                   paste0("*Has raised ", curRecord$`Current Funding`, " so far. ", checkmark, "*"), 
                                   # if not skip bolding and display percentage
                                   paste0("Has raised ", curRecord$`Current Funding`, " so far. ", "(~", curRecord$`Funding Percent`, "%)")),
                            "*") 
      
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
      paste(projectInfo,                                                            # Project Info                          
          paste(ifelse(is.na(curRecord$`Min Players`), "?", curRecord$`Min Players`), 
                ifelse(is.na(curRecord$`Max Players`), "?", curRecord$`Max Players`), 
                sep = " - "),                                                     # Players
          curRecord$Backers,                                                      # Backers
          paste0("$", curRecord$`Min Pledge`, " / ",curRecord$`Avg Pledge`),      # Pledges
          strftime(curRecord$`End Date`, format = "%b %d"),                       # Ends
          paste(comments, collapse = ' '),                                        # Comments
          sep="|") %>% writeLines(outputFile, useBytes = T)
    }
    
}

# this function is effectively the script

createKsPost <- function(data, begDate = today()) {
  
  # baseUrl <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort="
  # startPage <- 1
  endInterval <- interval(begDate + days(1), begDate + weeks(1)) # the "ending soon" list consists of projects that will end within 7 days after the posting date of the list
  newInterval <- interval(begDate - weeks(1), begDate - days(1)) # the "new" list consists of projects with startdates within the 7 days prior to the posting date of the list
  # pageMod <- "&page="

  paste("## What this is:\n\n",
      "This is a weekly, curated listing of Kickstarter board game projects that are either:\n\n",
      "- **newly posted in the past 7 days**, or\n",
      "- **ending in the next 7 days (starting ", strftime(begDate + days(1), format = "%b %d"), ")**",
      " and have at least a fighting chance of being funded.\n\n",
      "All board game projects meeting those criteria will automatically be included, **no need to ask.** (The occasional non-board game project may also sneak in!)\n\n",
      "Expect new lists each Sunday sometime between midnight and noon PST.\n*****\n",
      "## Ending Soon", 
      sep="") %>% writeLines(outputFile, useBytes = T)
  
  # write the projects that end within the endInterval out to the file in Markdown formatm in chronological order
  writePostTable(data = data %>% filter(`End Date` %within% endInterval, fundingPossible(`Funding Percent`, `End Date`)) %>% arrange(`End Date`), 
                 kicktraq = T)

  writeLines("## New This Week", outputFile, useBytes = T)
    
  # write the projects that launched within the newInterval out to the file in Markdown format, in alphabetical order
  writePostTable(data = data %>% filter(`Launch Date` %within% newInterval) %>% arrange(Name),
                 kicktraq = F) 
  
  # write the post footer and then close the file stream
  paste("## Need moar Kickstarter goodness?\n",
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
      " can help, too. Plus, it's swell!\n",
      "If you want a shoutout for your donation, put your reddit username in the notes.",
      sep="") %>% writeLines(outputFile, useBytes = T)
  
  return(data)
}

# open the connection
outputFile <- file("kspost.txt", open = "w+", encoding = "native.enc")

# gather data , write out and close the connection
tryCatch(atData <- queryAirtable("Data Entry", "") %>% createKsPost(),
         finally = close(outputFile))
