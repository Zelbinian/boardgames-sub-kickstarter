# -------- setup procedures ------------------

# checking for required packages, installing if necessary
reqPackages <- c("rvest", "magrittr", "lubridate")
newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

library(rvest)
library(magrittr)
library(lubridate)
library(R.utils)

sleeptime__ <- 5

# -------- functions -------------------------
parseStartDate <- function(asIsDate) {
    startDate <- as.Date(parse_date_time(asIsDate, "bd"))
    curDate <- Sys.Date()
    
    if (month(startDate)==12 && month(curDate)==1) {
        year(startDate) <- year(curDate) - 1
    }
    
    return(startDate)
}

scrapeProjectInfo <- function(ktURLs) {
    
    backers <- vector()
    fundingPct <- vector()
    fundingAmt <- vector()
    avgPledge <- vector()
    startDates <- as.Date(vector())
    endDates <- as.Date(vector())   # yeah, I know
    ksURLs <- vector()
    
    for(url in ktURLs) {
      
      repeat{
        projectPage <- withTimeout(
          read_html(paste0("http://www.kicktraq.com",url)), timeout = sleeptime__
        )
        
        cat(paste("Currently processing", url))
        
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
            cat(" and it exists \n")
            
            projectPageInfo <- projectPage %>%  
                html_node("#project-info-text") %>%   #selects the div with the project details in it
                html_text() %>%                     #pulling the text out
                strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
                unlist() %>%
                trimws()                            # Trimming white space to make life easier later
            
            # at this point we have two cases: projects with no backers will have 60 
            # lines of projectPageInfo, other projects will have 63. This means some
            # info is omitted, so where things are found will change.
            
            if (length(projectPageInfo) == 63) {
                fundingAmtStr <- projectPageInfo[10] %>% substring(10)
                datesStrs <- projectPageInfo[12] %>% substring(8) %>% strsplit(" -> ") %>% unlist()
                avgPledgeStr <- projectPageInfo[8] %>% substring(28) 
            } else {
                fundingAmtStr <- projectPageInfo[7] %>% substring(10)
                datesStrs <- projectPageInfo[9] %>% substring(8) %>% strsplit(" -> ") %>% unlist()
                avgPledgeStr <- NA
            }
            
            # these items are in the same location regardless
            backerStr <- projectPageInfo[5] %>% substring(10)
            fundingPctStr <- projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title") 
            
            # processing date strings
            
            
            # now that we have all the data, stitching together
            backers <- c(backers, backerStr)
            fundingPct <- c(fundingPct, fundingPctStr)
            fundingAmt <- c(fundingAmt, fundingAmtStr)
            avgPledge <- c(avgPledge, avgPledgeStr)
            startDates <- c(startDates, parseStartDate(datesStrs[1]))
            # this one looks a little complicated so let me explain
            # The end date has some parenthetical stuff attached we have to strip out.
            # strsplit lets us do this really easily, and the first part of the list it
            # returns is the actual date. It must be unlisted to access. Because we're 
            # only using part of what the function returns, magrittr can't be used for that.
            # 
            endDates <- c(endDates,
                          unlist(strsplit(datesStrs[2], "(", fixed = TRUE))[1] %>%
                              parse_date_time("bd") %>% as.Date())
            ksURLs <- c(ksURLs, thisKsUrl)
            print(paste("There are now",length(ksURLs),"items processed."))
        } 
        
        Sys.sleep(sleeptime__) # try not to hammer their server
    }
    
    return(list("url"=ksURLs,"backers"=backers, "fundingAmt"=fundingAmt, "fundingPcnt"=fundingPct,
                "avgPledge"=avgPledge, "startDates"=startDates, "endDates"=endDates))
}

scrapeProjectsList <- function(url) {
    webdata <- read_html(url)
    
    # # The project details, annoyingly, are just a text blob, so need to parse them out
    # prj_details <- webdata %>%                      #data source
    #     html_nodes(".project-details") %>%   #selects the div with the project details in it
    #     html_text() %>%                     #pulling the text out
    #     strsplit('\n')                      #storing each peice of data separately
    print("Page has been read.")
    # this is the meaty function, the thing that actually processes the scraped data
    ktURLs <- webdata %>% html_nodes("h2 a") %>% html_attr("href")
    prj_info <- scrapeProjectInfo(ktURLs)
    
    return(data.frame("Title"=webdata %>% html_nodes("h2 a") %>% html_text(),
               "URL"=prj_info$url,
               "Description"=webdata %>% html_nodes(".project-infobox > div:nth-child(2)") %>% html_text(),
               "Backers"=prj_info$backers,
               "Funding Amount"=prj_info$fundingAmt,
               "Funding Percent"=prj_info$fundingPcnt,
               "Average Pledge"=prj_info$avgPledge,
               "Project Start"=prj_info$startDates,
               "Project End"=prj_info$endDates,
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
    cat("* Did I miss something? Particularly stuff that might go in the Comments column? Let me know and I'll add it in.\n\n", file = outputFile, append = TRUE)
    cat("****\n", file = outputFile, append = TRUE)
    cat("[Tip Jar](https://www.paypal.me/Zelbinian/1) - Keep me in Kickstarter money.", file = outputFile, append = TRUE)
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
    endData <- newData <- data.frame("Title"=character(0),
                                     "URL"=character(0),
                                     "Description"=character(0),
                                     "Backers"=numeric(0),
                                     "Funding Amount"=character(0),
                                     "Funding Percent"=character(0),
                                     "Average Pledge"=character(0),
                                     "Project Start"=numeric(0),
                                     "Project End"=numeric(0),
                                     "Kicktraq URL"=character(0))
    
    # put together the 'ending this week' data and dumping it to a file
    if (type %in% c('e','end','both')) {
        page <- startPage
        
        print("Processing projects ending soon.")
        
        # grab more data as long as we don't have enough!
        while(nrow(endData) == 0 || max(endData$Project.End, na.rm = TRUE) <= begDate + days(endWindow)) {
            print(paste("Page ",page))
            currentUrl <- paste0(baseUrl, 'end', pageMod, page)
            endData <- rbind(endData, scrapeProjectsList(currentUrl))
            page <- page + 1
            
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(sleeptime__)
        }
        
        # subset the data, because, ironically, now we'll have too much
        endData <- endData[endData$Project.End <= (begDate + days(endWindow)),]
        
        # now dump it to the file
        createPostBody('end', outputFile, endData)
    }
    
    # put together the 'new this week' data and dumping it to a file
    if (type %in% c('n','new','both')) {
        page <- startPage
        print("Processing new projects.")
        # grab more data as long as we don't have enough!
        while(nrow(newData) == 0 || min(newData$Project.Start) >= begDate - days(newWindow)) {
            print(paste("Page ",page))
            currentUrl <- paste0(baseUrl, 'new', pageMod, page)
            newData <- rbind(newData, scrapeProjectsList(currentUrl))
            page <- page + 1
            
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(sleeptime__)
        }
        
        # subset the data, because, ironically, now we'll have too much
        newData <- newData[newData$Project.Start >= (begDate - days(newWindow)),]
        
        # now dump it to the file
        createPostBody('new', outputFile, newData, sort = T)
    }
    
    createPostFooter(outputFile)
    
    return(list("end" = endData, "new" = newData))
}