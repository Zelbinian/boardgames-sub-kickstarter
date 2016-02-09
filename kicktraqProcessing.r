# -------- setup procedures ------------------

# checking for required packages, installing if necessary
reqPackages <- c("rvest", "magrittr", "lubridate")
newPackages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

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
        
        ksURLs <- c(ksURLs, projectPage %>% html_node("#button-backthis") %>% html_attr("href"))
        
        projectPageInfo <- projectPage %>%  
            html_node("#project-info-text") %>%   #selects the div with the project details in it
            html_text() %>%                     #pulling the text out
            strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
            unlist(.)
        
        # the pledge is listed in the 9th line
        # if we substring on the location of the colon + 2, that will reliably get the dollar value
        avgPledge <- c(avgPledge, 
                       projectPageInfo[9] %>% 
                           substring(., gregexpr(pattern = ':',.) %>% unlist(.) + 2))
        
        Sys.sleep(1) # try not to hammer their server
    }
    
    return(list("url"=ksURLs,"backers"=backers, "fundingAmt"=fundingAmt, "fundingPcnt"=fundingPcnt,
                "avgPledge"=avgPledge, "startDates"=startDates, "endDates"=endDates,
                "remaining"=remaining))
}

scrapeKicktraq <- function(type = "both") {
    
    # argument validation
    # type
    type <- tolower(gsub(" ", "", type, fixed = TRUE))
    if (!(type %in% c("end","new", "both"))) stop("Type argument must be one of 'end', 'new', or 'both' (case insensitive).");
    
    # we're scraping from paginated data, so we these variables will help traverse that
    url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort="
    currentUrl <- paste0(url,type)
    pageMod <- "&page="
    page <- 1
    
    # data frame the function will return
    output <- data.frame("Title"=character(),"URL"=character(),"Description"=character(),
                         "Backers"=numeric(),"Funding Amount"=character(), "Funding Percent"=character(),
                         "Average Pledge"=character(),"Project Start"=numeric(),
                         "Project End"=numeric(),"Time Remaining"=character())
    
    repeat{
        webdata <- read_html(currentUrl) %>% html_nodes(".project-infobox")
        
        # The project details, annoyingly, are just a text blob
        prj_details <- webdata %>%                      #data source
            html_node(".project-details") %>%   #selects the div with the project details in it
            html_text() %>%                     #pulling the text out
            strsplit('\n')                      #storing each peice of data separately
        
        prj_info <- processProjectInfo(prj_details, webdata %>% html_node("a") %>% html_attr("href"))
        
        output <- rbind(output, 
                        data.frame("Title"=webdata %>% html_node("a") %>% html_text(),
                                   "URL"=prj_info$url,
                                "Description"=webdata %>% html_node("div") %>% html_text(),
                                "Backers"=prj_info$backers,
                                "Funding Amount"=prj_info$fundingAmt,
                                "Funding Percent"=prj_info$fundingPcnt,
                                "Average Pledge"=prj_info$avgPledge,
                                "Project Start"=prj_info$startDates,
                                "Project End"=prj_info$endDates,
                                "Time Remaining"=prj_info$remaining))
        
        # we only need 7 days worth of data, so if we've got that we're done
        if (type == "end" && max(output$Project.End) > today() + days(7)) {
            break;
        } else if (type == "new" && min(output$Project.Start) < today() - days(7)) {
            break;
        } else {
            # assemble new url for scraping
            page <- page + 1
            currentUrl <- paste0(url, type, pageMod, page)
            # throw in some wait time so we don't bludgeon their server
            Sys.sleep(1)
        }
    }
    
    if (type == "end") {
        return(output[output$Project.End <= (today() + days(7)),])
    } else {
        return(output[output$Project.Start >= (today() - days(7)),])
    }
}

# -------- processing boardgame kickstarter projects --------------
#kicktraqEnding <- scrapeKicktraq("end")
#kicktraqNew <- scrapeKicktraq("new")
cat("**What this is**: This is a curated listing of Kickstarter tabletop games projects",
"that are either: a) newly posted in the  past 7ish days or b) ending in the next 7ish days",
"and have at least a fighting chance of being funded. By and large they will be board game",
"projects, but the occasional surprise may also sneak in. Expect new lists each Sunday",
"sometime between 12:00am and 12:00pm PST.\n*****\n", file = "kspost.md", append = TRUE)
cat("## Ending This Week\n", file = "kspost.md", append = TRUE)
cat("Project Info|Status|Backers|Avg Pledge|Ending|Comments\n:--|:--|:--|:--|:--|:--\n", file = "kspost.md", append = TRUE)
for(i in 1:nrow(kicktraqEnding)) {
    with(kicktraqEnding[i,],
         # to make it easy to read, each line below is a column in the table
         cat("**[",as.character(Title),"](",as.character(URL),")** ",as.character(Description)," *(Has currently earned ",as.character(Funding.Amount),")*","|",
         as.character(Funding.Percent),"|",
         as.character(Backers),"|",
         as.character(Average.Pledge),"|",
         as.character(strftime(Project.End, format = "%m-%d")),"|",
         "  \n",sep = "",
         file = "kspost.md", append = TRUE)
    )
}
cat("## New Last Week\n", file = "kspost.md", append = TRUE)
cat("Project Info|Status|Backers|Avg Pledge|Ending|Comments\n:--|:--|:--|:--|:--|:--\n", file = "kspost.md", append = TRUE)
kicktraqNew <- kicktraqNew[with(kicktraqNew, order(Title)),]
for(i in 1:nrow(kicktraqNew)) {
    with(kicktraqNew[i,],
         # to make it easy to read, each line below is a column in the table
         cat("**[",as.character(Title),"](",as.character(URL),")** ",as.character(Description)," *(Has currently earned ",as.character(Funding.Amount),")*","|",
             as.character(Funding.Percent),"|",
             as.character(Backers),"|",
             as.character(Average.Pledge),"|",
             as.character(strftime(Project.End, format = "%m-%d")),"|",
             "  \n",sep = "",
             file = "kspost.md", append = TRUE)
    )
}
cat("*****\n", file = "kspost.md", append = TRUE)
cat("Looking for more comprehensive Kickstarter gaming information? ",
    "Check out [the meta listings on BGG](https://boardgamegeek.com/geeklist/166152/kickstarter-project-metalist),",
    "explore [Kicktraq's data-driven views](https://www.kicktraq.com/categories/games/tabletop%20games/),", 
    "or, of course, [Kickstater's Tabletop Category](https://www.kickstarter.com/discover/categories/games/tabletop%20games?ref=category).",
    file = "kspost.md", append = TRUE)