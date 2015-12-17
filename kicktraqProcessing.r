require(rvest)
require(magrittr)
require(lubridate)

# -------- functions -------------------------
processProjectInfo <- function(projects) {
    
    backers <- vector()
    funding <- vector()
    startDates <- as.POSIXct(vector()) # yeah, I know
    endDates <- as.POSIXct(vector())   # yeah, I know
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
        # using lubridate to make the date stuff less onerous
        # with_tz is some hacky bullshit to make c() not confuse timezones
        startDates <- with_tz(c(startDates, parse_date_time(dates[1], "Bd")), "UTC")
        endDates <- with_tz(c(endDates, mdy(dates[2])), "UTC")
        remaining <- c(remaining, splitData[[5]][2])
    }
    
    return(list("backers"=backers, "funding"=funding, "startDates"=startDates, 
                "endDates"=endDates, "remaining"=remaining))
}

# -------- setup variables -------------------------

# these are the base urls for the two data frames we need to make
ktrq_ending_base_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=end"
ktrq_new_base_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=new"

# these are the urls we'll be using for scraping and will be updated each time
ktrq_ending_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=end"
ktrq_new_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=new"

# we're scraping from paginated data, so we these variables will help traverse that
ktrq_page_mod <- "&page="
cur_page <- 1

# again, due to the pagination, we have to build the data frame a bit at a time
# starting with a blank one allows for adding data the same time every time
ending_data <- data.frame("Title"=character(),"Description"=character(),
                          "Backers"=numeric(),"Funding Status"=character(),
                          "Project Start"=numeric(),"Project End"=numeric(),
                          "Time Remaining"=character())

# -------- processing ending projects --------------
repeat{
    ktrq_ending_data <- read_html(ktrq_ending_url) %>% html_nodes(".project-infobox")
    
    # The project details, annoyingly, are just a text blob
    prj_details <- ktrq_ending_data %>%                      #data source
                    html_node(".project-details") %>%   #selects the div with the project details in it
                    html_text() %>%                     #pulling the text out
                    strsplit('\n')                      #storing each peice of data separately
    
    prj_info <- processProjectInfo(prj_details)
    
    ending_data <- rbind(ending_data, 
                         data.frame("Title"=ktrq_ending_data %>% html_node("a") %>% html_text(),
                                    "Description"=ktrq_ending_data %>% html_node("div") %>% html_text(),
                                    "Backers"=prj_info$backers,
                                    "Funding Status"=prj_info$funding,
                                    "Project Start"=prj_info$startDates,
                                    "Project End"=prj_info$endDates,
                                    "Time Remaining"=prj_info$remaining))
    
    # we only need 7 days worth of data, so if we've got that we're done
    #if (max(ending_data$Project.End) > today() + days(7)) {
        break;
   # } else {
        # assemble new url for scraping
   #     cur_page <- cur_page + 1
    #    ktrq_ending_url <- paste0(ktrq_ending_base_url, ktrq_page_mod, cur_page)
    #    # throw in some wait time so we don't bludgeon their server
    #    Sys.sleep(5)
    #}
}