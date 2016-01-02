# load data --------------------------------------------------------------------------

responses <- read.csv("redditSurveyResponses.csv",
                      col.names = c("Timestamp", "PostIsPrimarySource","AidedDiscovery",
                                    "CurPrimarySource","PrimarySourceReason",
                                    "SwitchToSub","RPGprojects","DiceProjects",
                                    "ComponentProjects","PlayingCardProjects","AppProjects",
                                    "AccProjects","OtherProjects","HighFundingProb",
                                    "Comment","MonthsOnSub","BGGuse","KickstarterAcct",
                                    "Gender","Birthyear"),
                      stringsAsFactors = FALSE)

# functions --------------------------------------------------------------------------

getScore <- function(list) {
    sums <- sum(list %in% "Never include") * -1.5 +
        sum(list %in% "Sparingly include, if interesting") * -1 +
        sum(list %in% "Usually include, unless dumb") +
        sum(list %in% "Always include") * 1.5
    
    return(sums / length(list))
}

# data clean up ----------------------------------------------------------------------

# measuring the project types
# we'll do this simply; there are 5 possible responses ranging from very neg to very pos.
# we'll map these to the following values: -1.5, -1, 0, 1, 1.5
# then we'll take an average and see where they land on the scale
rpgScore <- length(responses$RPGprojects[responses$RPGprojects == "Never include"])

str(responses)
