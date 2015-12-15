require(rvest)
require(data.table)

ktrq_ending_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=end"
ktrq_new_url <- "http://www.kicktraq.com/categories/games/tabletop%20games?sort=new"
ktrq_page_mod <- "&page="
cur_page <- 1

ktrq_ending <- read_html(ktrq_ending_url) %>% html_nodes(".project-infobox")
ending_data <- data.table("Title"=ktrq_ending %>% html_node("a") %>% html_text(),
                          "Description"=ktrq_ending %>% html_node("div") %>% html_text(),
                          )