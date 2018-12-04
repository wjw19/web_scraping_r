# install.packages("rvest")
library(rvest)
library(dplyr)
library(tidyr)

# scrape info of url
lego_movie <- html("http://www.imdb.com/title/tt1490017/")
url <- html("http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature")

# extract the rating
beasts %>% 
  html_node("strong span") %>% # find the 1st node that matches the selector 
  html_text() %>% # extract its contents
  as.numeric() # convert it

# extract the cast
lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

# 
lego_movie %>%
  html_nodes("table") %>% # find title and authors
  .[[3]] %>%
  html_table()

rank_data <- html_nodes(url, ".text-primary") %>% 
  html_text %>% 
  as.numeric %>% 
  as.data.frame %>% 
  rename(rank = '.')

header_data <- html_nodes(url, ".lister-item-header a") %>% 
  html_text %>% 
  as.character %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  rename(header = '.') %>% 
  mutate(rank = c(1:100))

desc_data <- html_nodes(url,".ratings-bar+ .text-muted") %>% 
  html_text %>% 
  as.character %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  rename(description = '.') %>% 
  # clean description data
  extract(col = description,
          regex = c("(^.*)(\n    )(.*$)"),
          into = c("blanks", "n_blank", "desc")) %>% 
  select(desc) %>% 
  mutate(rank = c(1:100))

rank_data %>% 
  inner_join(header_data, by = "rank") %>% 
  inner_join(desc_data, by = "rank")

html_nodes(url, "span") %>% 
  xml_children %>% 
  html_text %>% 
  as.data.frame


html_nodes(url, 
           ".lister-item .sort-num_votes-visible > span") %>% 
  html_text %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  rename(mix = '.') 






















