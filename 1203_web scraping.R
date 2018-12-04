# install.packages("rvest")
library(rvest)
library(dplyr)
library(tidyr)


# ----- basic web scraping -----

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


# ----- scraping multi levels of websites -----

# top 250 rated TV shows
main_url <- "http://www.imdb.com/chart/toptv/?ref_=nv_tp_tv250_2"

# define a function that extracts the cast from TV show page:
getcast <- function(url){
  page <- read_html(url)
  nodes <- html_nodes(page, "td a")
  # nodes <- html_nodes(page, "#titleCast .itemprop")
  cast <- html_text(nodes)
  cast <- cast[2]
  # inds <- seq(from=2, to=length(cast))
  # cast <- cast[inds]
  return(cast)
}

# open main url
main_page <- read_html(main_url)
movies_html <- html_nodes(main_page, ".titleColumn a")

# get the titles and url
m_titles <- html_text(movies_html)
sub_urls <- html_attr(movies_html, "href")
m_urls <- paste0('http://www.imdb.com', sub_urls)

# use "getcast()" to extract movie cast from every url in last one
m_cast <- lapply(m_urls, getcast)

# bind all the lists
major_cast <- m_cast %>% 
  as.character %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  rename(main_cast = '.')

m_titles %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  cbind(., major_cast) %>% 
  rename(movie_title = '.') %>% 
  mutate(main_cast = gsub("\n", "", movie_title)) %>% 
  head










