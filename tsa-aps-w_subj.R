library(rvest)
library(tidyverse)

# choose the day of the week
date_choice <- "mardi"
date_choice_TSA <- "Le: 27"

link_TSA <- "https://www.tsa-algerie.com/"
link <- "http://www.aps.dz/algerie"
link2 <- "http://www.aps.dz/economie"

link3 <- "http://www.aps.dz/sante-science-technologie"
link4 <- "http://www.aps.dz/societe"

# to make a comlete link
link_base <- "http://www.aps.dz/"

#
selector_i <- ".catItemTitle"

find_headers <- function(link, selector, date_choice){
  # adding subject
  ifelse(link == "http://www.aps.dz/algerie", subj <- "P", ifelse(
    link == "http://www.aps.dz/economie", subj <- "E", ifelse(
      link == "http://www.aps.dz/societe", subj <- "O", subj <- "S")
  ))
  
  html <- read_html(link)  
  
  # read date line
  date_nodes <- html_nodes(html, ".catItemDateCreated")
  date_line <- html_text(date_nodes, trim = TRUE)
  
  # read text lines
  text1 <- html_nodes(html, selector) %>% html_text(trim = TRUE)
  
  #read APS news links for subj
  rel_urls <- html_nodes(html, selector_i) %>% 
    html_children() %>% 
    html_attr("href") %>%  
    url_absolute(link_base)
  
  #extract time from date line
  time <- substr(date_line, nchar(date_line)-5, nchar(date_line))
  
  #creates data.frame object for APS w/ time, source, subject, info and link
  news <- data.frame(0, "source", "subject", "info", "link")
  names(news) <- c("TM", "SRC", "SBJCT", "INF", "LNK")
  journal <- "A"
  
  text2 <- html_nodes(html, selector)
  text_links <- html_attrs(text2)
  
  #finding links 
  text_rel_urls <- html_attr(text2, "href")
  
  for (i in 1:length(date_nodes)) {
    if (str_starts(date_line[i], date_choice)) {
      time[i] <- parse_number(str_replace(time[i], ":", ""))
      update <- data.frame(parse_number(time[i]), journal, subj, strtrim(text1[i], 120), rel_urls[i])
      news <- rbind(news, setNames(update, names(news)))
    }
  }
  print(news[-1,])
}



find_headers_TSA <- function(link, date_choice){
  subj <- "T"
  link <-"https://www.tsa-algerie.com/"
  html <- read_html(link)
  
  # read date line
  date_nodes <- html_nodes(html, ".ntdga__date")
  date_nodes
  date_line <- html_text(date_nodes, trim = TRUE)
  date_line
  
  # read text lines
  text1 <- html_nodes(html, ".ntdga__title a") %>% 
    html_text(trim = TRUE)
  text1
  
  text2 <- html_nodes(html, ".ntdga__title a")
  text2
  text_links <- html_attrs(text2)
  text_links
  
  #finding publish time
  text_rel_urls <- html_attr(text2, "href")
  text_rel_urls
  
  #creates data.frame object w/ 5 components
  news <- data.frame(0, "source", "subject", "info", "link")
  news
  names(news) <- c("TM", "SRC", "SBJCT", "INF", "LNK")
  journal <- "T"
  
  # i try to create time object
  time <- substr(date_line, nchar(date_line)-5, nchar(date_line))
  time
  for (i in 1:length(date_nodes)) {
    if (str_starts(date_line[i], date_choice)) {
      
      html_time <- read_html(text_rel_urls[i])
      html_time
      time_nodes <- html_nodes(html_time, ".article__meta-time")
      time_nodes
      time_line <- html_text(time_nodes, trim = TRUE)
      time_line
      time2 <- substr(time_line, nchar(time_line)-5, nchar(time_line))
      time2
      time[i] <- parse_number(str_replace(time2, ":", ""))
      time[i]
      update <- data.frame(parse_number(time[i]), journal, subj, strtrim(text1[i], 110), text_rel_urls[i])
      update
      news <- rbind(news, setNames(update, names(news)))
      news
    }
  }
  print(news[-1,])
}


part1 <- find_headers(link, selector_i, date_choice)
part2 <- find_headers(link2, selector_i, date_choice)
part3 <- find_headers(link3, selector_i, date_choice)
part4 <- find_headers(link4, selector_i, date_choice)
part_TSA <- find_headers_TSA(link_TSA, date_choice_TSA)

all_parts <- rbind(part1, part2, part3, part4, part_TSA)

df <- all_parts[order(all_parts$TM, decreasing = TRUE),]
print(df %>% select(TM, SRC, SBJCT, INF), right = FALSE)
