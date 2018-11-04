# scrape_weedmaps
library(tidyverse)
library(rvest)
library(testthat)
source('code/functions.R')

# Direct remote driver to index page --------------------------------------
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
source("code/start_remote_driver.R")
wm_index <- "https://weedmaps.com/dispensaries/in/united-states/california"
remDr$navigate(wm_index) # used to work
remDr$screenshot(display = TRUE)
wm_index_html <- read_html_safely(remDr$getPageSource() %>% unlist)


# Compile City Index URLs -----------------------------------
city_links_CSS <- "div.listing-container div.scroll ul li a"
city_links <- wm_index_html %>% 
  html_nodes(city_links_CSS) %>% 
  html_attr("href")
city_links <- paste0("https://weedmaps.com", wm_index_html %>% 
                       html_nodes(city_links_CSS) %>% html_attr("href"))
write.csv(city_links, "data/weedmaps_city_links.csv")


# Scrape Store URLs, iterating through city index URLs --------------------

city_links <- read_csv("data/weedmaps_city_links.csv") %>% 
  select(-X1) %>% unlist # drop empty column
city_index_url <- city_links[7]


# Get details for all stores (med/rec), deploying function for each (gives 1003 links)
all_store_links <- city_links %>%
  map(~get_city_store_links(city_index_url = ., rec_only = FALSE, delay = 0.5)) %>% 
  unlist %>% 
  unique
# Add link prefix and details page suffix (which can be static-ly scraped)
all_store_links <- paste0("https://weedmaps.com", all_store_links, "#/details")
# Save
write.csv(all_store_links, "data/weedmaps_store_links.csv"); beepr::beep(5)


# Repeat the above step, this time for recreational only (gives 148 links)
city_links %>%
  map(~get_city_store_links(city_index_url = ., rec_only = TRUE, delay=1, screenshot=TRUE)) %>% 
  unlist %>% 
  unique %>%
  paste0("https://weedmaps.com", ., "#/details") %>% 
  write.csv("data/weedmaps_store_links_rec.csv")
proc.time()


# Scrape store info from store URLs ---------------------------------------

store_links <- read_csv("data/weedmaps_store_links.csv") %>% 
  select(-X1) %>% unlist

# Write function that extracts key info from store page (Weedmaps)

# Deploy on each store page.
all_store_details <- store_links %>%
  map(get_store_details_wm) # This recently crashed due to a 504 error 

# Transform to dataframe, adding URL columns, cleaning
stores_wm <- do.call(rbind, lapply(all_store_details, data.frame)) %>% 
  mutate(name = str_replace_all(name, ".* Dispensary -", ""),
         state = str_replace_all(state, ".*(?i)CA.*", "CA")) %>%
  filter(state != "AZ")

# Export/Save
# Note: File still has quasi-duplicates
write.csv(stores_wm, "data/store_details_wm.csv")

beepr::beep(5)
