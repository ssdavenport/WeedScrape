# scrape_weedmaps
library(tidyverse)
library(rvest)
library(testthat)


# Direct remote driver to index page --------------------------------------
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
source("code/start_remote_driver.R")
wm_index <- "https://weedmaps.com/dispensaries/in/united-states/california"
remDr$navigate(wm_index) # used to work
remDr$screenshot(display = TRUE)
wm_index_html <- read_html(remDr$getPageSource() %>% unlist)



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
# Write function that extracts store links from a city index page (Weedmaps)
# It collects medical and rec stores by default, or can be set to rec only
get_city_store_links <- function(city_index_url, rec_only = FALSE, screenshot = FALSE, delay = 1) {
  
  # Grab all unique dispensary URLs in sidebar
  remDr$navigate(city_index_url) # Get Page Source
  Sys.sleep(delay)
  remDr$screenshot(display = screenshot)
  
  # If looking for recreational only, then click the rec button.
  # This will mean only rec stores are collected.
  if (rec_only) {
    # Click filter screen
    remDr$findElement("css", "#filters-modal-button.wm-filter-button")$clickElement()
    Sys.sleep(delay)
    # Click on the medical toggle button (this will turn medical off)
    remDr$findElement("css", "div.wm-filter-modal-toggle-control span")$clickElement()
    Sys.sleep(delay)
    # Click on the "okay" ("See n Results") Button to keep changes
    remDr$findElement("css", "ion-footer-bar span")$clickElement()
    }
  
  # Extract store links (sometimes produces 0-length output, if no stores there)
  source <- read_html(remDr$getPageSource() %>% unlist) 
  store_links <- source %>% 
    xml_find_all('//*[@data-ng-switch-default]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique
  
  store_links
}


# Get details for all stores (med/rec), deploying function for each.
all_store_links <- city_links %>%
  map(~get_city_store_links(city_index_url = ., rec_only = FALSE, delay = 0.5)) %>% 
  unlist %>% 
  unique
# Add link prefix and details page suffix (which can be static-ly scraped)
all_store_links <- paste0("https://weedmaps.com", all_store_links, "#/details")
# Save
write.csv(all_store_links, "data/weedmaps_store_links.csv"); beepr::beep(5)


# Repeat the above step, this time for recreational only
# TODO: Run this step some time.
proc.time()
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
get_store_details_wm <- function(store_url) {
  
  html_page <- read_html(store_url)
  
  # Grab fields from the HTML page.
  state <- html_page %>% html_node("body") %>% html_nodes("[itemprop=addressRegion]") %>% html_text()
  name <- html_page %>% html_nodes(".listing-subtitle") %>% html_text() %>% head(1)
  address <- html_page %>% html_nodes("[itemprop=streetAddress]") %>% html_text()
  city <- html_page %>% html_nodes("[itemprop=addressLocality]") %>% html_text()
  ZIP <- html_page %>% html_nodes("[itemprop=postalCode]") %>% html_text()
  phone <- html_page %>% html_nodes("[itemprop=telephone]") %>% html_text() %>% head(1)
  phone2 <- html_page %>% html_nodes("[itemprop=telephone]") %>% html_text() %>% head(2) %>% tail(1)
  email <- html_page %>% html_nodes("[itemprop=email]") %>% html_text() %>% head(1)
  email2 <- html_page %>% html_nodes("[itemprop=email]") %>% html_text() %>% head(2) %>% tail(1)
  membersince <- html_page %>% html_nodes("div .details-card-items") %>% html_text() %>%
    str_subset("Member Since") %>% str_replace_all("Member Since", "") %>%
    str_replace_all("(rd)?(nd)?(st)?(th)?,?", "") %>% as.Date("%b %d %Y")
  # convert membersince to a date.
  hits <- html_page %>% html_nodes("div .listing-hits-number") %>% html_text() %>%
    str_replace_all(",", "") %>% as.numeric
  twitter <- html_page %>% html_nodes("div .social-links #twitter") %>% html_text()
  instagram <- html_page %>% html_nodes("div .social-links #instagram") %>% html_text()
  facebook <- html_page %>% html_nodes("div .social-links #facebook") %>% html_text()
  website <- html_page %>% html_nodes("div .social-links") %>% html_text() %>%
    str_extract_all("www.*") %>% unlist
  reviews <- html_page %>% html_nodes("div .listing-reviews-number") %>% html_text() %>%
    str_replace_all(",", "") %>% as.numeric
  description <- html_page %>% html_nodes("div .details-body") %>% html_text() %>% str_c(collapse = "<break>")
  hours <- html_page %>% html_nodes("div .details-card-items") %>% html_text() %>% str_subset("Sunday")
  
  # Format the output.
  features <- c('state', 'name', 'address', 'city', 'ZIP', 'phone', 'phone2', 'email', 'email2', 'membersince',
                'hits', 'twitter', 'instagram', 'facebook', 'website', 'reviews', 'description', 'hours')
  out <- list(state, name, address, city, ZIP, phone, phone2, email, email2, membersince,
              hits, twitter, instagram, facebook, website, reviews, description, hours)
  names(out) = features
  
  # Testing.
  # If any element has length zero, than set that to NA (rather than null)
  out[sapply(out, function(x) length(x)==0)] <- NA
  # If any elements have more than one length, truncate to one and send a message.
  for (i in seq_along(out)) {
    if ( length(out[i]) >1 )  {
      message(paste0("WARNING: field ", names(out)[i], " with values ", out[i], " is being truncated for store ", name))
      out[i] <- head(out[i], 1)
    }
  }
  # Double check that all elements are of length 1.
  expect_true(all(sapply(out, length)==1)) 
  # Prevent this from ever resolving to a 0-length output
  if(length(out)==0) out <- NA
  # Output store details
  out
}
# Deploy on each store page.
all_store_details <- store_links %>%
  map(get_store_details_wm)
# Transform to dataframe, adding URL columns, cleaning
stores_wm <- do.call(rbind, lapply(all_store_details, data.frame)) %>% 
  mutate(url = store_links,
         name = str_replace_all(name, ".* Dispensary -", ""),
         state = str_replace_all(state, ".*(?i)CA.*", "CA")) %>%
  filter(state != "AZ")
# Export/Save
# Note: File still has quasi-duplicates
write.csv(stores_wm, "output/store_details_wm.csv")


# TODO: Repeat this code for the rec only dataset.



# OLD STUFF ---------------------------------------------------------------
# This function gets called by scrape_wm_reviews. It stops a dead website from breaking the whole loop.
tryWebAction <- function(action, url=NA, name='generic') {
  tryCatch(expr = action,
           error = function(e) {
             message(paste0('the web action ', name, ' did not work at URL: ', url))
             do_over <- tryCatch(
               expr = {
                 Sys.sleep(2)
                 action
               },
               error = function(e) {
                 message(paste0('the web action ', name, 'failed on second try at URL: ', url))
               })
             return(do_over)
           })
}


# Perform dynamic scraping (for review dates) -----------------------------
for (i in 4001:4482) {#4482
  # Cycle throough HTMLs, getting details. Pause every once in awhile and print status indicator.
  weedmaps_url_html_list2$reviews[[i]] <- 
    tryCatch( 
      expr = {scrape_wm_reviews(weedmaps_url_html_list2$urls[[i]], wait_time = 1)},
      error = function(e) scrape_wm_reviews(weedmaps_url_html_list2$urls[[i]], wait_time = 5, quiet = F)
    )
  if (i%%5==0) Sys.sleep(5);if (i%%10==0) print(i)  # bonus pause and status indicators.
}