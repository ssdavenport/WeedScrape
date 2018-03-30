# scrape leafly
source("code/functions.R")
library(rvest)
library(tidyverse)
# Start w LA County ZIP Codes

# Set Up ------------------------------------------------------------------
source("code/start_remote_driver.R")
# Direct the remote browser to the proper page.
lf_index <- "https://www.leafly.com/finder/"
remDr$navigate(lf_index) # used to work
remDr$screenshot(T)
# Click "21+ years old, okay" for first time.
# After first time, cookies makes this unnecessary
# Are you eligible to visit? (21+ years old)
remDr$findElement("css", "input#eligibility-tou-confirm")$clickElement() # click old enough
remDr$findElement("css", "button#tou-continue")$clickElement() # click old enough
# Would you sign up for the newsletter?
# remDr$findElement("css", "label.terms-checkbox")$clickElement() # click old enough (broken?)
# remDr$findElement("css", "label.terms-checkbox")$clickElement() # click checkbox?
# remDr$findElement("css", "button.terms-button")$clickElement() # click continue

remDr$getCurrentUrl()

# Find Stores by ZIP ------------------------------------------------------
# Get list of LA County ZIP Codes
# source("code/compile_lacounty_zipcodes.R")
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2]
# Write function to fetch Leafly store URLs by ZIP
get_zip_store_links_leafly <- function(ZIP, delay=2) {
  # Go to index webpage.
  remDr$navigate(lf_index) 
  Sys.sleep(delay) 
  # Enter ZIP into textbox; seerch
  textbox <- remDr$findElement("css", "location-changer form input")
  textbox$sendKeysToElement(list(as.character(ZIP))) # type ZIP
  textbox$sendKeysToElement(list("\uE007")) # hit enter (unicode)
  Sys.sleep(delay)
  # Zoom out! (not implemented, but would find potentially find more stores)
  # zoom_out_button <- remDr$findElement("css", "button[type^='Zoom out']")
  # zoom_out_button$clickElement() # Zoom out once
  # Sys.sleep(.25)
  # zoom_out_button$clickElement() # Zoom out again
  # Sys.sleep(.25)
  
  # Scrape sidebar links as normal.
  zip_stores_as <- remDr$findElements("css", "ul.clearfix a.col-xs-12") # anchors
  zip_stores_urls <- unlist(lapply(zip_stores_as, function(x) x$getElementAttribute("href")))
  Sys.sleep(delay/2)
  zip_stores_urls
}
# Deploy function to get stores for each ZIP
# TODO: run this code.
# WARNING: this seems to be generating different results each time; maybe duplicate?
proc.time()
store_links_leafly <- LAZIPs %>%
  map(~get_zip_store_links_leafly(ZIP=., delay = 5)) %>%
  unlist %>%
  unique

# store_links_leafly
# store_links_leafly
# remDr$screenshot(T)
# get_zip_store_links_leafly(LAZIPs[1])
# undebug(get_zip_store_links_leafly)

# Export / save
write.csv(store_links_leafly, "data/leafly_store_links.csv")
proc.time()

store_links_leafly <- read.csv("data/leafly_store_links.csv") %>%
  select(-X) %>% unlist

# Scrape details per store ------------------------------------------------
# TODO: Run this code
# Write function to scrape details from Leafly store pages
scrape_leafly_page <- function(page) {
  # Parse the webpage, looking for attributes
  tryCatch(
    expr = {
      # med / rec / licensed
      store_type_info <- page %>% html_nodes("ul li") %>% html_text()
      # Name
      name <- page %>% html_nodes("div.site-actions-breadcrumbs") 
      name <- ifelse(is.null(name), NA, name %>% html_text() %>% str_replace("Home Dispensaries ", ""))
      
      # store_type_info <- page %>% html_nodes("ul.m-pills li") %>% html_text()
      is_med <- any(str_detect(store_type_info, "Medical Dispensary"))
      is_rec <- any(str_detect(store_type_info, "Recreational Store"))
      is_licensed <- any(str_detect(store_type_info, "Licensed"))
      # Basic details
      body <- page %>% html_node('body')
      title_text <- page %>% html_node('head') %>% html_node('title') %>% html_text()
      joined_date <- body %>% html_nodes('label > time') %>% magrittr::extract(1) %>% html_text()
      phone <- body %>% html_node('label[itemprop=telephone]') %>% html_text()
      address <- body %>% html_node('label[itemprop=streetAddress]') %>% html_text()
      city <- body %>% html_node('span[itemprop=addressLocality]') %>% html_text()
      state <- body %>% html_node('span[itemprop=addressRegion]') %>% html_text()
      # use XPATH to identify the contents of the H6 that says "WEB".
      web <- body %>% html_nodes(xpath='//h6[text()="Web"]/following::*[1][a]') %>%
        html_node('a') %>% html_attr('href') # Get the address of the link right after the Web header
      about <- body %>% html_nodes(xpath='//h2[text()="About"]/following::*[1]') %>% html_text()
      hours <- body %>% html_nodes(xpath='//h6[text()="Hours"]/following::*[1]') %>% html_text()
      # Reviews
      recent_review_date <- body %>% html_node('header.heading--article > time') %>% html_attr('datetime')
      if(length(recent_review_date)==0)  recent_review_date <- "No Review"
      recent_review_content <- body %>% html_node('p[itemprop=reviewBody]') %>% html_text()
      if(is.na(recent_review_content) | is.null(recent_review_content)  | length(recent_review_content)==0) {
        recent_review_content <- "No Review" 
      }
      # Menu
      # Get menu update, if available.
      if(length(body %>% html_nodes('label > time')) > 1) {
        menu_updated_date <- body %>% html_nodes('label > time') %>% magrittr::extract(2) %>% html_text()
      } else {menu_updated_date <- 'No Menu info'
      }
    },
    error = function(e) message(paste0("this page could not load becuase of error: ", e))
  )
  
  # Save output to a list.
  output <- list()
  if(!is.null(joined_date)) output[['joined_date']] <- joined_date
  if(!is.null(menu_updated_date)) output[['menu_updated_date']] <- menu_updated_date
  if(!is.null(phone)) output[['phone']] <- phone
  if(!is.null(address)) output[['address']] <- address
  if(!is.null(city)) output[['city']] <- city
  if(!is.null(state)) output[['state']] <- state
  if(!is.null(recent_review_date)) output[['recent_review_date']] <- recent_review_date
  if(!is.null(recent_review_content)) output[['recent_review_content']] <- recent_review_content
  if(!is.null(web)) output[['web']] <- web
  if(!is.null(name)) output[['name']] <- name
  if(length(web)==0) output[['web']] <- NA
  if(!is.null(about)) output[['about']] <- about
  if(length(about)==0) output[['about']] <- NA
  if(!is.null(hours)) output[['hours']] <- hours
  if(length(hours)==0) output[['hours']] <- NA
  output[['is_licensed']] <- is_licensed
  output[['is_med']] <- is_med
  output[['is_rec']] <- is_rec
  if(!is.null(title_text)) output[['title_text']] <- title_text
  if(is.null(title_text)) output[['title_text']] <- NA
  output
}

# Write wrapper function to call page scraper safely
apply_scrape_to_leafly_urls <- function(urls, range=1:length(urls)) {
  output <- list()
  output$urls <- urls
  for(i in range) { # Iterate over each URL.
    output$htmls[[i]] <- tryCatch(read_html(urls[i]), 
                                  error=function(e) return(NA))
    output$results[[i]] <- tryCatch(scrape_leafly_page(output$htmls[[i]]),
                                    error=function(e) return(NA))
    output$results[[i]]$url <- urls[i] # add in the URL to the results list.
    if(i %%25 == 0) print(paste0("Copied the ", i, "th link"))
  }
  output
}

ptm <- proc.time()
# Deploy scraper on all store pages
store_details_lf <- vector(mode='list', length(store_links_leafly))
for (store_url in 1:length(store_links_leafly)) {
  webpage <- read_html_safely(as.character(store_links_leafly[store_url]))
  store_details_lf[[store_url]] <- scrape_leafly_page(webpage)
  for (colnames in names(store_details_lf[[store_url]])) {
    store_details_lf[[store_url]][[colnames]] <- ifelse(
      length(store_details_lf[[store_url]][[colnames]]) != 0, 
      store_details_lf[[store_url]][[colnames]], NA)
  }
}
proc.time() - ptm


# Reshape to dataframe ----------------------------------------------------

stores_lf <- do.call(rbind, lapply(store_details_lf, data.frame))
container <- list()
for (store_url in seq_along(store_details_lf)) {
  # Make a new list, one entry has a dataframe each
  container[[store_url]] <- data.frame(store_details_lf[store_url])
}

stores_lf_df <- bind_rows(container)

# Add URL
stores_lf_df <- stores_lf_df %>% 
  mutate(url = store_links_leafly)

# Export / Save
write.csv(stores_lf_df, "data/leafly_stores.csv")

beepr::beep(5)

