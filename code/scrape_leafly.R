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
write.csv(stores_lf_df, "data/leafly_stores_nov2018.csv")

beepr::beep(5)

