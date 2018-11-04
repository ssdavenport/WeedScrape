# scrape leafly
source("code/functions.R")
library(rvest)
library(tidyverse)
# Start w LA County ZIP Codes

# Set Up ------------------------------------------------------------------

# Start remote driver (Docker); follow instructions in Terminal
source("code/start_remote_driver.R")

# To prevent going to mobile site, set window to a big size!
remDr$setWindowSize(width=1600, height=900, winHand = "current")


### First time running, you must manually guide past the "21+?" splash page
# Click "21+ years old, okay" for first time.
# After first time, cookies makes this unnecessary
# Are you eligible to visit? (21+ years old)

# Direct the remote browser to the proper page.
lf_index <- "https://www.leafly.com/finder/"
remDr$getCurrentUrl()
remDr$navigate(lf_index) # used to work
remDr$screenshot(T)

# remDr$findElement("css", "label.terms-checkbox")$clickElement() # click old enough (broken?)
# remDr$findElement("css", "label.terms-checkbox")$clickElement() # click checkbox?
# remDr$findElement("css", "button.terms-button")$clickElement() # click continue
remDr$findElement("css", "input#eligibility-tou-confirm")$clickElement() # click old enough
remDr$findElement("css", "button#tou-continue")$clickElement() # click old enough


# Find Stores by ZIP ------------------------------------------------------

# Get list of LA County ZIP Codes
# source("code/compile_lacounty_zipcodes.R")
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2]
# LAZIPs <- LAZIPs[3] # test with small version

# Deploy function to get stores for each ZIP
# WARNING: this seems to be generating different results each time; maybe duplicate?
# sometimes errors. why?
# Deploy this with a bunch of try-expect loops!
# LAZIPs <- LAZIPs[4:5]

# Make a list of all zip codes with their store URLs
# zip_list <- list()
# error_zips <- c()
error_zips <- error_zips[!is.na(error_zips)]
for (zip_i in error_zips) {#3:length(LAZIPs)
  
  zip <- LAZIPs[zip_i]
  zip_list[[zip_i]] <- list()
  zip_list[[zip_i]]$zip <- zip
  
  URL <- tryCatch(expr=get_zip_store_links_leafly(ZIP=zip, delay = 1, screenshots=T),
            error = function (e) {
              tryCatch(expr = get_zip_store_links_leafly(ZIP=zip, delay = 2, screenshots=T),
                       error = function (e) {
                         tryCatch(expr = get_zip_store_links_leafly(ZIP=zip, delay = 3, screenshots=T),
                                                      error = function (e) {
                                                        error_zips <<- c(error_zips, zip)
                                                        print(zip_i)
                                                      })
                       })
            })
  zip_list[[zip_i]]$url <- URL
  if (zip_i%%10==0) message(paste0("finished zip code #", zip_i))
}
zip_i
error_zips %>% write.csv("data/error_zips_leafly.csv")
disp_urls <- lapply(zip_list, function(x) x$url) %>% unlist %>% unique %>% str_subset("leafly")
### NEXT: MANUALLY PATCH UP ZIPs with ERRORs

# Export / save

# write.csv(disp_urls, "data/leafly_store_links_nov42018.csv")

# Scrape details per store ------------------------------------------------
# disp_urls <- read.csv("data/leafly_store_links.csv") %>% select(-X)
ptm <- proc.time()

# Deploy scraper on all store pages
# store_details_lf <- vector(mode='list', length(disp_urls))

disp_urls <-  paste0(disp_urls, "/info")
page_scrapes <- apply_scrape_to_leafly_urls(disp_urls)

# TODO NEXT:
# also grab date of most recent review?

# Combine into single data_frame ----------------------------------------------------

stores_lf <- data.table::rbindlist(page_scrapes$results, fill = TRUE)
View(page_scrapes$results)
View(stores_lf)

# Export / Save
write.csv(stores_lf, "data/leafly_stores_nov42018.csv")

beepr::beep(5)

