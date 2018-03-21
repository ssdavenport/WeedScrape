# scrape leafly

# Start w LA County ZIP Codes


# Set Up ------------------------------------------------------------------
# Direct the remote browser to the proper page.
lf_index <- "https://www.leafly.com/finder/"
remDr$navigate(lf_index) # used to work
# Click "21+ years old, okay" for first time.
# After first time, cookies makes this unnecessary
remDr$findElement("css", "label.terms-checkbox")$clickElement()
remDr$findElement("css", "button.terms-button")$clickElement()



# Find Stores by ZIP ------------------------------------------------------
# Get list of LA County ZIP Codes
# source("code/compile_lacounty_zipcodes.R")
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2]
# Write function to fetch Leafly store URLs by ZIP
get_zip_store_links_leafly <- function(ZIP, delay=2) {
  # Go to index webpage.
  remDr$navigate(lf_index) # used to work
  # Enter ZIP into textbox; seerch
  textbox <- remDr$findElement("css", "location-changer form input")
  textbox$sendKeysToElement(list(as.character(ZIP))) # type ZIP
  textbox$sendKeysToElement(list("\uE007")) # hit enter (unicode)
  Sys.sleep(delay) # wait
  # Scrape sidebar links as normal.
  zip_stores_as <- remDr$findElements("css", "ul.clearfix a.col-xs-12") # anchors
  zip_stores_urls <- unlist(lapply(zip_stores_as, function(x) x$getElementAttribute("href")))
  zip_stores_urls
}
# Deploy function to get stores for each ZIP
# TODO: run this code.
# WARNING: this seems to be generating different results each time; maybe duplicate?
store_links_leafly <- LAZIPs[1:3] %>%
  map(~get_zip_store_links_leafly(ZIP=., delay = 5)) %>%
  unlist %>%
  unique
# Export / save
write.csv(store_links_leafly, "data/leafly_store_links.csv")



# Scrape details per store ------------------------------------------------
# TODO: Run this code
# Write function to scrape details from Leafly store pages
scrape_leafly_page <- function(page) {
  # Parse the webpage, looking for attributes
  tryCatch(
    expr = {
      # med / rec / licensed
      store_type_info <- page %>% html_nodes("ul.m-pills li") %>% html_text()
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
  if(length(web)==0) output[['web']] <- NA
  if(!is.null(about)) output[['about']] <- about
  if(length(about)==0) output[['about']] <- NA
  if(!is.null(hours)) output[['hours']] <- hours
  if(length(hours)==0) output[['hours']] <- NA
  output[['is_licensed']] <- is_licensed
  output[['is_med']] <- is_med
  output[['is_rec']] <- is_rec
  
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
# Deploy scraper on all store pages
store_details_lf <- store_links_leafly[4:5] %>%
  apply_scrape_to_leafly_urls
# Reshapee to dataframe
stores_lf <- do.call(rbind, lapply(store_details_lf$results, data.frame))
# Export / Save
write.csv(stores_lf, "data/leafly_stores.csv")





# Text from old leafly script ---------------------------------------------


leafly_urls_df <- leafly_scrape$results %>% 
  reduce(dplyr::bind_rows) %>% 
  mutate(
    joined_date = as.Date(joined_date, "%m/%d/%Y"),
    recent_review_date = as.Date(recent_review_date, "%Y-%m-%d"),
    menu_updated_date = as.Date(menu_updated_date, "%m/%d/%Y"))


# Loop through again to get more info that you missed, just for OR stores.
# You want: title, number of reviews.
leafly_or <- leafly_urls_df %>% rowwise() %>%
  mutate(page_body = possibly(~.x %>% read_html() %>% html_nodes("body"), NA)(url),
         page_head = possibly(~.x %>% read_html() %>% html_nodes("head"), NA)(url),
         name = possibly(~.x %>% html_node("h1[itemprop=name]") %>% html_text(), NA)(page_body),
         num_reviews = possibly(~.x %>% 
                                  html_node('header.heading--article:contains("Recent Reviews") > a') %>%
                                  html_text() %>%
                                  str_extract_all("[0-9]+"), NA)(page_body))

# saveRDS(leafly_or, "leafly oregon.Rds")

# We can see that most datasets have stores mostly opening post-2014
leafly_or %>% ggplot(aes(x=joined_date)) + geom_histogram()
wm_scrape_df %>% ggplot(aes(x=membersince)) + geom_histogram()

# Individually look at places that opened pre-2014?
leafly_or %>% ggplot(aes(x=joined_date, y= menu_updated_date)) + geom_point()
leafly_or %>% ggplot(aes(x=recent_review_date, y= menu_updated_date)) + geom_point()

