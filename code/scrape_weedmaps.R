# scrape_weedmaps
library(tidyverse)
library(rvest)
library(testthat)
library(tidyverse)
source('code/functions.R')

# Direct remote driver to index page --------------------------------------
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
source("code/start_remote_driver.R")

# To prevent going to mobile site, set window to a big size!
remDr$setWindowSize(width=1600, height=900, winHand = "current")

wm_index <- "https://weedmaps.com/dispensaries/in/united-states/california"
remDr$navigate(wm_index) # used to work
remDr$navigate("weedmaps.com")
remDr$screenshot(display = TRUE)

# Click on RAND splash screen
remDr$findElement("css", "a")$clickElement()
remDr$screenshot(display = TRUE)

wm_index_html <- read_html_safely(remDr$getPageSource() %>% unlist) # read HTML after loading



# Compile City Index URLs -----------------------------------
# Grab links from left sidebar
city_links_CSS <- "ion-nav-view.pane li a"
city_links <- paste0("https://weedmaps.com",
                     wm_index_html %>%
                     html_nodes(city_links_CSS) %>% 
                     html_attr("href") %>%
                     str_subset("dispensaries") %>%
                     str_replace_all("%2F", "/"))
    
  
write.csv(city_links, "data/weedmaps_city_links_111118.csv")



# Scrape Store URLs, iterating through city index URLs --------------------

city_links <- read_csv("data/weedmaps_city_links_111118.csv") %>% 
  select(-X1) %>% unlist # drop empty column
city_index_url <- city_links[1]


######### DO ONCE FOR ALL STORES (MED OR REC)
# Get details for all stores (med/rec), deploying function for each (gives 1003 links)
links <- list()
for (i in 1:length(city_links)) {
  links[[i]] <- get_city_store_links(city_index_url = city_links[[i]], rec_only = FALSE, delay = 1) %>%
    unlist %>% unique
  if (i %%10 == 0) print(i)
}

# Add link prefix and details page suffix (which can be static-ly scraped)
all_store_links <- store_links %>% lapply( function (x) paste0(x, "#/details")) %>%
  lapply(as.data.frame) %>% bind_rows

all_store_links <- all_store_links %>% map(function(x) str_replace(x, "#/details", "/about")) %>% unlist %>% unique
write.csv(all_store_links, "data/weedmaps_store_links_111518.csv"); beepr::beep(5)

######### DO ONCE FOR ONLY REC


# Get details for all stores (med/rec), deploying function for each (gives 1003 links)
rec_links <- list()
for (i in 1:length(city_links)) {
  rec_links[[i]] <- get_city_store_links(city_index_url = city_links[[i]], 
                                         rec_only = TRUE, delay = 1.5, screenshot=TRUE) %>%
    unlist %>% unique
  if (i %%10 == 0) print(i)
}

# Add link prefix and details page suffix (which can be static-ly scraped)

rec_store_links <- rec_links %>% lapply( function (x) paste0(x, "#/details")) %>%
  lapply(as.data.frame) %>% bind_rows

rec_store_links <- rec_store_links %>% map(function(x) str_replace(x, "#/details", "/about")) %>% unlist %>% unique
write.csv(rec_store_links, "data/weedmaps_store_links_111518_v2_rec.csv"); beepr::beep(5)


########## DO ONCE FOR ONLY MED


# Get details for all stores (med/rec), deploying function for each (gives 1003 links)
med_links <- list()
for (i in 153:length(city_links)) {
  med_links[[i]] <- get_city_store_links(city_index_url = city_links[[i]], 
                                         med_only = TRUE, delay = 1.5, screenshot=TRUE) %>%
    unlist %>% unique
  if (i %%10 == 0) print(i)
}


# Add link prefix and details page suffix (which can be static-ly scraped)
med_store_links <- med_links %>% lapply( function (x) paste0(x, "#/details")) %>%
  lapply(as.data.frame) %>% bind_rows

med_store_links <- med_store_links %>% map(function(x) str_replace(x, "#/details", "/about")) %>% unlist %>% unique

write.csv(med_store_links, "data/weedmaps_store_links_111518_v2_med.csv"); beepr::beep(5)


# Combine links -----------------------------------------------------------

# Because you might not get the full list from a single scrape...
# combine all the med attempts and all the rec attempts

# "Generic" search (med or rec)
generic15 <- read_csv("data/weedmaps_store_links_111518.csv") %>% select(x) %>% unlist
generic11 <- read_csv("data/weedmaps_store_links_111118.csv")  %>% select(x) %>% unlist
generic <- c(generic11, generic15) %>% map(function(x) str_replace(x, "/about/about", "/about")) %>%
  unlist %>% unique %>% str_subset("^h")

# Rec only

# rec14 <- read_csv("data/weedmaps_store_links_111418_rec.csv")%>% select(x) %>% unlist
rec15 <- read_csv("data/weedmaps_store_links_111518_rec.csv")%>% select(x) %>% unlist
rec15_2 <- read_csv("data/weedmaps_store_links_111518_v2_rec.csv")%>% select(x) %>% unlist
rec <- c(rec15, rec15_2) %>% unique %>% str_subset("^h")

# Deleted validation step:
# Why the big diff b/w rec14 and rec15? rec14 included false positives!
# So we are discarding that, bc rec15 and 15_2 agreed and spot checks of rec14 diff are med only
# setdiff(rec14, rec15)[15]

# Med only
med14 <- read_csv("data/weedmaps_store_links_111418_med.csv")%>% select(x) %>% unlist
med15 <- read_csv("data/weedmaps_store_links_111518_med.csv")%>% select(x) %>% unlist
med15_2 <- read_csv("data/weedmaps_store_links_111518_v2_med.csv")%>% select(x) %>% unlist
med <- c(med14, med15, med15_2) %>% unique %>% str_subset("^h")


length(generic)
length(med)
length(rec)



wm_links_type <- full_join(
  data.frame(url=med, med=TRUE),
  data.frame(url=rec, rec=TRUE)) %>%
  full_join(data.frame(url=generic, generic=TRUE))

write_csv(wm_links_type, "data/weedmaps_store_links_111518_type_validated.csv")




# Scrape store info from store URLs ---------------------------------------

# Load list of URLS, w info by if returned from med/rec/generic search
wm_links_type <- read_csv("data/weedmaps_store_links_111518_type_validated.csv")

# Iterate through each URL, collecting store info.
store_details_list <- list()
for (i in 1:length(wm_links_type$url)) {
  store_details_list[[i]] <- tryCatch(get_store_details_wm(wm_links_type$url[[i]], delay = .5),
                                      error = function (e) data.frame(url=wm_links_type$url[[i]]))
  if (i %%10 == 0) print(i)
}
i <- 1038
# Restore to data-frame, add on URL info
stores_wm <- lapply(store_details_list, data.frame) %>% 
  bind_rows() %>% 
  full_join(wm_links_type)
stores_wm %>% tail(30)

# Export for cleaning
write.csv(stores_wm, "data/store_details_wm_nov15.csv"); beepr::beep(5)

beepr::beep(5)
