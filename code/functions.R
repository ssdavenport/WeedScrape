# functions

# This function replaces all read_html() calls
read_html_safely <- function(url, times_tried=0, max_trials=5, delay=0, quietly = T) {
  
  # Try getting page html 5 times, or output error
  tryCatch(
    expr = {
      Sys.sleep(delay)
      read_html(url)},
    error =  function(e) {
      times_tried <- times_tried + 1
      if(times_tried == max_trials) {
        if(!quietly) message(paste0("\nno html page for url: ", url))
        return(NA)
      }
      read_html_safely(url, times_tried)
    }
  )
}



# Leafly ------------------------------------------------------------------

# Write function to fetch Leafly store URLs by ZIP
get_zip_store_links_leafly <- function(ZIP, delay=2, screenshots = T) {
  
  # Go to index webpage.
  remDr$navigate(lf_index) 
  Sys.sleep(delay) 
  if(screenshots) remDr$screenshot(T)
  
  # Enter ZIP into textbox; seerch
  # When you enter text box, a drop-down is created
  
  # Find textbox to enter ZIP
  textbox <- remDr$findElement("css", ".search-bar .search-container form label input") # select text box.
  textbox$sendKeysToElement(list(as.character(ZIP))) # type in ZIP code
  Sys.sleep(delay/2) 
  if(screenshots) remDr$screenshot(T)
  
  
  # A result-list will pop up as drop-down! Contains locaitons and maybe also dispensaries
  results <- remDr$findElement("css", ".search-bar .search-container .result-list") 
  resultLinks <- results$findChildElements("xpath", "a")
  
  # Grab the results for the generic ZIP code results (not a dispensary or other location)
  # Dispensaries have "href"  attributes, so we skip these
  # to get right lcoation, make sure it contains the ZIP
  # Grab the non-href link that contains the ZIP code in the text
  locationLink <- remDr$findElement("xpath", paste0("//div[contains(@class,'search-bar')]//a[not(@href) and contains(., 'CA ", ZIP, "')]"))
  locationLink$clickElement()
  if(screenshots) remDr$screenshot(T)
  
  # Now scrape links from the dispensary results (sidebar)!
  dispensaryResults <- remDr$findElement("xpath", "//div[contains(@class,'dispensary-container')]")
  
  # get unique list and simplify this by removing trailing "/reviews" or "/menu"
  dispensaryAnchors <- remDr$findElements("xpath", "//div[contains(@class,'dispensary-container')]//a[@href]")
  dispensaryURLs <- unlist(lapply(dispensaryAnchors, function(x) x$getElementAttribute("href")))
  dispensaryURLsUnique <- str_replace_all(dispensaryURLs, "(/reviews)|(/menu)$", "") %>% unique
  
  # Zoom out! (not implemented, but would find potentially find more stores)
  # zoom_out_button <- remDr$findElement("css", "button[type^='Zoom out']")
  # zoom_out_button$clickElement() # Zoom out once
  # Sys.sleep(.25)
  # zoom_out_button$clickElement() # Zoom out again
  # Sys.sleep(.25)
  dispensaryURLsUnique
}

# TODO: Run this code
# Write function to scrape details from Leafly store pages
scrape_leafly_page <- function(page) {
  # Parse the webpage, looking for attributes
  tryCatch(
    expr = {
      # med / rec / licensed
      misc <- page %>% html_nodes("ul li") %>% html_text()
      spans <- page %>% html_nodes("span") %>% html_text()
      
      
      # Name
      name <- page %>% html_nodes(".dispensary-name")  %>% html_text()
      rating <- page %>% html_nodes(".avg-rating") %>% html_text()
      reviews <- page %>% html_nodes(".review-count") %>% html_text() %>% str_replace(" reviews", "")
      # name <- ifelse(is.null(name), NA, name %>% html_text() %>% str_replace("Home Dispensaries ", ""))
      
      # Get "types" of store
      store_info_tags <- page %>% html_nodes(".container ul li") %>% html_text()
      is_med <- any(str_detect(store_info_tags, "(18 years)|(18\\+)|(recommendation)"))
      is_rec <- any(str_detect(store_info_tags, "(21\\+)"))
      is_delivery <- any(str_detect(store_info_tags, "Delivery"))
      
      # Get long text of store information
      info_text <- page %>% html_nodes(".about-container") %>% html_text() # Info text!
      
      # get miscellaneous info
      info_box <- page %>% html_nodes(".info-box")# %>% html_text()
      address <- info_box %>% html_nodes(".address-container")  %>% html_text()
      
      # Ccontact info
      contact_info <- info_box %>% html_nodes("a") %>% html_attr("href")
      phone <- contact_info %>% str_subset("tel:") %>% str_replace("tel:", "")
      email <- contact_info %>% str_subset("mailto:") %>% str_replace("mailto:", "")
      website <- contact_info %>% str_subset("(https?://)|(www)")
      
      # join date
      joined_date <- spans %>% str_subset("Joined Leafly in ") %>% str_replace("Joined Leafly in ", "")
      
      # schedule
      days <- page %>% html_nodes("div .hours .dayName div") %>% html_text
      hours <- page %>% html_nodes("div .hours .hours div") %>% html_text
      
      
      # //a[not(@href) and contains(., 'CA ", ZIP, "')]"))
      
      # //li[contains(., "Recreational")]
      # is_licensed <- any(str_detect(store_type_info, "Licensed"))
      # # Basic details
      # body <- page %>% html_node('body')
      # title_text <- page %>% html_node('head') %>% html_node('title') %>% html_text()
      # joined_date <- body %>% html_nodes('label > time') %>% magrittr::extract(1) %>% html_text()
      # phone <- body %>% html_node('label[itemprop=telephone]') %>% html_text()
      # address <- body %>% html_node('label[itemprop=streetAddress]') %>% html_text()
      # city <- body %>% html_node('span[itemprop=addressLocality]') %>% html_text()
      # state <- body %>% html_node('span[itemprop=addressRegion]') %>% html_text()
      # # use XPATH to identify the contents of the H6 that says "WEB".
      # web <- body %>% html_nodes(xpath='//h6[text()="Web"]/following::*[1][a]') %>%
      #   html_node('a') %>% html_attr('href') # Get the address of the link right after the Web header
      # about <- body %>% html_nodes(xpath='//h2[text()="About"]/following::*[1]') %>% html_text()
      # hours <- body %>% html_nodes(xpath='//h6[text()="Hours"]/following::*[1]') %>% html_text()
      # # Reviews
      # recent_review_date <- body %>% html_node('header.heading--article > time') %>% html_attr('datetime')
      # if(length(recent_review_date)==0)  recent_review_date <- "No Review"
      # recent_review_content <- body %>% html_node('p[itemprop=reviewBody]') %>% html_text()
      # if(is.na(recent_review_content) | is.null(recent_review_content)  | length(recent_review_content)==0) {
      #   recent_review_content <- "No Review" 
      # }
      # # Menu
      # # Get menu update, if available.
      # if(length(body %>% html_nodes('label > time')) > 1) {
      #   menu_updated_date <- body %>% html_nodes('label > time') %>% magrittr::extract(2) %>% html_text()
      # } else {menu_updated_date <- 'No Menu info'
      
    },
    error = function(e) message(paste0("this page could not load becuase of error: ", e))
  )
  
  # Save output to a list.
  output <- list()
  
  
  # if(!is.null(menu_updated_date)) output[['menu_updated_date']] <- menu_updated_date
  # if(!is.null(city)) output[['city']] <- city
  # if(!is.null(state)) output[['state']] <- state
  # if(!is.null(recent_review_date)) output[['recent_review_date']] <- recent_review_date
  # if(!is.null(recent_review_content)) output[['recent_review_content']] <- recent_review_content
  # if(length(web)==0) output[['web']] <- NA
  # if(!is.null(about)) output[['about']] <- about
  # if(length(about)==0) output[['about']] <- NA
  
  if(!is.null(name)) output[['name']] <- name
  if(!is.null(rating)) output[['rating']] <- rating
  if(!is.null(address)) output[['address']] <- address
  if(!is.null(phone)) output[['phone']] <- phone
  if(!is.null(email)) output[['email']] <- email
  
  if(!is.null(website)) output[['website']] <- website
  if(length(website)==0) output[['website']] <- NA
  
  if(!is.null(hours)) output[['hours']] <- paste0(days, hours, collapse=" ")
  if(length(hours)==0) output[['hours']] <- NA
  
  # if(!is.null(days)) output[['days']] <- days
  # if(length(days)==0) output[['days']] <- NA
  
  store_info_tags
  output[['store_info_tags']] <- store_info_tags %>% paste0(collapse=" - ")
  output[['is_delivery']] <- is_delivery
  output[['is_med']] <- is_med
  output[['is_rec']] <- is_rec
  if(!is.null(info_text)) output[['info_text']] <- info_text
  
  if(!is.null(joined_date)) output[['joined_date']] <- joined_date
  output
}

# Write wrapper function to call page scraper safely
apply_scrape_to_leafly_urls <- function(urls, range=1:length(urls), delay = .5) {
  output <- list()
  output$urls <- urls
  for(i in range) { # Iterate over each URL.
    output$htmls[[i]] <- tryCatch(read_html(urls[i]), 
                                  error=function(e) return(NA))
    Sys.sleep(delay)
    if(!is.na(output$htmls)) {
      output$results[[i]] <- tryCatch(scrape_leafly_page(output$htmls[[i]]),
                                      error=function(e) return(NA))
      output$results[[i]]$url <- urls[i] # add in the URL to the results list.
      
      output$results[[i]]$recent_review_date <- tryCatch(read_html(str_replace(urls[i], "/info", "/reviews")) %>%
                                                           html_node(".review-container .date-desktop") %>% html_text()
                                                           ,
      
      error=function(e) return(NA))
    }
    
    
    if(i %%25 == 0) print(paste0("Copied the ", i, "th link"))
  }
  output
}



# Weedmaps ----------------------------------------------------------------

# Write function that extracts store links from a city index page (Weedmaps)
# It collects medical and rec stores by default, or can be set to rec only
get_city_store_links <- function(city_index_url, 
                                 rec_only = FALSE, 
                                 med_only = FALSE,
                                 screenshot = FALSE, 
                                 delay = 1) {
  
  # Grab all unique dispensary URLs in sidebar
  remDr$navigate(city_index_url) # Get Page Source
  Sys.sleep(delay)
  # remDr$screenshot(display = screenshot)
  
  # If looking for recreational only, then click the rec button.
  # This will mean only rec stores are collected.
  if (rec_only) {
    # Click "medical" once to toggle it off
    remDr$findElement("css", "#license-medical-filter-button")$clickElement()
    Sys.sleep(delay)
    remDr$screenshot(display = screenshot)
  }
  
  if (med_only) {
    # Click "recreational" once to toggle it off
    remDr$findElement("css", "#license-rec-filter-button")$clickElement()
    Sys.sleep(delay)
    remDr$screenshot(display = screenshot)
  }
  
  # Extract store links (sometimes produces 0-length output, if no stores there)
  source <- read_html_safely(remDr$getPageSource() %>% unlist) 
  store_links <- source %>% 
    xml_find_all('//*[@data-ng-switch-default]') %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique
  
  store_links
}


# Write function that extracts key info from store page (Weedmaps)
get_store_details_wm <- function(store_url, delay = 1) {
  
  html_page <- read_html_safely(store_url)
  
  header_info <-
    html_page %>% html_node("body") %>% 
    html_nodes("span#collapsible-description") %>%
    html_text()
    
  store_info <- html_page %>% html_node("body") %>% 
    html_nodes("div#collapsible-container") #div.sc-ifAKCX")
  
  phone <-
    store_info %>% html_text() %>%
    str_extract("\\(?[0-9]{3}\\)? ?-?[0-9]{3}-?[0-9]{4}")
  
  hours <- html_page %>% html_node("body") %>% html_text() %>%
    str_extract("mon.+?sun.+?[(hours)([ap]m)]")
  
  name <- html_page %>% html_node("h1") %>% html_text()
  
  email <- 
    html_page %>% html_node("body") %>% html_text() %>%
    str_extract("Email.+?@.[A-Za-z0-9]*") %>%
      # str_replace("Facebook", "") %>%
      str_replace("Email", "")
  
  address <-
    html_page %>% html_node("body") %>% html_text() %>%
    str_extract("Address.+?[0-9]{5}") %>%
    str_replace("Address:? ?", "") 
  
  licenseIDs <-
    html_page %>% html_node("body") %>% html_text() %>%
    str_extract_all("[AM]-?[0-9]{2}-[0-9]{2}-[0-9]{7}-[0-9A-Za-z]{4}") %>% unlist %>%  unique %>% str_c(collapse=" ")
  
  
  
  about_us <-
    html_page %>% 
      html_text()  %>% 
        str_extract_all("About Us.+?Hours Of Operation") %>% unlist %>%  
      str_replace("About Us", "") %>%
      str_replace("Hours Of Operation", "")
    
  member_since <- html_page %>% 
    html_text() %>%
    str_extract_all("Member Since[0-9]{4}") %>% unlist %>%  
    str_replace("Member Since", "") 
  
  
  # facebook <-
  # html_page %>% 
  #   # html_nodes(".haaqWK")
  #   html_text() %>%
  #   str_extract_all("Facebook.+?/.*") %>% unlist %>%  
  #   str_replace("Facebook", "") 
  
  name <- html_page %>%
    html_node("div h1") %>%
    html_text()
  
  url <- store_url
  
  # member_since <- html_page %>% 
  #   html_text() %>%
  #   str_extract_all("Member Since[0-9]{4}") %>% unlist %>%  
  #   str_replace("Member Since", "") 
  # 
  # member_since <- html_page %>% 
  #   html_text() %>%
  #   str_extract_all("Member Since[0-9]{4}") %>% unlist %>%  
  #   str_replace("Member Since", "") 
  # 
  # ###
  
  
  # html_page %>% 
  #   html_text() %>%
  #   str_extract_all("Member Since[0-9]{4}") %>% unlist %>%  
  #   str_replace("Member Since", "") 
  
  
  
  # Grab fields from the "Details" page.
  # state <- html_page %>% html_node("body") %>% html_nodes("[itemprop=addressRegion]") %>% html_text()
  # licenses <- html_page %>% html_node("body") %>% html_nodes("div .details-licenses") %>% html_text() %>% 
  #   str_c(collapse=" ")
  # name <- html_page %>% html_nodes(".listing-subtitle") %>% html_text() %>% head(1)
  # address <- html_page %>% html_nodes("[itemprop=streetAddress]") %>% html_text()
  # city <- html_page %>% html_nodes("[itemprop=addressLocality]") %>% html_text()
  # ZIP <- html_page %>% html_nodes("[itemprop=postalCode]") %>% html_text()
  # phone <- html_page %>% html_nodes("[itemprop=telephone]") %>% html_text() %>% head(1)
  # phone2 <- html_page %>% html_nodes("[itemprop=telephone]") %>% html_text() %>% head(2) %>% tail(1)
  # email <- html_page %>% html_nodes("[itemprop=email]") %>% html_text() %>% head(1)
  # email2 <- html_page %>% html_nodes("[itemprop=email]") %>% html_text() %>% head(2) %>% tail(1)
  # membersince <- html_page %>% html_nodes("div .details-card-items") %>% html_text() %>%
  #   str_subset("Member Since") %>% str_replace_all("Member Since", "") %>%
  #   str_replace_all("(rd)?(nd)?(st)?(th)?,?", "") %>% as.Date("%b %d %Y")
  # # convert membersince to a date.
  # hits <- html_page %>% html_nodes("div .listing-hits-number") %>% html_text() %>%
  #   str_replace_all(",", "") %>% as.numeric
  # twitter <- html_page %>% html_nodes("div .social-links #twitter") %>% html_text()
  # instagram <- html_page %>% html_nodes("div .social-links #instagram") %>% html_text()
  # facebook <- html_page %>% html_nodes("div .social-links #facebook") %>% html_text()
  # website <- html_page %>% html_nodes("div .social-links") %>% html_text() %>%
  #   str_extract_all("www.*") %>% unlist
  # reviews <- html_page %>% html_nodes("div .listing-reviews-number") %>% html_text() %>%
  #   str_replace_all(",", "") %>% as.numeric
  # description <- html_page %>% html_nodes("div .details-body") %>% html_text() %>% str_c(collapse = "<break>")
  # hours <- html_page %>% html_nodes("div .details-card-items") %>% html_text() %>% str_subset("Sunday")
  # age18 <- html_page %>% html_nodes("div.icon_age_18")
  # age21 <- html_page %>% html_nodes("div.icon_age_21")
  # min_age <- case_when(length(age18) != 0 ~ "18+",
  #                      length(age21) != 0 ~ "21+")
  
  # Grab fields from the "reviews" page
  # review_url <- str_replace(store_url, "/about", "#/reviews")
  # review_page <- read_html_safely(review_url)
  # # Grab latest review date
  # latest_review_date <- review_page %>% 
  #   html_nodes(".created-at .timeago_standard_date") %>% 
  #   html_attr("title")
  # # In case no dates, and we've found a null object, fill w NA
  # if(is.null(latest_review_date)) latest_review_date <- NA 
  # latest_review_date <- as.Date(latest_review_date) %>%
  #   max # get latest date
  # 
  # Format the output.
  
  avg_rating <- html_page %>% html_text() %>% 
    str_extract("[0-9.]* stars") %>%
    str_extract("[0-9.]*")
  
  reviews <- html_page %>% html_text() %>% 
    str_extract("stars by [0-9]* reviews") %>%
    str_extract("[0-9]+")
  
  
  ######### Reviews ####
  remDr$navigate(str_replace(store_url, "about", "reviews")) # load page;
  Sys.sleep(delay) # wait to load
  remDr$screenshot(T)
  review_page <- read_html_safely(remDr$getPageSource() %>% unlist) 
  
  ######### Menu ####
  remDr$navigate(str_replace(store_url, "about", "menu")) # load page;
  Sys.sleep(delay) # wait to load
  remDr$screenshot(T)
  menu_page <- read_html_safely(remDr$getPageSource() %>% unlist) 
  
  
  menu_update_date <- 
    menu_page %>%
    html_nodes('span') %>%
    html_text() %>%
    str_subset("(?i)updated .* ago") %>%
    head(1) %>%
    str_replace_all("Updated ?", "")
  
  
  features <- c("name", "url",
                "phone", "hours", "email", "address", "licenseIDs", 
                "about_us", "member_since", 'menu_update_date',
                "avg_rating", "reviews"
                )
  
  out <- list(name, url,
              phone, hours, email, address, licenseIDs, 
              about_us, member_since, menu_update_date,
              avg_rating, reviews)
  
  
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
