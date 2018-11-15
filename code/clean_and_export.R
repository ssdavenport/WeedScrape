# Clean datasets
library(tidyverse)
source("code/functions.R")
# Obejctive: For both sites...
# ... Identify medical v recreational stores 
# ... Look for license number?


# Prep --------------------------------------------------------------------

lf <- read.csv("data/leafly_stores_nov42018.csv") %>% select(-X)

# Parse hours of the day
lf <- lf %>%
  mutate(hours_m = str_extract(hours, "Monday.*Tuesday") %>% str_replace("Monday", "") %>% str_replace("Tuesday", ""),
         hours_t = str_extract(hours, "Tuesday.*Wednesday") %>% str_replace("Tuesday", "") %>% str_replace("Wednesday", ""),
         hours_w = str_extract(hours, "Wednesday.*Thursday") %>% str_replace("Wednesday", "") %>% str_replace("Thursday", ""),
         hours_r = str_extract(hours, "Thursday.*Friday") %>% str_replace("Thursday", "") %>% str_replace("Friday", ""),
         hours_f = str_extract(hours, "Friday.*Saturday") %>% str_replace("Friday", "") %>% str_replace("Saturday", ""),
         hours_sa = str_extract(hours, "Saturday.*Sunday") %>% str_replace("Saturday", "") %>% str_replace("Sunday", ""),
         hours_su = str_extract(hours, "Sunday.*[(AM)(PM)(hours)]")  %>% str_replace("Sunday", "") %>% str_replace("(hours)|(am)|(pm)", "")) %>% 
  # Add state
  mutate(state = str_extract(address, "[A-Za-z]*$") %>% toupper(),
         city = str_extract_all(address, "[A-Za-z]*, [A-Za-z]*") %>% str_extract("[A-Za-z]*")) %>% 
  # Add "storefront" tag
  mutate(is_storefront = str_detect(store_info_tags, "Storefront"),
         
         # Clean name
         name = as.character(name) %>% str_replace_all("[\n]", "") %>% str_replace_all(" {2,}", " ") %>% str_replace_all("^ *", ""),
         name = str_replace_all(name, " - .*", "") %>% str_replace_all("Home Dispensaries", ""))

# get imputed license type.
lf <- lf %>% 
  mutate(type = case_when(is_med & is_rec ~ "Rec/Med",
                          is_med & !is_rec ~ "Med",
                          !is_med & is_rec ~ "Rec",
                          # There are 3 "No Type" cases: 2 delivery; 1 unlabeled
                          !is_med & !is_rec ~ "Unclear"),
         med_license = str_extract_all(info_text, "M[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         rec_license = str_extract_all(info_text, "A[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         shows_med_license = str_detect(info_text, "M[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         shows_rec_license = str_detect(info_text, "A[0-9]{2}[0-9\\-A-Za-z]{6,}"), 
         license_types_shown = case_when(shows_med_license & shows_rec_license ~ "Rec/Med",
                                         shows_med_license & !shows_rec_license ~ "Med",
                                         !shows_med_license & shows_rec_license ~ "Rec",
                                         !shows_med_license & !shows_rec_license ~ "Neither"),
         type_imputed = case_when(license_types_shown == "Rec/Med" | type == "Rec/Med" ~ "Rec/Med",
                                  license_types_shown == "Rec" & type == "Med" ~ "Rec/Med",
                                  license_types_shown == "Med" & type == "Rec" ~ "Rec/Med",
                                  license_types_shown == "Med" | type == "Med" ~ "Med", # either has med -> med
                                  license_types_shown == "Rec" | type == "Rec" ~ "Rec", # either has rec -> rec
                                  TRUE ~ "Unclear"),
         
         delivery = case_when(is_delivery & is_storefront ~ "Storefront + Delivery",
                                      !is_delivery & is_storefront ~ "Storefront Only",
                                      is_delivery & !is_storefront ~ "Delivery Only",
                                      TRUE ~ "Unclear")
         ) %>%
  select(-shows_med_license,
         -shows_rec_license,
         -license_types_shown,
         -is_delivery,
         -is_storefront,
         -type) %>%
  rename(type=type_imputed)

# Miscellaneous
lf <- lf %>% 
  mutate(recent_review_date = as.Date(recent_review_date)) %>%
  rowwise %>%
  mutate(med_license = ifelse(is.null(med_license), NA, med_license),
         rec_license = ifelse(is.null(rec_license), NA, rec_license))

# Export with final coluns
lf_clean <- lf %>%
  select(name, url, type, delivery, state, city, address, phone, email, website,
         joined_year = joined_date, recent_review_date, rating, hours_m:hours_su, About = info_text
         
  ) 


lf_clean %>% 
  mutate(About = str_replace_all(About, "[\r\n\t]+", " ") %>% tolower) %>%
  `colnames<-`(tools::toTitleCase(colnames(.))) %>%
  write.csv("output/Leafly_cleaned_nov152018.csv")



# WeedMaps ----------------------------------------------------------------

# Load Data
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2] # ZIPs
wm_r <- read.csv("data/store_details_wm_rec.csv") %>% select(-X) #%>%
wm_all <- read.csv("data/store_details_wm.csv") %>% select(-X) #%>%
# Check if there are any rec stores that did not appear in med/rec search
any(!(wm_r$url %in% wm_all$url)) # any left out? should resolve to FALSE
# Get a list of ALL the urls (med or rec), and tag them acc'g to group
wm_urls <- unique(c(as.character(wm_r$url), as.character(wm_all$url)))
# Starting w URLs, identify by type, then add on details from scrape.
wm_stores <- tibble(url=wm_urls) %>% 
  mutate(type = ifelse(url %in% wm_r$url, "Rec or Med/Rec only",
                       ifelse(url %in% wm_all$url, "Med Only", "Unknown"))) %>%
  left_join(wm_all) %>% 
  # Detect if ZIP is in LA County, extract med/rec license #  
  mutate(LACounty_ZIP = ZIP %in% LAZIPs,
         med_license = str_extract(description, "M[0-9]{2}\\-[0-9]{2}\\-[0-9]{7}(\\-TEMP)?"),
         rec_license = str_extract(description, "A[0-9]{2}\\-[0-9]{2}\\-[0-9]{7}(\\-TEMP)?"),
         latest_review_date = as.Date(latest_review_date))
# Diagnose
sapply(wm_stores, function(x) mean(is.na(x)))
wm_stores %>% filter(LACounty_ZIP) %>% .$type %>% table
wm_stores %>% filter(LACounty_ZIP) %>% .$med_license %>% unlist %>% table
wm_stores %>% filter(LACounty_ZIP) %>% .$rec_license %>% unlist %>% table


wm_stores <- wm_stores %>% 
  mutate(days_inactive= max(as.Date(latest_review_date), na.rm=T) - as.Date(latest_review_date),
         address = as.character(address),
         address = ifelse(nchar(address) < 5 | is.na(address), "Call for Address", address),
         address = tools::toTitleCase(tolower(address))) %>%
  select(Name=name, Address=address, City=city, ZIP, 
         LACounty=LACounty_ZIP,
         Phone=phone, Email=email,
         Days_Inactive=days_inactive, Joined=membersince,
         Reviews=reviews,
         Min_Age = min_age,
         Med_License=med_license, Rec_License=rec_license,
         Twitter=twitter, IG=instagram, FB=facebook, Website=website,
         Phone2=phone2, Email2=email2, Hits=hits,
         About=description) 


wm_stores <- wm_stores %>% 
  mutate(About = str_replace_all(About, "[\r\n\t]+", " ") %>% tolower,
         About = str_replace_all(About, "<break>", " "),
         About = iconv(About, to = "ASCII//TRANSLIT"), 
         Name = iconv(Name, to = "ASCII//TRANSLIT"), 
         About = ifelse(nchar(About) < 5 | is.na(About), " ", About),
         Med = str_detect(About, "(?i)medical"),
         Rec = str_detect(About, "(?i)recreational") | 
           str_detect(About, "(?i)adult[\\- ]use"))


# Save/Export
write.csv(wm_stores, "output/Weedmaps_cleaned.csv")

         
# NOTE: Try to find date of most recent review?
