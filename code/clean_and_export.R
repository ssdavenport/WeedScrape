# Clean datasets
library(tidyverse)
source("code/functions.R")
# Obejctive: For both sites...
# ... Identify medical v recreational stores 
# ... Look for license number?



# Leafly ------------------------------------------------------------------

lf <- read.csv("data/leafly_stores_nov152018.csv") %>% select(-X)
lf16 <- read.csv("data/leafly_stores_nov162018.csv") %>% select(-X)

lf <- bind_rows(list(lf, lf16)) %>% unique


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

# lf[35, ]
# get imputed license type.
lf <- lf %>% 
  mutate(is_med = str_detect(store_info_tags, "Medical"),
         is_rec = str_detect(store_info_tags, "Recreational"),
         type = case_when(is_med & is_rec ~ "Rec/Med",
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
         joined_year = joined_date, recent_review_date, rating, hours_m:hours_su, About = info_text)  %>% 
  mutate(About = str_replace_all(About, "[\r\n\t]+", " ") %>% tolower) %>%
  `colnames<-`(tools::toTitleCase(colnames(.)))

table(lf_clean$Type)
# Export
lf_clean %>% write.csv("output/Leafly_cleaned_nov16_2018.csv")

lf_clean15 <-  read.csv("output/Leafly_cleaned_nov152018.csv") %>% 
  select(-X) %>%
  mutate(Recent_review_date = as.Date(as.character(Recent_review_date)))

lf_clean17 <- bind_rows(list(lf_clean,
               lf_clean15[!lf_clean15$Name %in% lf_clean$Name, ])) 

lf_clean17 %>% write.csv("output/Leafly_cleaned_nov15final_2018.csv")

 

# WeedMaps ----------------------------------------------------------------
stores_wm <- read.csv("data/store_details_wm_nov15.csv") %>% select(-X)#; beepr::beep(5)

# Load Data
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2] # ZIPs

# Add ZIP (works for almost all)
stores_wm <- stores_wm %>%
  mutate(zip = str_extract(address, "9[0-9]{4}") %>% as.numeric,
         LACounty_ZIP = zip %in% LAZIPs,
         address = as.character(address),
         address = ifelse(nchar(address) < 5 | is.na(address), "Call for Address", address),
         address = tools::toTitleCase(tolower(address)),
         type = case_when(med & rec ~ "Med/Rec",
                          med ~ "Med Only",
                          rec ~ "Rec Only",
                          TRUE ~ "Missing Info"),
         about_us = tolower(about_us)) %>%
  
  # Add hours
  mutate(hours_m = str_extract(hours, "mon.*tue") %>% str_replace("mon", "") %>% str_replace("tue", ""),
         hours_t = str_extract(hours, "tue.*wed") %>% str_replace("tue", "") %>% str_replace("wed", ""),
         hours_w = str_extract(hours, "wed.*thurs") %>% str_replace("wed", "") %>% str_replace("thurs", ""),
         hours_r = str_extract(hours, "thurs.*fri") %>% str_replace("thurs", "") %>% str_replace("fri", ""),
         hours_f = str_extract(hours, "fri.*sat") %>% str_replace("fri", "") %>% str_replace("sat", ""),
         hours_sa = str_extract(hours, "sat.*sun") %>% str_replace("sat", "") %>% str_replace("sun", ""),
         hours_su = str_extract(hours, "sun.*[(am)(pm)(hours)]")  %>% str_replace("sun", "") %>% str_replace("(hours)|(am)|(pm)", "")) %>% 
  # Add state
  mutate(state = str_extract(address, "[A-Za-z]*$") %>% toupper(),
         city = str_extract_all(address, "[A-Za-z]*, [A-Za-z]*") %>% str_extract("[A-Za-z]*")) %>%
  mutate(city = str_replace(city, "^(?i)drive|ave|blvd|circle|address|street|rd|parkway|way|road|harbor|walk|place|st|hwy", ""))

# Diagnose
table(is.na(stores_wm$zip)) # all but about ~15 have ZIPs
table(stores_wm$LACounty_ZIP) # 557 in LACounty

table(stores_wm$type)

table(stores_wm$LACounty_ZIP, stores_wm$type) # 557 in LACounty
# sum(is.na(stores_wm$LACounty_ZIP))

stores_wm_clean <- stores_wm %>%
  select(name, url, type, state, city, address, phone, email,
       joined_year = member_since, 
       menu_update_date, avg_rating, reviews, hours_m:hours_su, about_us, licenseIDs) %>%
  `colnames<-`(tools::toTitleCase(colnames(.)))

# Save/Export
# write.csv(stores_wm_clean, "output/Weedmaps_cleaned_nov152018.csv")

         
# NOTE: Try to find date of most recent review?
