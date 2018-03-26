# Clean datasets
library(tidyverse)
source("code/functions.R")
# Obejctive: For both sites...
# ... Identify medical v recreational stores 
# ... Look for license number?

 

# Leafly ------------------------------------------------------------------
# Load, identify stores removed BC unlicensed
lf <- read.csv("data/leafly_stores.csv") %>% 
  select(-X, -X.1) %>%
  mutate(rmvd_unlicensed =  !is_med & !is_rec & is.na(city),
         type = case_when(is_med & is_rec ~ "Rec/Med",
                          is_med & !is_rec ~ "Med",
                          !is_med & is_rec ~ "Rec",
                          rmvd_unlicensed & !is_med & !is_rec ~ "Removed", # 13 have page down
                          # There are 3 "No Type" cases: 2 delivery; 1 unlabeled
                          !is_med & !is_rec ~ "Unclear"),
         med_license = str_extract_all(about, "M[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         rec_license = str_extract_all(about, "A[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         shows_med_license = str_detect(about, "M[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         shows_rec_license = str_detect(about, "A[0-9]{2}[0-9\\-A-Za-z]{6,}"),
         license_types_shown = case_when(shows_med_license & shows_rec_license ~ "Rec/Med",
                                         shows_med_license & !shows_rec_license ~ "Med",
                                      !shows_med_license & shows_rec_license ~ "Rec",
                                      !shows_med_license & !shows_rec_license ~ "Neither"),
         type_imputed = case_when(license_types_shown == "Rec/Med" | type == "Rec/Med" ~ "Rec/Med",
                                  license_types_shown == "Rec" & type == "Med" ~ "Rec/Med",
                                  license_types_shown == "Med" & type == "Rec" ~ "Rec/Med",
                                  license_types_shown == "Med" | type == "Med" ~ "Med", # either has med -> med
                                  license_types_shown == "Rec" | type == "Rec" ~ "Rec", # either has rec -> rec
                                  type == "Removed" ~ "Removed"))

# Validate the store type data.
table(lf$license_types_shown, lf$type) # Confusion matrix for two license type approaches
table(lf$type_imputed) # Licenses by best guess @ type

#TODO: Add columns for medical/adult-use license IDs


# WeedMaps ----------------------------------------------------------------
# Load Data
LAZIPs <- read.csv("data/LACountyZIPs.csv")[, 2] # ZIPs
wm_r <- read.csv("output/store_details_wm_rec.csv") %>% select(-X) #%>%
wm_all <- read.csv("output/store_details_wm.csv") %>% select(-X) #%>%
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
         rec_license = str_extract(description, "A[0-9]{2}\\-[0-9]{2}\\-[0-9]{7}(\\-TEMP)?"))
# Diagnose
sapply(wm_stores, function(x) mean(is.na(x)))
wm_stores %>% filter(LACounty_ZIP) %>% .$type %>% table
wm_stores %>% filter(LACounty_ZIP) %>% .$med_license %>% unlist %>% table
wm_stores %>% filter(LACounty_ZIP) %>% .$rec_license %>% unlist %>% table

wm_stores <- wm_stores %>% 
  mutate(days_inactive= max(as.Date(latest_review_date)) - as.Date(latest_review_date),
         address = as.character(address),
         address = ifelse(nchar(address) < 5 | is.na(address), "Call for Address", address),
         address = tools::toTitleCase(tolower(address))) %>%
  select(Name=name, Address=address, City=city, ZIP, 
         LACounty=LACounty_ZIP,
         Phone=phone, Email=email,
         Days_Inactive=days_inactive, Joined=membersince,
         Reviews=reviews,
         Med_License=med_license, Rec_License=rec_license,
         Twitter=twitter, IG=instagram, FB=facebook, Website=website,
         Phone2=phone2, Email2=email2, Hits=hits,
         About=description) 

wm_stores <- wm_stores %>% 
  mutate(About = str_replace_all(About, "[\r\n\t]+", " ") %>% tolower,
         About = str_replace_all(About, "<break>", " "),
         About = iconv(About, to = "ASCII//TRANSLIT"), 
         Name = iconv(Name, to = "ASCII//TRANSLIT"), 
         About = ifelse(nchar(About) < 5 | is.na(About), " ", About))

wm_stores[is.na(wm_stores)] <- " "

head(wm_stores)
# Save/Export
write.csv(wm_stores, "output/Weedmaps_cleaned.csv")

         
# NOTE: Try to find date of most recent review?

# More --------------------------------------------------------------------

lf <- lf %>% 
  mutate(recent_review_date = as.Date(recent_review_date),
         address = as.character(address)) %>%
  rowwise %>%
  mutate(med_license = ifelse(is.null(med_license), NA, med_license),
         rec_license = ifelse(is.null(rec_license), NA, rec_license),
         last_active = max(recent_review_date, menu_updated_date)) %>%
  ungroup %>%
  mutate(days_inactive = max(last_active, na.rm=T) - last_active,
         address = ifelse(nchar(address) < 5 | is.na(address), "Call for address", address),
         address = tools::toTitleCase(tolower(address))
         )

lf <- lf %>%
  select(URL=url, City=city, Address=address, Phone=phone,
         Days_Inactive=days_inactive, Joined=joined_date, 
         About=about, Web=web, #hours, 
         Type=type,
         Med_License=med_license,
         Rec_License=rec_license) 

lf %>% 
  mutate(About = str_replace_all(About, "[\r\n\t]+", " ") %>% tolower) %>%
  write.csv("output/Leafly_cleaned.csv")

              # about = str_replace(about, "[^[:alnum:]]", "[^a-zA-Z0-9]"))          %>%
  .$about %>% head

lf$about %>% head

         

