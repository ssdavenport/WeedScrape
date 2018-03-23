# Clean datasets
library(tidyverse)

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
                          !is_med & !is_rec ~ "No Type (?)"),
         med_license = str_extract_all(about, "M[0-9\\-A-Za-z]{8,}"),
         rec_license = str_extract_all(about, "A[0-9\\-A-Za-z]{8,}"),
         shows_med_license = str_detect(about, "M[0-9\\-A-Za-z]{8,}"),
         shows_rec_license = str_detect(about, "A[0-9\\-A-Za-z]{8,}"),
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
# Save/Export
write.csv(wm_stores, "wm_stores_cleaned.csv")
