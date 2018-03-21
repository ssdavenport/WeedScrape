# Get list of LA County ZIPCodes

# We require a list of LA County ZIP Codes for use to iterate through Leafly.com/finder.

# Load libraries
library(tabulizer)
library(dplyr)
library(rJava)

# Download PDF containg ZIP Master List provided by Eric Pederson
LAzip_pdf <- "http://file.lacounty.gov/SDSInter/lac/1031552_MasterZipCodes.pdf" 

# Extract each of the 10 pages of tables.
LAzip_tbls <- extract_tables(LAzip_pdf, encoding="UTF-8") 

# In each, first column holds ZIPs + city name.
# Make a vector that holds everything from the first col.
# Then do a regexp to get ZIPs only.
# Finally, save to file for import elsewhere.
zip_cities <- c()
for (i in seq_along(LAzip_tbls)) zip_cities <- c(zip_cities, LAzip_tbls[[i]][, 1])
zips <- stringr::str_extract_all(zip_cities, "[0-9]{5}") %>% unlist
write.csv(zips, "data/LACountyZIPs.csv")
