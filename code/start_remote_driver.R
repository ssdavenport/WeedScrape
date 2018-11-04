# Start the Docker headless driver.
library(docker)
library(RSelenium)

# Terminal command for first-time Firefox setup:
# docker pull selenium/standalone-firefox:2.53.0 

# If installed, enter into terminal:
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
# docker run -d -p 4446:4444 selenium/standalone-firefox:2.53.0 (at RAND)

remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "firefox")
remDr$open()
remDr$setImplicitWaitTimeout(milliseconds=6000) # Timeout


