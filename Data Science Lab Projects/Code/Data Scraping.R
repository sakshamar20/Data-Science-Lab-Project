library(tidyverse)
library(rvest)
library(dplyr)
library(RSelenium)
library(netstat)

# Opening Website on Chrome to scrape Data
driver <- rsDriver(browser = "chrome", chromever = NULL, verbose = F, port = free_port())
rd <- driver$client
rd$open()
rd$navigate("https://admits.fyi/")
rd$maxWindowSize()

# Implicit Wait
rd$setTimeout(type = "page load", milliseconds = 10000)

# Closing the Pop-Up
popup <- rd$findElement(using = 'xpath', '//*[@id="eBookModal"]/div/div/div/button')
popup$clickElement()

# List of College Names to Scrape Data for
college_names <- c("IIT Bombay", "IIT Delhi", "IIT Madras", "IIT Kanpur", "IIT Kharagpur", "IIT Roorkee", "IIT Guwahati")
size <- length(college_names)
page_table <- numeric(size * 10)

for(i in 1:size)
{
  # Filtering out search results for Colleges
  UG_College <- rd$findElement(using = 'xpath', '//*[@id="college"]/div')
  UG_College$clickElement()
  
  UG_Input <- rd$findElement(using = 'xpath', '//thead/tr[1]/th[7]/ul[1]/li[1]/input[1]')
  UG_Input$sendKeysToElement(list(college_names[i])) # Entering College name
  
  Sys.sleep(1)
  
  college <- rd$findElement(using = 'xpath', '//thead/tr[1]/th[7]/ul[1]/li[1]/ul[1]/li[1]/a[1]')
  college$clickElement()
  
  Sys.sleep(1)
  
  next_btn <- rd$findElement(using = 'xpath', '//a[contains(text(),"Next")]')
  
  # Making table for each page
  table <- rd$findElement(using = 'id', 'tbl')
  
  Sys.sleep(1)
  
  # Scraping Data from pages
  for(j in 1:10)
  {
    html <- table$getPageSource()
    page <- read_html(html %>% unlist())
    page_table[10*(i-1)+j] <- html_table(page)
    
    if(j != 10) next_btn$clickElement()
    Sys.sleep(1)
  }
  #Sys.sleep(1)
}
rd$close()
#driver$stop()

# Merging all the page tables together
merged_table <- data.frame(page_table[1])
merged_table <- merged_table[2:11, 1:14]
for(i in 2:(10*size))
{
  add_table <- data.frame(page_table[i])
  add_table <- add_table[2:11, 1:14]
  merged_table <- rbind(merged_table, add_table)
}

# Changing Column Names
colnames(merged_table) <- c("UNIVERSITY NAME","STATUS", "TARGET MAJOR", "TERM", GRE = c("GRE(Q)", "GRE(V)", "GRE(AWA)", "GRE(TOTAL)"), "TOEFL IELTS", "UG COLLEGE", "UG MAJOR", "CGPA", "PAPERS", "WORK EXPERIENCE")

# If want to Shuffle Data
shuffled_table <- data %>% arrange(sample(n()))

# Saving Data in .csv file
write.csv(merged_table, " Scraped Data.csv")
# write.csv(shuffled_data, "Shuffled_Data.csv")

# Final Data in form of tibble
merged_table <- as_tibble(merged_table)
# shuffled_table <- as_tibble(shuffled_table)
