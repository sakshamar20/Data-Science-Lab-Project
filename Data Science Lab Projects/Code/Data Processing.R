library(tidyverse)
library(rvest)
library(dplyr)

# Load Data in R
basket_table <- read.csv("Scraped Data.csv")

# Sort CGPA, Papers and Work Experience

ielts <- numeric(dim(basket_table)[1])

for(i in 1:(dim(basket_table)[1]))
{
  # Making CGPA Range (0, 10)
  if((basket_table$CGPA[i]) > 10) basket_table$CGPA[i] <- round((basket_table$CGPA[i]) / 10, 2)
  
  # Introducing 0 for "N/A" Papers
  if((basket_table$PAPERS[i]) == "N/A") basket_table$PAPERS[i] <- 0
  
  # Introducing 0 for "N/A" Work Experience
  basket_table$WORK.EXPERIENCE[i] <- strsplit(basket_table$WORK.EXPERIENCE[i], " ")[[1]]
  if((basket_table$WORK.EXPERIENCE[i]) == "N/A") basket_table$WORK.EXPERIENCE[i] <- 0
  
  # Segregate into TOEFL and IELTS
  if(basket_table$TOEFL.IELTS[i] == "N/A") ielts[i] <- "N/A"
  else if((as.numeric(basket_table$TOEFL.IELTS[i])) < 10)
  {
    ielts[i] <- basket_table$TOEFL.IELTS[i]
    basket_table$TOEFL.IELTS[i] <- "N/A"
  }
  else ielts[i] <- "N/A"
}

basket_table <- basket_table %>%
  mutate(IELTS = ielts) %>%
  select(UNIVERSITY.NAME, STATUS, TARGET.MAJOR, TERM, GRE.Q., GRE.V., GRE.AWA., GRE.TOTAL., TOEFL.IELTS, IELTS, UG.COLLEGE, UG.MAJOR, CGPA, PAPERS, WORK.EXPERIENCE)

# Making Course Baskets

# For Target Major Courses
Computer <- c("Electrical & Computer Engineering", "Computer Science", "Computer Engineering", 
              "Computing Science", "Applied Computing", "Software Engineering", 
              "Information Management and Systems", "Cyber Security", 
              "Computational Science & Engineering", "Information Technology Management", 
              "Computer & Information Science", "Information Technology", "Computer Networks", 
              "Big Data", "Information Systems")

AI_ML_DS <- c("Data Science", "Data Analytics", "Artificial Intelligence", "Machine Learning", 
              "Robotics", "Computational and Mathematical Engineeri", "Bioinformatics", 
              "Data Science and Business Analytics")

Electrical <- c("Electrical Engineering", "EECS", "Telecommunications Engineering")

Mechanical <- c("Mechanical Engineering", "Industrial Engineering", 
                "Industrial and Systems Engineering")

Chemical <- c("Chemical Engineering", "Chemical and Petroleum Engineering")

Civil <- c("Civil Engineering", "Civil & Environmental Engineering")

Business <- c("Finance", "Business Analytics", "Business Analytics and Information Syste", 
              "MBA", "Business Analytics Flex", "Business Intelligence and Analytics")

Management <- c("Engineering Management", "Information Management", 
                "Supply Chain Management", "Management Science and Engineering")

for(i in 1:(dim(basket_table)[1]))
{
  if((basket_table$TARGET.MAJOR[i] %in% Computer) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Computer Science"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% AI_ML_DS) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "AI_ML_DS"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Electrical) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Electrical Engineering"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Mechanical) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Mechanical Engineering"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Civil) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Civil Engineering"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Chemical) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Chemical Engineering"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Business) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Business Studies"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Management) == TRUE)
  {
    basket_table$TARGET.MAJOR[i] <- "Management Studies"      
  }
  
  else next
}

# For UG Major Courses
Cse <- c("Computer Science", "Information Technology")
EE <- c("EEE", "ECE", "Instrumentation Engineering")
Mech <- c("Industrial Engineering", "Mechanical Engineering") 
Bio <- c("Biotechnology", "Biomedical Engineering")

for(i in 1:(dim(basket_table)[1]))
{
  if((basket_table$UG.MAJOR[i] %in% Cse) == TRUE)
  {
    basket_table$UG.MAJOR[i] <- "Computer Science"      
  }
  
  else if((basket_table$UG.MAJOR[i] %in% EE) == TRUE)
  {
    basket_table$UG.MAJOR[i] <- "Electrical Engineering"      
  }
  
  else if((basket_table$UG.MAJOR[i] %in% Mech) == TRUE)
  {
    basket_table$UG.MAJOR[i] <- "Mechanical Engineering"      
  }
  
  else if((basket_table$TARGET.MAJOR[i] %in% Bio) == TRUE)
  {
    basket_table$UG.MAJOR[i] <- "Biotechnology"      
  }
  
  else next
}

# Updating Column Names
colnames(basket_table) <- c("UNIVERSITY.NAME", "STATUS", "TARGET.MAJOR", "TERM", "GRE.Q.", "GRE.V.", "GRE.AWA.", "GRE.TOTAL.", "TOEFL", "IELTS", "UG.COLLEGE", "UG.MAJOR", "CGPA", "PAPERS", "WORK.EXPERIENCE.M.") 

# Removing "N/A" entries and Changing Data Type Of Some Columns from Character to Integer / Numeric Respectively
basket_table <- basket_table %>%
  filter(GRE.Q. != "N/A", GRE.V. != "N/A", GRE.AWA. != "N/A", GRE.TOTAL. != "N/A", CGPA != "N/A", PAPERS != "N/A", WORK.EXPERIENCE.M. != "N/A", TOEFL != "N/A") %>%
  #filter(!(TOEFL == "N/A" & IELTS == "N/A")) %>%
  mutate(GRE.Q. = as.integer(GRE.Q.), GRE.V. = as.integer(GRE.V.), GRE.AWA. = as.numeric(GRE.AWA.), GRE.TOTAL. = as.integer(GRE.TOTAL.), CGPA = as.numeric(CGPA), PAPERS = as.integer(PAPERS), WORK.EXPERIENCE.M. = as.integer(WORK.EXPERIENCE.M.), TOEFL = as.integer(TOEFL)) %>%
  select(UNIVERSITY.NAME, STATUS, TARGET.MAJOR, TERM, GRE.Q., GRE.V., GRE.AWA., GRE.TOTAL., TOEFL, UG.COLLEGE, UG.MAJOR, CGPA, PAPERS, WORK.EXPERIENCE.M.)

# Saving  Basket Data in .csv file
write.csv(basket_table, "Basket Data.csv")
