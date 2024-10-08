# webscrape for all links
rm(list = ls())

library(rvest)
library(tidyverse)
library(purrr)

# load data
data <- read_csv("links.csv") # link 24 in dataset contains strategy review
strat_review <- data$Link[24]
teleconference_meeting <- data$Link[35]
data <- data[-c(24, 35),]

# create new column with dates

# Function to extract and return date as Date object
extract_date <- function(url) {
  # Use regex to find the date pattern
  date_pattern <- sub(".*mg(\\d{6}).*", "\\1", url)
  
  # Convert to Date object assuming 'yymmdd' format
  date <- as.Date(date_pattern, format = "%y%m%d")
  
  # Return the Date object
  return(date)
}

# Add a new column 'meeting_date' to the dataset
data <- data %>% mutate(date = sapply(Link, extract_date))

data$date <- as.Date(data$date, format = "%y%m%d")

### SECTION 2:

# Initialize empty data frames to store the results for section 2
full_texts_section2 <- data.frame(date = character(), text = character(), stringsAsFactors = FALSE)
sentences_df_section2 <- data.frame(date = character(), sentence = character(), stringsAsFactors = FALSE)

# Iterate through each link
for (i in seq_len(nrow(data))) {
  # Read the HTML content from the link
  minutes <- read_html(data$Link[i])

  
  section_2 <- minutes %>%
    html_nodes(xpath = "//*[self::h3 or self::p][contains(., 'Economic, monetary and financial analyses') or contains(., 'Economic and monetary analyses')]/following-sibling::*[self::p][following-sibling::*[self::h3 or self::p][contains(., 'Monetary policy decisions and communication')]]") %>%
    html_text() %>%
    paste(collapse=" ") %>%
    str_remove_all("[\n\r]") %>% 
    str_replace_all(pattern = "\\s{2,}", replacement = " ")
  
  # Store the full text with the corresponding date in the new data frame
  full_texts_section2 <- rbind(full_texts_section2, data.frame(date = data$date[i], text = section_2))
  
  # Split the text into sentences
  sentences2 <- str_split(section_2, pattern = "(?<=[.!?])\\s+", simplify = FALSE) %>% unlist()
  sentences2 <- sentences2[sentences2 != ""]  # Remove empty sentences
  
  # Prepare the sentence dataframe for this iteration
  sentences_data2 <- data.frame(date = rep(data$date[i], length(sentences2)), sentence = sentences2)
  
  # Append the sentences to the new main dataframe
  sentences_df_section2 <- rbind(sentences_df_section2, sentences_data2)
}

sentences_df_section2$date <- as.Date(sentences_df_section2$date, format = "%d.%m.%Y")

write.csv(full_texts_section2, "full_texts_section2.csv", row.names = FALSE)
write.csv(sentences_df_section2, "sentences_df_section2.csv", row.names = FALSE)




