# Load necessary library
library(tidyverse)
library(ggplot2)
set.seed(333)


# Add a column with row numbers to sentences_df_section2
sentences_df_section2$row_number <- seq.int(nrow(sentences_df_section2))

View(sentences_df_section2)


# Assuming sentences_df_section2 is already loaded into the environment
# Extract the year from the date column and create a new column 'year'
sentences_df_section2 <- sentences_df_section2 %>%
  mutate(year = as.integer(format(as.Date(date), "%Y")))

# Calculate the number of sentences per date
date_counts <- sentences_df_section2 %>%
  group_by(date) %>%
  summarise(sentences_per_date = n()) %>%
  ungroup()

# Calculate the total number of sentences across all dates
total_sentences <- sum(date_counts$sentences_per_date)

# Calculate the proportional samples needed for each date
date_counts <- date_counts %>%
  mutate(proportional_samples = round(500 * (sentences_per_date / total_sentences)))

# Add the proportional samples and sentences_per_date information back to the original dataframe
sentences_df_section2 <- sentences_df_section2 %>%
  left_join(date_counts, by = "date")

# Sample the sentences according to the proportional samples calculated for each date
sampled_sentences <- sentences_df_section2 %>%
  group_by(date) %>%
  sample_n(proportional_samples[1], replace = FALSE) %>%
  ungroup()

# Ensure the total number of sampled sentences is 500
total_sampled <- nrow(sampled_sentences)
if (total_sampled < 500) {
  # If there are less than 500, sample more from dates with more sentences
  remaining_samples <- 500 - total_sampled
  additional_samples <- sentences_df_section2 %>%
    filter(!(row_number %in% sampled_sentences$row_number)) %>%
    arrange(desc(sentences_per_date)) %>%
    group_by(date) %>%
    sample_n(min(n(), remaining_samples), replace = FALSE) %>%
    ungroup() %>%
    slice_head(n = remaining_samples)
  sampled_sentences <- bind_rows(sampled_sentences, additional_samples)
} else if (total_sampled > 500) {
  # If there are more than 500, sample less
  sampled_sentences <- sampled_sentences %>%
    sample_n(500, replace = FALSE)
}

# Check the distribution of the number of sampled sentences for each date
distribution <- table(sampled_sentences$date)
print(distribution)

### MAYBE ADD PLOTS TO DISS
plot(distribution)

# Add sampling probability to the sampled sentences
sampled_sentences <- sampled_sentences %>%
  mutate(sampling_prob = proportional_samples / sentences_per_date) %>%
  select(date, sentence, sampling_prob, row_number)

# Print the sampled sentences with sampling probabilities
View(sampled_sentences)

# Create a CSV file for sampled_sentences
write.csv(sampled_sentences, "gold_standard_prop.csv", row.names = FALSE) 


### SAMPLE FOR SURROGATE LABELS
set.seed(333)

full_sentences <- sentences_df_section2

# Remove the sampled sentences from the full dataset
remaining_sentences_df <- full_sentences %>%
  filter(!(row_number %in% sampled_sentences$row_number))

# Calculate the number of sentences per date for the remaining sentences
date_counts_remaining <- remaining_sentences_df %>%
  group_by(date) %>%
  summarise(sentences_per_date = n()) %>%
  ungroup()

# Add sentences_per_date information back to the remaining_sentences_df
remaining_sentences_df <- remaining_sentences_df %>%
  left_join(date_counts_remaining, by = "date")

# Calculate the total number of sentences across all dates for the remaining sentences
total_sentences_remaining <- sum(date_counts_remaining$sentences_per_date)

# Calculate the proportional samples needed for each date for the remaining sentences
date_counts_remaining <- date_counts_remaining %>%
  mutate(proportional_samples_2 = round(1000 * (sentences_per_date / total_sentences_remaining)))

# Add the proportional samples information back to the remaining dataframe
remaining_sentences_df <- remaining_sentences_df %>%
  left_join(date_counts_remaining, by = "date")

# Sample the sentences according to the proportional samples calculated for each date
additional_sampled_sentences <- remaining_sentences_df %>%
  group_by(date) %>%
  sample_n(proportional_samples_2[1], replace = FALSE) %>%
  ungroup()

# Ensure the total number of sampled sentences is 1000
total_sampled_remaining <- nrow(additional_sampled_sentences)
if (total_sampled_remaining < 1000) {
  # If there are less than 1000, sample more from dates with more sentences
  remaining_samples_needed <- 1000 - total_sampled_remaining
  additional_samples_needed <- remaining_sentences_df %>%
    filter(!(row_number %in% additional_sampled_sentences$row_number)) %>%
    arrange(desc(sentences_per_date)) %>%  # Arrange by sentences_per_date to get dates with more sentences
    group_by(date) %>%
    sample_n(min(n(), remaining_samples_needed), replace = FALSE) %>%
    ungroup() %>%
    slice_head(n = remaining_samples_needed)
  additional_sampled_sentences <- bind_rows(additional_sampled_sentences, additional_samples_needed)
} else if (total_sampled_remaining > 1000) {
  # If there are more than 1000, sample less
  additional_sampled_sentences <- additional_sampled_sentences %>%
    sample_n(1000, replace = FALSE)
}

# Check the distribution of the number of sampled sentences for each date
distribution_remaining <- table(additional_sampled_sentences$date)
print(distribution_remaining)

### MAYBE ADD PLOTS OF DISTRIBUTION TO DISS
plot(distribution_remaining)

# Add sampling probability to the sampled sentences
additional_sampled_sentences <- additional_sampled_sentences %>%
  mutate(sampling_prob = proportional_samples_2 / sentences_per_date)%>%
  select(date, sentence, sampling_prob, row_number)

# Print the additional sampled sentences with sampling probabilities
View(additional_sampled_sentences)

# Prepare the surrogate sample data frame
surrogate_sample <- additional_sampled_sentences %>% 
  select(date, sentence, sampling_prob, row_number)

View(surrogate_sample)

# Create a CSV file for surrogate_sample
write.csv(surrogate_sample, "surrogate_sample_prop.csv", row.names = FALSE)



### Cleaning dataset with surrogate labels for HF AutoTrain
surrogate_labels <- read.csv("surrogate_labels.csv")
View(surrogate_labels)

surrogate_labels <- surrogate_labels %>% 
  select(date, text, label_llm_cot_multiple, sampling_prob, row_number)

# change names of columns to "date", "sentence", "label", and "label_num"
colnames(surrogate_labels) <- c("date", "sentence", "label", "sampling_prob", "row_number")

#print any rows that do not include "uncertain" or "certain" in the label column
surrogate_labels %>% 
  filter(!grepl("uncertain|certain", label))

# change the rows that do not include "uncertain" or "certain" in the label column to "certain"
surrogate_labels <- surrogate_labels %>% 
  mutate(label = ifelse(!grepl("uncertain|certain", label), "certain", label))
  
View(surrogate_labels)


write.csv(surrogate_labels, "surrogate_labels_at.csv", row.names = FALSE)

at_train_data <- surrogate_labels

# load expert-coded gold standard data
gold_labels <- read.csv("gold_standard_prop.csv")

gold_labels$date <- as.Date(gold_labels$date, format = "%d.%m.%Y")

View(gold_labels)

# create training data for HF AutoTrain
at_train_data <- rbind(surrogate_labels, gold_labels)

# create validation dataset from at_train_data
set.seed(333)

at_val_data <- at_train_data %>% 
  sample_n(nrow(at_train_data)*0.2, replace = FALSE) 

View(at_val_data)

# remove validation data from training data
at_train_data <- at_train_data %>%
  filter(!(row_number %in% at_val_data$row_number))
  
View(at_train_data)


#create csv file for training data and validation data for HF AutoTrain
write.csv(at_train_data, "at_train_data.csv", row.names = FALSE)
write.csv(at_val_data, "at_valid_data.csv", row.names = FALSE)


# final dataset for labeling

sentences_final <- sentences_df_section2 %>% 
  select(date, sentence, row_number)

# add binary indicator to sentences_final to indicate if the sentence is in sampled_sentences
sentences_final$gold_standard <- ifelse(sentences_final$row_number %in% sampled_sentences$row_number, 1, 0)

# add column label from gold_labels to sentences_final, match it to relevant sentences based on row_number
sentences_final <- sentences_final %>% 
  left_join(gold_labels, by = "row_number") 

sentences_final$gold_class <- ifelse(sentences_final$label == "uncertain", 1, 0)

sentences_final <- sentences_final %>% 
  select(date.x, sentence.x, label, gold_class, gold_standard, row_number)

colnames(sentences_final) <- c("date", "sentence", "gold_label", "gold_class", "gold_standard", "row_number")

View(sentences_final)

write.csv(sentences_final, "sentences_final.csv", row.names = FALSE)

### load final dataset with all sentences labeled

full_data_labeled <- read.csv("full_data_labeled.csv")

# class id's found in config.json file of language model used for labeling, 1 is uncertain and 0 certain
full_data_labeled$predicted_label <- ifelse(full_data_labeled$predicted_class == 1, "uncertain", "certain")

View(full_data_labeled)


# subset data that is gold_standard to check accuracy
gold_check2 <- full_data_labeled %>% 
  filter(gold_standard == 1)

View(gold_check2)

# check percentage of correct predictions
accuracy <- gold_check2 %>% 
  filter(gold_label == predicted_label) %>% 
  nrow() / nrow(gold_check2)

accuracy

### Create dataset with proportion of sentences as uncertain or certain
uncertain_proportion <- full_data_labeled %>%
  group_by(date) %>%
  summarize(
    total_sentences = n(),
    uncertain_sentences = sum(predicted_class == 1, na.rm = TRUE),
    proportion_uncertain = uncertain_sentences / total_sentences
  )

uncertain_proportion$date <- as.Date(uncertain_proportion$date, format = "%d.%m.%Y")

View(uncertain_proportion)

write.csv(uncertain_proportion, "uncertain_proportion.csv", row.names = FALSE)

