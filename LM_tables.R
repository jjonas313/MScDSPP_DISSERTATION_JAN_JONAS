### Tables for langauge models

# Load the libraries
library(knitr)
library(kableExtra)

# Create a data frame with model performance metrics
model_performance <- data.frame(
  Model = c("DistilRoBERTa-base",
            "DistilBERT-base-uncased",
            "DeBERTaV3-large",
            "BERT-base-uncased",
            "DistilRoberta-financial-sentiment *",
            "FinBERT †",
            "XLM-RoBERTa-large",
            "Multinomial Naive Bayes Classifier"),
  Accuracy = c(0.790, 0.767, 0.757, 0.747, 0.756, 0.753, 0.753, confusion_test_statistics$overall[1]),
  'F1 Score' = c(0.738, 0.720, 0.704, 0.710, 0.681, 0.675, 0.694, confusion_test_statistics$byClass[7]),
  stringsAsFactors = FALSE
)

# round all values to 3 decimal places apart from the first column
model_performance[, -1] <- round(model_performance[, -1], 3)

# Generate the table with a caption and footnotes
kable(model_performance, 
      caption = '<div style="text-align:left; font-size:20px; font-weight:bold; color:#000000;">
                   Table 1: Text Classification Model Performance
                 </div>', 
      align = 'lcc', 
      booktabs = TRUE) %>% kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "bordered"),
        position = "center"
      ) %>%
  add_footnote(c(
    " <span style='font-size:small; color:grey;'>Fine-tuned version of distilroberta-base on the financial_phrasebank dataset.</span>",
    " <span style='font-size:small; color:grey;'>Pre-trained on financial communication text.</span>"
  ),
  notation = "symbol",
  escape = FALSE  # Allow HTML styling in footnotes
  )


# Parameter Tables for Classification Model

# Define the generation parameters (same as above)
generation_params <- data.frame(
  Parameter = c(
    "top_p",
    "top_k",
    "temperature",
    "repetition_penalty",
    "do_sample",
    "max_new_tokens",
    "return_full_text",
    "max_time",
    "stream",
    "details",
    "use_cache",
    "wait_for_model"
  ),
  Value = c(
    0.90,
    "None",
    0.4,
    1.0,
    "TRUE",
    512,
    "FALSE",
    "None",
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE"
  ),
  stringsAsFactors = FALSE
)



# Create and display the table using kable
generation_params %>% kable(, caption = '<div style="font-size:25px; font-weight:bold; color:#000000;">
                   Table A.1: Generation Parameters
                 </div>') %>%
  kable_styling(
    full_width = TRUE,
    bootstrap_options = c("striped", "bordered"),
  ) %>% save_kable("generation_params.png")



### training paramters

training_params <- data.frame(
  Parameter = c(
    "data_path", "model", "learning_rate", "epochs", "max_seq_length", "batch_size", 
    "warmup_ratio", "gradient_accumulation", "optimizer", "scheduler", 
    "weight_decay", "max_grad_norm", "seed", "train_split", "valid_split", 
    "text_column", "target_column", "logging_steps", "project_name", 
    "auto_find_batch_size", "mixed_precision", "save_total_limit", 
    "push_to_hub", "eval_strategy", "username", "log", 
    "early_stopping_patience", "early_stopping_threshold"
  ),
  Value = c(
    "autotrain-wwugg-4tqdk/autotrain-data", "distilbert/distilroberta-base", 
    1e-05, 3, 256, 10, 0.1, 1, "adamw_torch", "linear", 0.0, 1.0, 333, 
    "train", "validation", "autotrain_text", "autotrain_label", -1, 
    "autotrain-wwugg-4tqdk", "false", "fp16", 1, "true", "epoch", 
    "jjonas313", "tensorboard", 5, 0.01
  ),
  stringsAsFactors = FALSE
)

generation_params %>% kable(, caption = '<div style="font-size:25px; font-weight:bold; color:#000000;">
                   Table A.2: Training Parameters
                 </div>') %>%
  kable_styling(
    full_width = TRUE,
    bootstrap_options = c("striped", "bordered"),
  ) %>% save_kable("training_params.png")


## Table with models used and their links

models <- data.frame(Model = c("DistilRoBERTa-base",
                               "DistilBERT-base-uncased",
                               "DeBERTaV3-large",
                               "BERT-base-uncased",
                               "DistilRoberta-financial-sentiment",
                               "FinBERT",
                               "XLM-RoBERTa-large"))

url_model <- c("https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-distilroberta-base", 
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-distilbert-base-uncased", 
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-deberta-v3-large", 
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-bert-base-uncased", 
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-financial-news-sentiment" ,
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-finbert-tone", 
               "https://huggingface.co/jjonas313/autotrain-ecb-uncertainty-xlm-roberta-large") 


models %>%
  kbl(caption = '<div style="text-align:left; font-size:18px; font-weight:bold; color:#000000;">
  Table A.3: Models Finetuned
</div>', booktabs = T, align = "c") %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE) %>%
  column_spec(1, link = url_model) %>%
  add_footnote(c(
    " <span style='font-size:small; color:grey;'>Multinomial Niave Bayes Classifier specified in R.</span>"
  ),
  escape = FALSE  # Allow HTML styling in footnotes
  ) %>%
  save_kable("table_models.html") 