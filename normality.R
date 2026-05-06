library(dplyr)
library(purrr)

base <- getwd()
print(base)

path <- file.path(base, "data_brsm", "single", "phone")

files <- sort(list.files(path = path,
                         pattern = "\\.csv$",
                         full.names = TRUE))

# Load all CSV files
single_phone <- map_df(files, function(f) {
  read.csv(f)
})

# Clean column names
names(single_phone) <- make.names(names(single_phone))

# Convert numeric columns
single_phone$SuccessRate... <- as.numeric(single_phone$SuccessRate...)
single_phone$HitRate... <- as.numeric(single_phone$HitRate...)

single_phone_rt <- c()
single_phone_acc <- c()

for (file in files) {
  
  df <- read.csv(file)
  
  # special case for 6th participant game data
  if (startsWith(basename(file), "6")) {
    df <- df[df$PlayerID != "Player_2", ]
  }
  
  df <- df[df$Completed == "true", ]
  df <- df[df$InitialResponseTime.ms. > 0, ]
  
  response_times <- df$InitialResponseTime.ms.
  
  single_phone_rt <- c(single_phone_rt,
                       mean(response_times, na.rm = TRUE))
  
  df$SuccessRate... <- as.numeric(df$SuccessRate...)
  acc <- mean(df$SuccessRate..., na.rm = TRUE)
  single_phone_acc <- c(single_phone_acc, acc)
}

path <- file.path(base, "data_brsm", "multiple", "phone")

multiple_files <- sort(list.files(path = path,
                                  pattern = "\\.csv$",
                                  full.names = TRUE))
multi_phone_rt <- c()
multi_phone_acc <- c()

for (file in multiple_files) {
  
  df <- read.csv(file)
  
  df <- df[df$Completed == "true", ]
  df <- df[df$InitialResponseTime.ms. > 0, ]
  df <- df[df$Level %in% 1:10, ]
  
  response_times <- df$InitialResponseTime.ms.
  multi_phone_rt <- c(
    multi_phone_rt,
    mean(response_times, na.rm = TRUE)
  )
  
  df$SuccessRate... <- as.numeric(df$SuccessRate...)
  acc <- mean(df$SuccessRate..., na.rm = TRUE)
  multi_phone_acc <- c(multi_phone_acc, acc)
}

path <- file.path(base, "data_brsm", "single", "lab")

files <- sort(list.files(path = path,
                         pattern = "\\.csv$",
                         full.names = TRUE))

single_lab_rt <- c()
single_lab_acc <- c()
single_lab_fr <- c()

for (file in files) {
  
  df <- read.csv(file)
  
  response_times <- df$mouse.time
  response_times <- response_times[!is.na(response_times)]
  response_times <- gsub("\\[|\\]", "", response_times)
  response_times <- as.numeric(response_times)
  single_lab_rt <- c(single_lab_rt,
                     mean(response_times, na.rm = TRUE))
  
  entries <- df$mouse.clicked_name
  entries <- entries[!is.na(entries)]
  
  all_clicks <- c()
  for (entry in entries) {
    
    clean <- gsub("\\[|\\]|'", "", entry)
    clicks <- strsplit(clean, ",")[[1]]
    clicks <- trimws(clicks)
    
    all_clicks <- c(all_clicks, clicks)
  }
  
  total_clicks <- length(all_clicks)
  hits <- sum(all_clicks == "target")
  false_clicks <- total_clicks - hits
  
  accuracy <- hits / total_clicks
  false_alarm_rate <- false_clicks / total_clicks
  
  single_lab_acc <- c(single_lab_acc, accuracy)
  single_lab_fr  <- c(single_lab_fr, false_alarm_rate)
}

path1 <- file.path(getwd(), "data_brsm", "multiple", "lab")

files <- sort(list.files(path = path1,
                         pattern = "\\.csv$",
                         full.names = TRUE))

multi_lab_rt <- c()
multi_lab_acc <- c()

for (file in files) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # ----- RT calculation -----
  
  all_values <- c()
  entries <- df$mouse.time
  entries <- entries[!is.na(entries)]
  for (entry in entries) {
    
    split_lists <- strsplit(gsub("\\]\\[", "]|||[", entry), "\\|\\|\\|")[[1]]
    
    for (lst in split_lists) {
      
      clean <- gsub("\\[|\\]", "", lst)
      
      if (nchar(clean) > 0) {
        parsed <- as.numeric(strsplit(clean, ",")[[1]])
        
        if (length(parsed) > 0) {
          s <- tail(parsed, 1)
          all_values <- c(all_values, s / length(parsed))
        }
      }
    }
  }
  multi_lab_rt <- c(multi_lab_rt, mean(all_values))
  
  
  # ----- Accuracy calculation -----
  
  entries1 <- df$mouse.clicked_name
  entries1 <- entries1[!is.na(entries1)]
  
  round_acc <- c()
  
  for (entry in entries1) {
    
    clean <- gsub("\\[|\\]|'", "", entry)
    clicks <- strsplit(clean, ",")[[1]]
    clicks <- trimws(clicks)
    
    unique_targets <- length(unique(clicks))
    
    acc <- unique_targets / 5
    
    round_acc <- c(round_acc, acc)
  }
  
  multi_lab_acc <- c(multi_lab_acc, mean(round_acc))
}

shapiro.test(single_phone_rt)
shapiro.test(multi_phone_rt)
shapiro.test(single_lab_rt)
shapiro.test(multi_lab_rt)
shapiro.test(single_phone_acc)
shapiro.test(multi_phone_acc)
# shapiro.test(single_lab_acc)
shapiro.test(multi_lab_acc)

qqnorm(single_phone_rt, main = "Single Phone RT"); qqline(single_phone_rt)
qqnorm(multi_phone_rt, main = "Multi Phone RT"); qqline(multi_phone_rt)
qqnorm(single_lab_rt, main = "Single Lab RT"); qqline(single_lab_rt)
qqnorm(multi_lab_rt, main = "Multi Lab RT"); qqline(multi_lab_rt)
qqnorm(single_phone_acc, main = "Single Phone Acc"); qqline(single_phone_acc)
qqnorm(multi_phone_acc, main = "Multi Phone Acc"); qqline(multi_phone_acc)
qqnorm(single_lab_acc, main = "Single Lab Acc"); qqline(single_lab_acc)
qqnorm(multi_lab_acc, main = "Multi Lab Acc"); qqline(multi_lab_acc)