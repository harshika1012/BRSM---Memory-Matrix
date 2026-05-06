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
}

path <- file.path(base, "data_brsm", "multiple", "phone")

multiple_files <- sort(list.files(path = path,
                                  pattern = "\\.csv$",
                                  full.names = TRUE))
multi_phone_rt <- c()

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
}

path <- file.path(base, "data_brsm", "single", "lab")

files <- sort(list.files(path = path,
                         pattern = "\\.csv$",
                         full.names = TRUE))

single_lab_rt <- c()

for (file in files) {
  
  df <- read.csv(file)
  
  response_times <- df$mouse.time
  response_times <- response_times[!is.na(response_times)]
  response_times <- gsub("\\[|\\]", "", response_times)
  response_times <- as.numeric(response_times)
  single_lab_rt <- c(single_lab_rt,
                     mean(response_times, na.rm = TRUE))
}

path1 <- file.path(getwd(), "data_brsm", "multiple", "lab")

files <- sort(list.files(path = path1,
                         pattern = "\\.csv$",
                         full.names = TRUE))

multi_lab_rt <- c()

for (file in files) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
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
  
}

set.seed(123)
observed_cor <- cor(single_phone_rt, single_lab_rt, method = "spearman")
print(observed_cor)
p_value <- 0
N <- 10000
for (i in 1:N) {
  perm_lab <- sample(single_lab_rt)
  perm_corr <- cor(single_phone_rt, perm_lab, method = "spearman")
  if (abs(perm_corr) >= abs(observed_cor)) {
    p_value <- p_value + 1
  }
}
print((p_value+1) / (N+1))
#p_value is slightly less than 0.05 (0.0448), so it's significant. 

set.seed(123)
observed_cor <- cor(multi_phone_rt, multi_lab_rt, method = "spearman")
print(observed_cor)
p_value <- 0
N <- 10000
for (i in 1:N) {
  perm_lab <- sample(multi_lab_rt)
  perm_corr <- cor(multi_phone_rt, perm_lab, method = "spearman")
  if (abs(perm_corr) >= abs(observed_cor)) {
    p_value <- p_value + 1
  }
}
print((p_value + 1) / (N + 1))
#p value is 0.9789, so it's not significant

set.seed(123)
observed_diff <- mean(single_phone_rt) - mean(single_lab_rt)
print(observed_diff)
combined <- c(single_phone_rt, single_lab_rt)
n1 <- length(single_phone_rt)
p_value <- 0
N <- 10000
for (i in 1:N) {
  perm <- sample(combined)
  group1 <- perm[1:n1]
  group2 <- perm[(n1 + 1):length(perm)]
  perm_diff <- mean(group1) - mean(group2)
  if (abs(perm_diff) >= abs(observed_diff)) {
    p_value <- p_value + 1
  }
}
print((p_value + 1) / (N + 1))

set.seed(123)
observed_diff <- mean(multi_phone_rt) - mean(multi_lab_rt)
print(observed_diff)
combined <- c(multi_phone_rt, multi_lab_rt)
n1 <- length(multi_phone_rt)
p_value <- 0
N <- 10000
for (i in 1:N) {
  perm <- sample(combined)
  group1 <- perm[1:n1]
  group2 <- perm[(n1 + 1):length(perm)]
  perm_diff <- mean(group1) - mean(group2)
  if (abs(perm_diff) >= abs(observed_diff)) {
    p_value <- p_value + 1
  }
}
print((p_value + 1) / (N + 1))
#For both cases (RT means), the p value is zero, so there is a significant difference