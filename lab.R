base <- getwd()
path <- file.path(base, "data_brsm", "single", "lab")
files <- sort(list.files(path = path,
                         pattern = "\\.csv$",
                         full.names = TRUE))

single_lab_rt <- c()
single_lab_acc <- c()
single_lab_fr <- c()
single_levelwise_rt <- NULL

for (file in files) {
  
  df <- read.csv(file)
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
  response_times <- df$mouse.time
  response_times <- response_times[!is.na(response_times)]
  response_times <- gsub("\\[|\\]", "", response_times)
  response_times <- as.numeric(response_times)

  single_levelwise_rt <- rbind(single_levelwise_rt, response_times)
  single_lab_rt <- c(single_lab_rt,
                          mean(response_times, na.rm = TRUE))
}

print(mean(single_lab_rt))

path1 <- file.path(getwd(), "data_brsm", "multiple", "lab")

files <- sort(list.files(path = path1,
                         pattern = "\\.csv$",
                         full.names = TRUE))

multi_lab_rt <- c()
multi_lab_acc <- c()
multi_levelwise_rt <- NULL

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
  multi_levelwise_rt <- rbind(multi_levelwise_rt, all_values)
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

hist(single_lab_rt,
     main = "Single Lab - Mean RT",
     probability = TRUE,
     ylim = c(0, 3),
     xlab = "Mean Reaction Time",
     col = "lightblue",
     border = "black")
lines(density(single_lab_rt),
      col = "blue",
      lwd = 2)

hist(multi_lab_rt,
     main = "Multiple Lab - Mean RT",
     ylim = c(0, 3),
     probability = TRUE,
     xlab = "Mean Reaction Time",
     col = "lightgreen",
     border = "black")
lines(density(multi_lab_rt),
      col = "darkgreen",
      lwd = 2)

boxplot(single_lab_rt, multi_lab_rt,
        names = c("Single Target", "Multi Target"),
        main = "Lab Reaction Time Comparison",
        ylab = "Mean Reaction Time",
        col = c("lightblue", "lightgreen"),
        border = "black")

print(sd(single_lab_rt))
print(sd(multi_lab_rt))
print(mean(multi_lab_acc))
print(sd(multi_lab_acc))
single_levelwise_rt <- colMeans(single_levelwise_rt, na.rm = TRUE)
print(single_levelwise_rt, na.rm = TRUE)
print(multi_levelwise_rt)