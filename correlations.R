library(dplyr)
library(purrr)

#It seems to be the case that odd participants did game first, even lab first
multi_lab_first <- c(22, 24, 26, 28, 30, 32, 34, 36)
multi_phone_first <- c(23, 25, 27, 29, 31, 33, 35, 37)
single_lab_first <- c(2, 4, 6, 8, 10, 12 ,14, 16, 18, 20)
single_phone_first <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21)

base <- getwd()

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
  
  # skip participants with no valid trials
  if (length(response_times) > 0) {
    
    multi_phone_rt <- c(
      multi_phone_rt,
      mean(response_times, na.rm = TRUE)
    )
    
  }
  if (length(response_times) == 0) {
    print(paste("Dropped participant:", file))
  }
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
  
  multi_lab_rt <- c(multi_lab_rt,
                                        mean(all_values))
}

print(single_phone_rt)
print(single_phone_rt[seq(1, length(single_phone_rt), 2)])
print(multi_phone_rt)
print(single_lab_rt)
print(multi_lab_rt)

single_corr <- cor.test(
  single_lab_rt,
  single_phone_rt,
  method = "pearson"
)

multi_corr <- cor.test(
  multi_lab_rt,
  multi_phone_rt,
  method = "pearson"
)

print(single_corr)
print(multi_corr)

print("Phone RT - Single target, phone first: ")
print(mean(single_phone_rt[seq(1, length(single_phone_rt), 2)], na.rm = TRUE))

print("Phone RT - Single target, lab first: ")
print(mean(single_phone_rt[seq(2, length(single_phone_rt), 2)], na.rm = TRUE))

print("Phone RT - Multi target, phone first: ")
print(mean(multi_phone_rt[seq(2, length(multi_phone_rt), 2)], na.rm = TRUE))

print("Phone RT - Multi target, lab first: ")
print(mean(multi_phone_rt[seq(1, length(multi_phone_rt), 2)], na.rm = TRUE))

print("Lab RT - Single target, phone first: ")
print(mean(single_lab_rt[seq(1, length(single_lab_rt), 2)], na.rm = TRUE))

print("Lab RT - Single target, lab first: ")
print(mean(single_lab_rt[seq(2, length(single_lab_rt), 2)], na.rm = TRUE))

print("Lab RT - Multi target, phone first: ")
print(mean(multi_lab_rt[seq(2, length(multi_lab_rt), 2)], na.rm = TRUE))

print("Lab RT - Multi target, lab first: ")
print(mean(multi_lab_rt[seq(1, length(multi_lab_rt), 2)], na.rm = TRUE))

plot(single_lab_rt, single_phone_rt,
     pch = 19,
     col = "blue",
     xlab = "Lab Response Time (in sec)",
     ylab = "Game Response Time (in ms)",
     main = "Single Target: Lab vs Game")
abline(lm(single_phone_rt ~ single_lab_rt),
       col = "red",
       lwd = 2)

plot(multi_lab_rt, multi_phone_rt,
     pch = 19,
     col = "darkgreen",
     xlab = "Lab Response Time (in sec)",
     ylab = "Game Response Time (in ms)",
     main = "Multi Target: Lab vs Game")
abline(lm(multi_phone_rt ~ multi_lab_rt),
       col = "red",
       lwd = 2)

single_corr <- cor.test(single_lab_rt,
                        single_phone_rt,
                        method = "spearman")
print(single_corr)

multi_corr <- cor.test(multi_lab_rt,
                       multi_phone_rt,
                       method = "spearman")
print(multi_corr)

print(cor(single_lab_rt, single_phone_rt, method="spearman"))
print(cor(multi_lab_rt, multi_phone_rt, method="spearman"))