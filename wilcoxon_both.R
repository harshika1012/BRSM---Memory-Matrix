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

rq2_phone <- wilcox.test(single_phone_rt, multi_phone_rt, alternative = "two.sided", exact = FALSE)
rq2_lab <- wilcox.test(single_lab_rt, multi_lab_rt, alternative = "two.sided", exact = FALSE)

median(single_phone_rt)
median(multi_phone_rt)
median(single_lab_rt)
median(multi_lab_rt)
#Results are significant for RQ2, but in the opposite dxn (i.e, multi is faster)

rq3_single <- wilcox.test(single_phone_rt, single_lab_rt, paired = TRUE, alternative = "two.sided", exact = FALSE)
rq3_multi <- wilcox.test(multi_phone_rt, multi_lab_rt, paired = TRUE, alternative = "two.sided", exact = FALSE)
#Results are significant for RQ3. RTs are faster in lab than game.

calc_r <- function(test, n) {
  z <- qnorm(test$p.value / 2, lower.tail = FALSE)
  r <- z / sqrt(n)
  return(r)
}

print(rq2_phone)
print(rq2_lab)
print(rq3_single)
print(rq3_multi)
calc_r(rq2_phone, length(single_phone_rt) + length(multi_phone_rt))
calc_r(rq2_lab, length(single_lab_rt) + length(multi_lab_rt))
calc_r(rq3_single, length(single_phone_rt))
calc_r(rq3_multi, length(multi_phone_rt))

