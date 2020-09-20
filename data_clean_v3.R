## Set environment -------------------------------------------------------------
library(tidyverse)
library(furrr)
plan(multicore, workers = 15)
options(future.globals.maxSize= 89128960000)

path = "/home/jccit_caltech_edu/SoS/"
# path = "/home/jccit/SoS/"
# path = "F:/Caltech/SoS/"
setwd(path)
data_path = "data/"
raw_path = "data/raw/"
clean_path = "data/clean/"
file_code = "1183"

data = readRDS(paste0(path,raw_path,file_code,"_data_vrd_latin1.Rds"))

## Functions -------------------------------------------------------------------
# Function to extract characters
extract_char = function(data){
  result = regmatches(data %>% unlist, 
                      gregexpr("[[:alpha:]]+",
                               data %>% unlist
                      )
  ) %>%
    future_map(
      ~paste0(.,
              collapse = ""
      )
    ) %>% 
    unlist
  return(result)
}


# Function to replace latin letters to english letters
replace_latin = function(data){
  lat_1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüýŠŽÞÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝ"
  eng_1 <- "szyaaaaaaceeeeiiiidnooooouuuuySZYAAAAAACEEEEIIIIDNOOOOOUUUUY"
  lat_2 <- c("œ", "ß", "æ", "ø", "Œ", "ß", "Æ", "Ø")
  eng_2 <- c("oe", "ss", "ae", "oe", "OE", "SS", "AE", "OE")
  result = data %>%
    unlist %>% 
    chartr(lat_1, eng_1, .)
  for (i in 1:length(lat_2)) {
    result = gsub(lat_2[i], eng_2[i], result, fixed = TRUE)
  }
  return(result)
}

# Function to replace "" with NA
replace_NA_char = function (data){
  if (class(data) == "character") {
    data = case_when(
      grepl("^\\s*$",data) ~ NA_character_,
      TRUE ~ data
    )
  }
  return(data)
}


## Clean data ------------------------------------------------------------------
data_clean = data %>% 
  mutate(
    LastName = LastName %>% replace_latin,
    FirstName = FirstName %>% replace_latin,
    MiddleName = MiddleName %>% replace_latin,
    Suffix = Suffix %>% replace_latin,
    
    LastName_char = LastName %>% extract_char %>% tolower,
    FirstName_char = FirstName %>% extract_char %>% tolower,
    MiddleName_char = MiddleName %>% extract_char %>% tolower,
    Suffix_char = Suffix %>% extract_char %>% tolower,
    
    LastName_char1 = LastName_char %>% substr(., 1, 1),
    FirstName_char1 = FirstName_char %>% substr(., 1, 1),
    MiddleName_char1 = MiddleName_char %>% substr(., 1, 1),
    Suffix_char1 = Suffix_char %>% substr(., 1, 1),
    
    DOB_year = DOB %>% substr(., 1, 4),
    DOB_month = DOB %>% substr(., 6, 7),
    DOB_day = DOB %>% substr(., 9, 10),
    
    Zip_5digit = Zip %>% substr(., 1, 5),
    Initials = paste0(FirstName_char1, LastName_char1)
  ) %>% 
  lapply(., replace_NA_char) %>% 
  as.data.frame(., stringsAsFactors = FALSE)

saveRDS(data_clean, file = paste0(path, clean_path, file_code, "_data_clean.Rds"))

