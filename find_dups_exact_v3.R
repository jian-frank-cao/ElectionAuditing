# Set environment -------------------------------------------------------------
library(tidyverse)
# library(furrr)
library(prodlim)
# plan(multicore, workers = 20)
# options(future.globals.maxSize= 89128960000)

# path = "/home/jccit/SoS/"
path = "/home/jccit_caltech_edu/SoS/"
# path = "F:/Caltech/SoS/"
setwd(path)

data_path = "data/"
clean_path = "data/clean/"
subset_path = "data/subset/"
result_path = "data/result/exact/"
file_code = "1146"

data = readRDS(
  file = paste0(
    path, 
    clean_path,
    file_code,
    "_data_clean.Rds"
  )
)

## Variable list --------------------------------------------------------------
design_exact = list(
  match_1 = list(
    var_list = c("LastName","FirstName","DOB",
                 "MiddleName_char1","Suffix_char"),
    var_string = c("LastName","FirstName","DOB",
                   "MiddleName_char1","Suffix_char"),
    var_numeric = NULL
  ),
  match_2 = list(
    var_list = c("LastName_char","FirstName_char","DOB",
                 "MiddleName_char1","Suffix_char"),
    var_string = c("LastName_char","FirstName_char","DOB",
                   "MiddleName_char1","Suffix_char"),
    var_numeric = NULL
  )
)

## Functions ------------------------------------------------------------------
# Function for exact matching
dedup_exact = function(data, var_list){
  # create id and unique id
  data = data %>% 
    mutate(
      ID = 1:nrow(data),
      ID_unique = 1:nrow(data)
    )
  print("IDs are created.")
  
  # find exact duplicates
  duplicated_asc = duplicated(data %>%
                                select(all_of(var_list))
  )
  print("duplicated_asc is finished.")
  
  duplicated_desc = duplicated(data %>%
                                 select(all_of(var_list)),
                               fromLast = TRUE
  )
  print("duplicated_desc is finished.")
  
  ind_dup = which(duplicated_asc | duplicated_desc)
  print("Exact dups are found.")
  
  # find representatives of dup pairs
  ind_rep = which(!duplicated_asc & duplicated_desc)
  print("Exact reps are found.")
  
  dups = NULL
  if (length(ind_dup)>0) {
    # extract dups
    dups = data[ind_dup,]
    print("Dups are extracted.")
    
    # extract reps
    reps = data[ind_rep,]
    print("Reps are extracted.")
    
    # use rep's ID_unique to represent the dup pair
    ind_ID = row.match(dups[var_list],reps[var_list])
    dups$ID_unique = reps$ID_unique[ind_ID]
    print("ID_unique are corrected.")

    # order the dups
    dups = dups %>% 
      arrange(ID_unique)
    print("Dups are rearranged.")
  }
  print("Done.")
  return(dups)
}

## Run ------------------------------------------------------------------------
for (match_n in 2:2) {
  if (match_n == 1) {
    dups = dedup_exact(
      data = data,
      var_list = design_exact[[match_n]][["var_list"]]
    )
  }else if(match_n == 2){
    dups = dedup_exact(
      data = data,
      var_list = design_exact[[match_n]][["var_list"]]
    )
  }
  
  saveRDS(list(dups = dups,
               design = design_exact[[match_n]],
               nrow = nrow(data)),
          file = paste0(path,
                        result_path,
                        file_code,
                        "_exact_match_",
                        match_n,
                        ".Rds"))
  print(
    paste0(path,
           result_path,
           file_code,
           "_exact_match_",
           match_n,
           ".Rds is saved.")
  )
}

