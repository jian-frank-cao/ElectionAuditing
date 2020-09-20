## Environment -----------------------------------------------------------------
library(tidyverse)
library(furrr)
library(assertthat)
plan(multicore, workers = 30)
options(future.globals.maxSize= 89128960000)

path = "/home/jccit_caltech_edu/SoS/"
# path = "F:/Caltech/SoS/"
clean_path = "data/clean/"
matches_path = "data/result/matches/"
setwd(path)

file_code_list = c(
  "1146",
  "1154"
)

## Load the cleaned data -------------------------------------------------------
data = file_code_list %>% 
  as.list %>% 
  set_names(file_code_list) %>% 
  future_map(
    ~ readRDS(
      file = paste0(
        path,
        clean_path,
        .,
        "_data_clean.Rds"
      )
    )
  )

names(data) = data %>% 
  lapply(., "[[", "ExtractDate") %>% 
  sapply(., "[", 1)

## Functions -------------------------------------------------------------------
# Function that does exact matching
exact_match = function(data,
                       var_exclude = "ExtractDate",
                       var_id = "RegistrantID"){
  var_list = setdiff(colnames(data[[1]]), var_exclude)
  out = NULL
  for (i in 1:(length(data)-1)) {
    out[[names(data)[i:(i+1)] %>% 
             paste0(., collapse = " | ")]] = list(
               exact_match = inner_join(
                 data[[i]],
                 data[[i + 1]],
                 by = var_list
               ) %>% 
                 .[var_id],
               mismatch_A = anti_join(
                 data[[i]],
                 data[[i + 1]],
                 by = var_list
               ),
               mismatch_B = anti_join(
                 data[[i + 1]],
                 data[[i]],
                 by = var_list
               )
             )
    print(
      paste0(
        "Matching of ",
        names(out)[i],
        " is finished."
      )
    )
  }
  return(out)
}

# Function that matches IDs
id_match = function(data,
                    dfA = "mismatch_A",
                    dfB = "mismatch_B",
                    var_id = "RegistrantID"){
  out = data %>% 
    future_map(
      ~ {
        df = .
        matched_id = inner_join(
          df[[dfA]],
          df[[dfB]],
          by = var_id
        ) %>% 
          .[var_id] %>% 
          unlist
        df[["id_match_A"]] = df[[dfA]][match(matched_id, df[[dfA]][[var_id]]), ]
        df[["id_match_B"]] = df[[dfB]][match(matched_id, df[[dfB]][[var_id]]), ]
        df[["only_A"]] = df[[dfA]][!df[[dfA]][[var_id]] %in% matched_id, ]
        df[["only_B"]] = df[[dfB]][!df[[dfB]][[var_id]] %in% matched_id, ]
        df[[dfA]] = NULL
        df[[dfB]] = NULL
        df
      }
    )
  return(out)
}

# Function that assert the number of records
assert_matches = function(matches, data){
  for (i in 1:length(matches)) {
    assert_that(
      nrow(matches[[i]][["exact_match"]]) + 
        nrow(matches[[i]][["id_match_A"]]) +
        nrow(matches[[i]][["only_A"]]) == 
        nrow(data[[i]])
    )
    assert_that(
      nrow(matches[[i]][["exact_match"]]) + 
        nrow(matches[[i]][["id_match_B"]]) +
        nrow(matches[[i]][["only_B"]]) == 
        nrow(data[[i + 1]])
    )
  }
}


## Run ------------------------------------------------------------------------
matches = exact_match(data)
matches = id_match(matches)
assert_matches(matches, data)

## Save match into Rds files --------------------------------------------------
response = future_map2(
  matches,
  names(matches) %>% 
    as.list,
  ~ {
    data = .x
    name = .y
    saveRDS(
      data,
      file = paste0(
        path,
        matches_path,
        substr(name, 1, 10),
        "_",
        substr(name, 14, 23),
        "_matches.Rds"
      )
    )
  }
)








