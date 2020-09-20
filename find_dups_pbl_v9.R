# Set environment -------------------------------------------------------------
library(tidyverse)
library(fastLink)
library(prodlim)
library(furrr)
library(purrr)
plan(multicore, workers = 90)
options(future.globals.maxSize= 89128960000)

# path = "/home/jccit/SoS/"
path = "/home/jccit_caltech_edu/SoS/"
# path = "F:/Caltech/SoS/"
setwd(path)

data_path = "data/"
subset_path = "data/subset/initials/"
result_path = "data/result/prob/"
file_code = "1183"
match_n = 2

## Variable list --------------------------------------------------------------
design_prob = list(
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
# Function for prob matching
dedup_pbl = function(data,
                     batches,
                     var_list,
                     var_string,
                     n_cores = 1,
                     threshold_match = 0.95,
                     var_numeric = NULL,
                     var_address = NULL,
                     reweight_names = FALSE,
                     var_firstname = NULL,
                     dedupe_matches = FALSE,
                     return_all = FALSE,
                     em_obj = NULL){
  # match batches
  match_result = batches %>% 
    map(
      ~fastLink(
        dfA = data[.[["ind.a"]],],
        dfB = data[.[["ind.b"]],],
        varnames = var_list,
        stringdist.match = var_string,
        n.cores = n_cores,
        threshold.match = threshold_match,
        numeric.match = var_numeric,
        address.field = var_address,
        reweight.names = reweight_names,
        firstname.field = var_firstname,
        dedupe.matches = dedupe_matches,
        return.all = return_all,
        em.obj = em_obj
      )
    )
  
  
  # find dups
  data = data %>% 
    mutate(
      ID_unique = 1:nrow(data)
    )
  
  for (i in 1:length(match_result)) {
    # find ind of dups in batches
    ind_dups = match_result[[i]]$matches$inds.a < match_result[[i]]$matches$inds.b
    
    # find dup pairs in batches
    id_A = match_result[[i]]$matches$inds.a[ind_dups]
    id_B = match_result[[i]]$matches$inds.b[ind_dups]
    
    patterns = match_result[[i]]$patterns
    
    # find dup pairs in subsets
    if (i == 1) {
      dups = data.frame(ID_A = batches[[i]][["ind.a"]][id_A],
                        ID_B = batches[[i]][["ind.b"]][id_B],
                        Prob = match_result[[i]][["posterior"]][ind_dups])
      patterns = match_result[[i]]$patterns[ind_dups,]
    }else{
      dups = rbind(dups,
                   data.frame(ID_A = batches[[i]][["ind.a"]][id_A],
                              ID_B = batches[[i]][["ind.b"]][id_B],
                              Prob = match_result[[i]][["posterior"]][ind_dups]))
      patterns = rbind(
        patterns,
        match_result[[i]]$patterns[ind_dups,]
      )
    }
  }
  
  return(list(dups = dups, patterns = patterns))
}

# Function that combines the dups indices
combine_dups = function(output){
  groups = output %>% 
    names %>% 
    strsplit(., "\\.") %>% 
    sapply(., "[", 1)
  
  dups_ind = NULL
  patterns = NULL
  for (item in unique(groups)) {
    ind = which(groups == item)
    dups_ind[[paste0(item,".Rds")]] = output %>% 
      .[ind] %>% 
      lapply(., "[[", "dups") %>% 
      lapply(., "[[", "dups") %>% 
      do.call("rbind", .)
    patterns[[paste0(item,".Rds")]] = output %>% 
      .[ind] %>% 
      lapply(., "[[", "dups") %>% 
      lapply(., "[[", "patterns") %>% 
      do.call("rbind", .)
  }
  return(list(dups_ind = dups_ind, patterns = patterns))
}

# Function that extract prob dups
extract_prob_dups = function(dups_ind){
  file_list = names(dups_ind)
  dups = future_map2(
    file_list %>% as.list,
    dups_ind,
    ~ {
      file = .x
      ind = c(
        .y["ID_A"] %>% unlist,
        .y["ID_B"] %>% unlist
      ) %>% unique
      data = readRDS(
        file = paste0(
          path,
          subset_path,
          file_code,
          "/",
          file
        )
      )
      data = data %>% 
        mutate(
          ID = 1:nrow(data)
        )
      data[ind,]
    }
  ) %>% 
    set_names(file_list)
  return(dups)
}

## Run ------------------------------------------------------------------------
batches = readRDS(
  file = paste0(
    path,
    subset_path,
    file_code,
    "/batches/",
    file_code,
    "_batches.Rds"
  )
)

output = batches %>%
  set_names(names(batches)) %>% 
  future_map(
    ~{
      batch = .
      data = readRDS(
        paste0(
          path,
          subset_path,
          file_code,
          "/",
          batch$file
        )
      )%>%
        select(design_prob[[match_n]][["var_list"]])
      output = list(
        dups = dedup_pbl(data = data,
                         batches = batch[c("ind.a","ind.b")] %>% list,
                         var_list = design_prob[[match_n]][["var_list"]],
                         var_string = design_prob[[match_n]][["var_string"]],
                         var_numeric = design_prob[[match_n]][["var_numeric"]],
                         threshold_match = 0.95),
        batch = batch
      )
      
    }
  )

result = combine_dups(output)
dups = extract_prob_dups(result$dups_ind)

saveRDS(
  list(
    output = output,
    dups_ind = result$dups_ind,
    patterns = result$patterns,
    dups = dups,
    design = design_prob[[match_n]]
  ),
  file = paste0(
    path,
    result_path,
    file_code,
    "_prob_match_initials_",
    match_n,
    ".Rds"
  )
)






