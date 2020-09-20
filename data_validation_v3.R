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
stats_path = "data/result/stats/"
file_code = "1146"


data = readRDS(
  file = paste0(
    path,
    raw_path,
    file_code,
    "_data_vrd_latin1.Rds"
  )
)

## Functions -------------------------------------------------------------------
# Function to get stats
get_stats = function(data){
  cat(paste0("Field Names: ", paste0(1:ncol(data),".",colnames(data), collapse = ", "), ".\n"))
  for (i in 1:ncol(data)) {
    cat("--------------------------------------------------------------------------\n")
    cat(paste0("Field: ",i,". ", colnames(data)[i],"\n"))
    cat(paste0("Class: ",class(data[,i]),"\n"))
    cat(paste0("Number of records: ",length(data[,i]),"\n"))
    cat(paste0("Number of missing data: ",length(which(is.na(data[,i])|grepl("^\\s*$", data[,i])|data[,i]=="NULL")),"\n"))
    cat(paste0("Missing data rate: ",round(length(which(is.na(data[,i])|grepl("^\\s*$", data[,i])|data[,i]=="NULL"))*100/length(data[,i]),4),"%\n"))
    
    if (class(data[,i])=="factor") {
      tab = data.frame(sort(table(as.character(data[,i])),decreasing = TRUE))[1:5,] %>% 
        mutate(
          percent = round(Freq*100/nrow(data),4)
        )
      cat(paste0("Top 5 common factors: \"",paste0(as.character(tab$Var1),collapse = "\", \""),"\"\n"))
      cat(paste0("          Percentage: ",paste0(tab$percent,collapse = "%, "),"%\n"))
    }else if(class(data[,i])=="character"){
      tab = data.frame(table(data[,i])) %>% 
        arrange(desc(Freq)) %>% 
        .[1:5,] %>% 
        mutate(
          percent = round(Freq*100/nrow(data),4)
        )
      cat(paste0("Top 5 common values: \"",paste0(as.character(tab$Var1),collapse = "\", \""),"\"\n"))
      cat(paste0("         Percentage: ",paste0(tab$percent,collapse = "%, "),"%\n"))
    }else if(class(data[,i])!="logical"){
      table = table(data[,i])
      if (length(table) != 1) {
        tab = data.frame(sort(table,decreasing = TRUE))[1:5,] %>% 
          mutate(
            percent = round(Freq*100/nrow(data),4)
          )
        cat(paste0("Top 5 common values: ",paste0(tab$Var1,collapse = ", "),"\n"))
        cat(paste0("         Percentage: ",paste0(tab$percent,collapse = "%, "),"%\n"))
      }else{
        tab = table %>% data.frame
        cat(paste0("Top 5 common values: ",tab$Var1,"\n"))
        cat(paste0("         Percentage: ",100.0000,"%\n"))
      }
    }
    
    if (class(data[,i])=="integer"|class(data[,i])=="numeric") {
      cat(paste0("Quantiles: \n"))
      print(quantile(data[,i], probs = seq(0, 1, 0.25), na.rm = TRUE))
    }
    
    if (class(data[,i])=="logical") {
      cat(paste0("Number of TRUEs: ", length(which(data[,i]==TRUE)),"\n"))
      cat(paste0("Percentage of TRUEs: ", round(length(which(data[,i]==TRUE))*100/length(which(!is.na(data[,i]))),2),"%\n"))
    }
    cat("--------------------------------------------------------------------------\n")
  }
}


## Run -------------------------------------------------------------------------
get_stats(data)


## Statistics ------------------------------------------------------------------
# Function to count the voters
count_voter = function(data, target){
  output = data %>% 
    select(all_of(target)) %>% 
    table %>% 
    data.frame %>% 
    `colnames<-`(c(target,"Count")) %>% 
    group_by_at(vars(all_of(target))) %>% 
    summarise(
      Count = sum(Count)
    ) %>% 
    data.frame %>% 
    arrange_at(vars(all_of(target))) %>% 
    mutate(
      Percent = Count/n_voters
    )
  return(output)
}

# Read county_code.csv
county_code = read.csv(
  file = paste0(
    path,
    data_path,
    "CA_CountyCode.csv"
  )
)

# count voters
n_voters = nrow(data)

# var list
var_list = c(
  "CountyCode", "City", "Zip", "Gender", "PartyCode",
  "Status", "RegistrationDate", "Precinct", "PrecinctNumber",
  "RegistrationMethodCode", "PrecinctId"
)


# Count voters
voters_count = var_list %>% 
  as.list %>% 
  set_names(var_list) %>% 
  future_map(
    ~ count_voter(data, .)
  )

# Preprocess
voters_count$CountyCode = voters_count$CountyCode %>% 
  mutate(
    County = county_code$County
  )

voters_count$RegistrationDate = voters_count$RegistrationDate %>% 
  mutate(
    RegistrationDate = as.Date(RegistrationDate)
  )

# Save RDS ---------------------------------------------------------------------
saveRDS(
  list(
    by_group = voters_count,
    n_voters = n_voters
  ),
  file = paste0(
    path,
    stats_path,
    file_code,
    "_stats.Rds"
  )
)








