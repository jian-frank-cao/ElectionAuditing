# Set environment -------------------------------------------------------------
library(tidyverse)
library(data.table)

path = "/home/jccit_caltech_edu/SoS/"
# path = "/home/jccit/SoS/"
# path = "F:/Caltech/SoS/"
setwd(path)

txt_path = "data/txt/"
raw_path = "data/raw/"
file_code = "1105"
file_vrd = '1105-70008-59-pvrdr-vrd-20200713-1130.TXT'
# error_rows = c(3848001, 12144665)  #1048
# error_rows = c(923547)             #1070
# error_rows = c(2121908)            #1077
# error_rows = c(2615862)            #1080
# error_rows = c(3506320)            #1086
# error_rows = c(292206)             #1089
# error_rows = c(13423381)           #1100
error_rows = c(8958095)            #1105


## Functions ------------------------------------------------------------------
# Function to read file
read_file = function(file_name, sep, quote, fileEncoding, error_rows){
  data = NULL
  count = 1
  data[["header-1"]] = fread(file = file_name,
                             header = FALSE,
                             skip = count - 1,
                             nrows = 1,
                             sep = sep,
                             quote = quote,
                             data.table = FALSE,
                             # fileEncoding = fileEncoding,
                             stringsAsFactors = FALSE
  )
  print("Header is read.")
  for (i in 1:length(error_rows)) {
    data[[paste0(count + 1, "-", error_rows[i] - 1)]] = fread(file = file_name,
                                                              header = FALSE,
                                                              skip = count,
                                                              nrows = error_rows[i] - 1 - count,
                                                              sep = sep,
                                                              quote = quote,
                                                              data.table = FALSE,
                                                              # fileEncoding = fileEncoding,
                                                              stringsAsFactors = FALSE
    )
    print(paste0(count + 1, "-", error_rows[i] - 1," is read."))
    count = error_rows[i] - 1
    data[[paste0("error", "-", error_rows[i])]] = fread(file = file_name,
                                                        header = FALSE,
                                                        skip = count,
                                                        nrows = 1,
                                                        sep = sep,
                                                        quote = quote,
                                                        data.table = FALSE,
                                                        # fileEncoding = fileEncoding,
                                                        stringsAsFactors = FALSE
    )
    print(paste0("error", "-", error_rows[i]," is read."))
    count = error_rows[i]
  }
  data[[paste0(count + 1, "-", "end")]] = fread(file = file_name,
                                                header = FALSE,
                                                skip = count,
                                                sep = sep,
                                                quote = quote,
                                                data.table = FALSE,
                                                # fileEncoding = fileEncoding,
                                                stringsAsFactors = FALSE
  )
  print(paste0(count + 1, "-", "end is read."))
  n = data %>% lapply(., nrow) %>% do.call("rbind",.) %>% sum
  cat(paste0("Number of lines in the file: \n"))
  system(paste0("wc -l ", file_name))
  cat("Number of lines has been read: \n")
  cat(paste0(n, "\n"))
  return(data)
}

# Function to correct the class
correct_class = function(data, target_class){
  for (i in 1:length(target_class)) {
    if (target_class[i]=="character") {
      data[,i] = as.character(data[,i])
    }else if(target_class[i]=="integer"){
      data[,i] = as.integer((data[,i]))
    }else if(target_class[i]=="factor"){
      data[,i] = as.factor((data[,i]))
    }else{
      print(paste0("Column ",i," is ",target_class[i]))
    }
  }
  return(data)
}

# Run -------------------------------------------------------------------------
data = read_file(file_name = paste0(path, txt_path, file_vrd),
                 sep = "\t",
                 quote = "",
                 fileEncoding = "latin1",
                 error_rows = error_rows
)

saveRDS(
  data,
  paste0(
    path,
    raw_path,
    file_code,
    "_raw_data_latin1.Rds"
  )
)

# =============================================
# Check the errors and make corrections by hand
# =============================================
data[[3]] = data[[3]][1,-5] %>% 
  `colnames<-`(colnames(data[[2]])) %>% 
  correct_class(.,sapply(data[[2]],class))

data[[5]] = data[[5]][1,-5] %>% 
  `colnames<-`(colnames(data[[2]])) %>% 
  correct_class(.,sapply(data[[2]],class))

# Combine data
data_vrd = data[2:length(data)] %>% 
  do.call("rbind",.)

data_vrd = data_vrd %>% 
  `colnames<-`(data[[1]][1,] %>% unlist) %>% 
  `rownames<-`(1:nrow(data_vrd))

saveRDS(
  data_vrd,
  paste0(
    path,
    raw_path,
    file_code,
    "_data_vrd_latin1.Rds"
  )
)


# error_rows = c(8958095)
# # test
# test = NULL
# test[[2]] = fread(file = paste0(path,txt_path,file_vrd),
#                   header = FALSE,
#                   # skip = 8958095,
#                   # nrows = 1,
#                   sep = "\t",
#                   quote = "",
#                   data.table = TRUE,
#                   # fileEncoding = "latin1",
#                   stringsAsFactors = FALSE
# )



