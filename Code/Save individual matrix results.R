library(googlesheets4)
library(googledrive)
library(tidyverse)

files = drive_ls(path = "My projects/Forecasting Analysis/Abstract screening and matrix analysis/Individual matrix analysis/")
for(i in 1:nrow(files)){
  matrix <- data.frame(t(read_sheet(files$id[i], col_types = "c")))
  colnames(matrix)<-matrix[1,]
  matrix <- matrix[2:nrow(matrix),]
  write.csv(matrix, paste0("../Data/Finished matrices/",files$name[i], ".csv"))
}
files = files%>% #In case it times out, get the last few
  map_df(rev)
for(i in 1:nrow(files)){
  matrix <- data.frame(t(read_sheet(files$id[i], col_types = "c")))
  colnames(matrix)<-matrix[1,]
  matrix <- matrix[2:nrow(matrix),]
  write.csv(matrix, paste0("../Data/Finished matrices/",files$name[i], ".csv"))
}

