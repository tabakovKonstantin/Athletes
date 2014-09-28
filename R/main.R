data = read_data(file_name)

data_transpose = data.table(t(data))

num_last_column = search_last_column(data_transpose)

if(!is.null(num_last_column)) { 
  data_transpose_cat = subset(data_transpose, select = names(data_transpose)[1:num_last_column])
} else {
  data_transpose_cat = data_transpose 
}

dataset = work_with_data(data_transpose_cat)

names(dataset) = as.vector(data$V1)[2 : length(data_transpose_cat)] 

dataset[1 : nrow(dataset), target :=  target_name]

write.csv2(dataset, file = result_dataset_name, row.names = FALSE)

View(dataset)

#########################
data_Endurance_1 = data.table( read.csv2("DataSet Endurance 1.csv"))
data_Endurance_2 = data.table( read.csv2("E:/Work/R/Athletes/output_data/DataSet Endurance 2.csv"))

data_Endurance_2_list = as.list.data.frame(data_Endurance_2)
View(rbind(data_Endurance_1, data_Endurance_2_list))

Full_dataset = data_Endurance_2

names_file_list = list.files(path = path_dataset_files, pattern = "^DataSet")

for(name_file in names_file_list) {
  print(name_file)
  tmp_dataset = data.table( read.csv2(paste(path_dataset_files, name_file, sep = "/")))
  #View(tmp_dataset)
  tmp_dataset_list = as.list.data.frame(tmp_dataset)
  Full_dataset = rbind(Full_dataset, tmp_dataset_list)
  
}
####################