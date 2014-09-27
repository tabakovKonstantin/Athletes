data = read_data(file_name)

data_transpose = data.table(t(data))

num_last_column = search_last_column(data_transpose)

if(!is.null(num_last_column)) { 
  data_transpose_cat = subset(data_transpose, select = names(data_transpose)[1:num_last_column])
} else {
  data_transpose_cat = data_transpose 
}

dataset = work_with_data(data_transpose_cat)

names(dataset) = as.vector(data$V1)[2 : nrow(data)] 

dataset[1 : nrow(dataset), target :=  target_name]

write.csv2(dataset, file = result_dataset_name, row.names = FALSE)
