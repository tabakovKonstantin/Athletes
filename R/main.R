# data = read_data(file_name)
# 
# data_transpose = data.table(t(data))
# 
# num_last_column = search_last_column(data_transpose)
# 
# if(!is.null(num_last_column)) { 
#   data_transpose_cat = subset(data_transpose, select = names(data_transpose)[1:num_last_column])
# } else {
#   data_transpose_cat = data_transpose 
# }
# 
# dataset = work_with_data(data_transpose_cat)
# 
# names(dataset) = as.vector(data$V1)[2 : length(data_transpose_cat)] 
# 
# dataset[1 : nrow(dataset), target :=  target_name]
# 
# write.csv2(dataset, file = result_dataset_name, row.names = FALSE)
# 
# View(dataset)

data_preparation()
full_data = row_bind()
View(full_data)
write.csv2(full_data, paste(path_output_data, "Full Dataset.csv", sep = "/"), row.names = FALSE)

#########################
full_data = data.table(read.csv2(paste(path_output_data, "Full Dataset.csv", sep = "/")))

full_data_list = as.list.data.frame(full_data)

for(i in 2 : (length(full_data_list)- 1)  ) {    ## исправить на один когда будут имена обьектов
  tmp_column = full_data_list[[i]]
  tmp_column_numeric = as.double(as.vector(tmp_column))
  full_data_list[[i]] = tmp_column_numeric
}

full_data_table = do.call(data.table, full_data_list)

#######################################
size_train_data = round(nrow(full_data) * 0.4)

train_data_row = sample(1 : nrow(full_data), size_train_data) 
testing_data_row = setdiff(1:nrow(full_data), train_data_row)

train_data = full_data[train_data_row,]
testing_data = full_data[testing_data_row,]

modelRF = randomForest(target ~ . - Name., full_data_table, na.action = na.omit)


# full_data_list = as.list.data.frame(full_data)
# 
# for(i in 2 : (length(full_data_list)- 1)  ) {    ## исправить на один когда будут имена обьектов
#   tmp_column = full_data_list[[i]]
#   tmp_column_numeric = as.double(as.vector(tmp_column))
#   full_data_list[[i]] = tmp_column_numeric
# }
# 
# full_data_table = do.call(data.table, full_data_list)



  
  
