## First stage ##
data_preparation()
full_data = row_bind()

View(full_data)

write.csv2(full_data, paste(path_output_data, "Full Dataset.csv", sep = "/"), row.names = FALSE)

## Second stage ##
full_data = data.table(read.csv2(paste(path_output_data, "Full Dataset.csv", sep = "/")))

full_data_list = as.list.data.frame(full_data)

for(i in 2 : (length(full_data_list)- 1)  ) {    ## исправить на один когда будут имена обьектов
  tmp_column = full_data_list[[i]]
  tmp_column_numeric = as.double(as.vector(tmp_column))
  full_data_list[[i]] = tmp_column_numeric
}

full_data_table = do.call(data.table, full_data_list)
full_data = full_data_table

# names_row = unique(full_data$Name.)
# names = c()
# 
# for(name_row in names_row) {
#   count_rep = length(which(full_data$Name. == name_row))
#   if(count_rep == 1) {
#     names = c(names, name_row)
#   } else {
#     for(i in 1 : count_rep) {
#       new_name_row = paste(name_row, i, sep = "_")
#       names = c(names, new_name_row)
#       print(new_name_row)
#     }
#   }
# }
# 
# row.names(full_data) = names

## Third stage ##

## AdDel ##

## Input ##
all_sign = setdiff(names(full_data), "Name.")
start_points = sample(all_sign, 10)
steps_forward = 3
steps_back = 2
nameFun = "QSARF_target"
max_step = 2
limit_step = 1
iterate_vicinity = 20

path_to_log = "E:/Work/R/Athletes/output_data/log"
name_folder  = "target_9_with_sampSize"                    

## Eval ##
ptm = proc.time()
model_predict_industry_id = algorithm_AdDel(all_sign, start_points, steps_forward, steps_back, nameFun, max_step, limit_step, iterate_vicinity) 
proc.time() - ptm


coef_size = 0.8
size_train_data = round(nrow(full_data) * coef_size)

train_data_row = sample(1 : nrow(full_data), size_train_data) 
testing_data_row = setdiff(1:nrow(full_data), train_data_row)

train_data = full_data[train_data_row,]
testing_data = full_data[testing_data_row,]

unique_class = unique(train_data$target)
count_element_class = c()

for(class in unique_class) {
  count_element_class = c(count_element_class, length(which(train_data$target == class)))
}

min_size_class = rep(min(count_element_class), length(unique_class))

modelRF = randomForest(target ~ . - Name., train_data, na.action = na.omit, sampsize = min_size_class)    ## sampsize параметр распределяющий количество элементов в классе? 

predicted_value = predict(modelRF, testing_data) 
real_value = testing_data$target

## Fourth stage ##
num_row_with_error = which(predicted_value != real_value)

count_error = length(num_row_with_error)
procent_error = (count_error * 100) / length(real_value)

table(real_value[num_row_with_error], predicted_value[num_row_with_error])    ## по горизонтали предсказанные значения 

modelRF

importance(modelRF)
sort(importance(modelRF))



x = c(1,2,3)
p = c(0.75,0.75,0.75)
x*p
sum(x*p)

freq_table = table(x)
prop_table = freq_table

if(length( unique(x) ) == 1) {
  
  for(j in 1 : length(freq_table) ) {
    prop_table[j] = 1 / length(x) 
  }
  
} else {
  
  for(i in 1:length(x)) {
    
    position_value_in_freq_table = which(as.numeric(names(freq_table)) == x[i])
    p = freq_table[[position_value_in_freq_table]][1] * (1 / length(x))
    
    prop_table[[position_value_in_freq_table]][1] = p
    
  }
}


