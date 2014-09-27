
data = read_data(file_name)

data_transpose = data.table(t(data))

num_last_column = search_last_column(data_transpose)

data_transpose_cat = subset(data_transpose, select = names(data_transpose)[1:num_last_column])


загрузка
переворот
обрезка 
добавления таргета
усреднение одинаковых
обьеденение со всеми 
