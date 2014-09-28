read_data = function(file_name) {
  data = data.table( read.csv2(file_name, header = FALSE))
}


search_last_column = function(data_table) {
  count_row = nrow(data_table)
  data_table_list = as.list.data.frame(data_table)
  
  for(i in 1:length(data_table_list)) {
    count_emptu_row = 0
    for(j in 1:count_row) {
      if(data_table_list[[i]][j] == "") {
        count_emptu_row = count_emptu_row + 1
      }
    }
    if(count_emptu_row == count_row) {
      return(i - 1)
    }
  }
}


work_with_data = function(data_table) {
  
  data_list = as.list.data.frame(data_table)    ## переводим таблицу в список, для удобства и быстроты использования
  result_data_table = data.table()
  
  names_athletes = as.vector(data_list[[1]])[which( data_list[[1]] != "")]    ## получаем не пустые имена спортсменов
  names_athletes = str_trim(names_athletes, side = "both")    ## удаляем пробелы на концах   
  names_athletes = unique(names_athletes)    ## оставляем уникальные имена
  
  for(name in names_athletes) {    ## идем по все уникальным именам спортсменов
    
    #print(paste(name, which( data_list[[1]] == name)))
    vector_number_row = which( str_trim(data_list[[1]], side = "both") == name)    ## вектор содержит номера строк, где поворяется имя "name"
    
    tmp_row = c()    ## временный вектор для строки в будущей таблицы
    tmp_row = c(tmp_row, name)
    
    for(ind_column in 3 : length(data_list) ) {    ## проходим по всем столбцам таблици
      
      tmp_value_vector = c()    ## вектор в котором содержится значения из разных строк таблицы одного столбца
      
      for(ind_number in 1 : length(vector_number_row)) {    ## проходим по всем строкам у которых одинаковый name
        ind_row = vector_number_row[ind_number]    ## номер строки в таблице
        
        tmp_value = as.numeric(as.vector(data_list[[ind_column]][ind_row]))    ##значение фиксированного столбца и строки
        tmp_value_vector = c(tmp_value_vector, tmp_value)    ## добавляем в вектор в котором будут все значения одного стобца для одного спортсмена
      } 
      
      tmp_row = c(tmp_row, mean(tmp_value_vector))    ## усредняем значения в векторе tmp_value и добавляем его в tmp_row те получаем новое значение строки
      
    }
    
    result_data_table = rbind(result_data_table, as.list(tmp_row))
    
  }
  
  return(result_data_table)
  
}

data_preparation = function() {
  
  names_file_list = list.files(path = path_input_data)    ## получаем список файлов из директории с входными данными
  
  for(name_file in names_file_list) {
    
    data = read_data(paste(path_input_data, name_file, sep = "/"))
    
    data_transpose = data.table(t(data))    ## транспонируем таблицу для удобства
    
    num_last_column = search_last_column(data_transpose)    ## посчитываем колличество значищих столбцов в талице
    
    if(!is.null(num_last_column)) {    ## если есть пустые столбцы, то обрезаем их
      data_transpose_cat = subset(data_transpose, select = names(data_transpose)[1:num_last_column])
    } else {
      data_transpose_cat = data_transpose    ## если пустых столбцов нету
    }
    
    dataset = work_with_data(data_transpose_cat)    ## объеденяем строки
    
    names(dataset) = as.vector(data$V1)[2 : length(data_transpose_cat)]    ## переименовываем столбцы 
    
    target_name = str_split(name_file, ".csv", n = Inf)[[1]][1]    ## добавляем целевой столбец, значение которого это имя файла без разширения
    
    dataset[1 : nrow(dataset), target :=  target_name]
    
    output_file_name = paste(path_output_data, paste("DataSet", name_file, sep = " "), sep = "/")    ## сохраняем получившуюся таблицу в папку выходных данных, добавляя при этом к имени файла "Dataset"
    
    write.csv2(dataset, file = output_file_name, row.names = FALSE)
    
    #   View(dataset)
    
  }
}

row_bind = function() {

  names_file_list = list.files(path = path_output_data, pattern = "^DataSet")    ## выбираем из директории только файлы начинающиеся с "Dataset"
  flag_first_file = TRUE    ## флаг показывающий что произошла загрузка первой таблицы
  
  for(name_file in names_file_list) {
    
#     print(name_file)
    tmp_dataset = data.table( read.csv2(paste(path_output_data, name_file, sep = "/")))
    
    if(flag_first_file) {
      
      flag_first_file = FALSE
      Full_dataset = data.table(tmp_dataset)
      
    } else {
      
      tmp_dataset_list = as.list.data.frame(tmp_dataset)
      Full_dataset = rbind(Full_dataset, tmp_dataset_list)    ## добавляем загруженную таблицу
      
    }
    
  }
  
#   View(Full_dataset)
  return(Full_dataset)

}
