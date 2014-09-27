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


data_table = data_transpose_cat

work_with_data = function(data_table) {
  
  data_list = as.list.data.frame(data_table)    ## переводим таблицу в список, для удобства и быстроты использования
  #final_data_list = list()    ## список для преобразования в таблицу
  result_data_table = data.table()
  
  names_athletes = as.vector(data_list[[1]])[which( data_list[[1]] != "")]    ## получаем не пустые имена спортсменов
  names_athletes = str_trim(names_athletes, side = "both")    ## удаляем пробелы на концах   
  names_athletes = unique(names_athletes)    ## оставляем уникальные имена
  
  #final_data_list[[1]] = names_athletes    ## добавляем их в первую позицию листа
  
  #count = 2    ## счетчик показывает в какую позицию списка записывыть tmp_row
  
  for(name in names_athletes) {    ## идем по все уникальным именам спортсменов
    
    #print(paste(name, which( data_list[[1]] == name)))
    vector_number_row = which( data_list[[1]] == name)    ## вектор содержит номера строк, где поворяется имя "name"
    
    tmp_row = c()    ## временный вектор для строки в будущей таблицы
    tmp_row = c(tmp_row, name)
    
    for(ind_column in 3 : length(data_list) ) {    ## проходим по всем столбцам таблици
      
      tmp_value_vector = c()    ## вектор в котором содержится значения из разных строк таблицы одного столбца
      
      for(ind_number in 1 : length(vector_number_row)) {    ## проходим по всем строкам у которых одинаковый name
        ind_row = vector_number_row[ind_number]    ## номер строки в таблице
        
        tmp_value = as.numeric(as.vector(data_list[[ind_column]][ind_row]))    ##значение фиксированного столбца и строки
        tmp_value_vector = c(tmp_value_vector, tmp_value)    ## добавляем в вектор в котором будут все значения одного стобца для одного спортсмена
      } 
      
      #print(str(tmp_value_vector))
      if(is.na(tmp_value)) {
        print(paste("!!!!", paste(ind_row, ind_column)))
      }
      tmp_row = c(tmp_row, mean(tmp_value_vector))    ## усредняем значения в векторе tmp_value и добавляем его в tmp_row те получаем новое значение строки
      ##print(tmp_row)
      
    }
    result_data_table = rbind(result_data_table, as.list(tmp_row))
    #final_data_list[[count]] = tmp_row    ## записываем в лист 
    #count = count + 1
    #print(name)
    
  }
  
  #   return(do.call(data.table,final_data_list))
  return(result_data_table)
  
}
