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
