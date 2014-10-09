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
  
  row_with_space = c()    ## номера строк которые нужно удалить так как они содержат пробелы
  
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
        
        #         if(is.na(tmp_value)) {
        #           row_with_space = c(row_with_space, ind_row)
        #         }
        
        tmp_value_vector = c(tmp_value_vector, tmp_value)    ## добавляем в вектор в котором будут все значения одного стобца для одного спортсмена
      } 
      
      tmp_value_vector = as.vector(na.omit(tmp_value_vector))    ## костыль убирающий NA
      
      if(length(tmp_value_vector) != 0) {
        
        expectation = get_expectation(tmp_value_vector)
        tmp_row = c(tmp_row, expectation)
        
        
        #         print(name)
        #         print(ind_column)
        #         print(tmp_value_vector)
        
        moment_2 = get_expectation((tmp_value_vector)^2)
        tmp_row = c(tmp_row, moment_2)
        
        
        
        moment_3 = get_expectation((tmp_value_vector)^3)
        tmp_row = c(tmp_row, moment_3)
        
        var = get_var(tmp_value_vector)
        tmp_row = c(tmp_row, var)
        
        sd = get_sd(tmp_value_vector)
        tmp_row = c(tmp_row, sd)
        
        median = median(tmp_value_vector)
        tmp_row = c(tmp_row, median)
        
        min = min(tmp_value_vector)
        tmp_row = c(tmp_row, min)
        
        max = max(tmp_value_vector)
        tmp_row = c(tmp_row, max)
        
      } else {
        
        row_with_space = c(row_with_space, ind_row)
        print(ind_row)
        print(ind_column)
        print(name)
        expectation = 0
        tmp_row = c(tmp_row, expectation)
        
        moment_2 = 0
        tmp_row = c(tmp_row, moment_2)
        
        moment_3 = 0
        tmp_row = c(tmp_row, moment_3)
        
        var = 0
        tmp_row = c(tmp_row, var)
        
        sd = 0
        tmp_row = c(tmp_row, sd)
        
        median = 0
        tmp_row = c(tmp_row, median)
        
        min = 0
        tmp_row = c(tmp_row, min)
        
        max = 0
        tmp_row = c(tmp_row, max)
      }
      #       tmp_row = c(tmp_row, mean(tmp_value_vector))    ## усредняем значения в векторе tmp_value и добавляем его в tmp_row те получаем новое значение строки
      
    }
    
    result_data_table = rbind(result_data_table, as.list(tmp_row))
    
  }
  
  print(paste("row with space ", length(row_with_space)))
  result_data_table = result_data_table[setdiff(1:nrow(result_data_table), row_with_space),]
  
  
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
    
    old_names = as.vector(data$V1)[2 : length(data_transpose_cat)]
    new_names = c()
    for(i in  1 : length(old_names)) {
      if(i == 1) {
        new_names = c(new_names, old_names[i])
      } else {
        new_names = c(new_names, paste(old_names[i], "exp", sep = "_") )
        new_names = c(new_names, paste(old_names[i], "mom2", sep = "_") )
        new_names = c(new_names, paste(old_names[i], "mom3", sep = "_"))
        new_names = c(new_names, paste(old_names[i], "var", sep = "_"))
        new_names = c(new_names, paste(old_names[i], "sd", sep = "_"))
        new_names = c(new_names, paste(old_names[i], "median", sep = "_") )
        new_names = c(new_names, paste(old_names[i], "min", sep = "_"))
        new_names = c(new_names, paste(old_names[i], "max", sep = "_"))
      }
    }
    
    #     names(dataset) = as.vector(data$V1)[2 : length(data_transpose_cat)]    ## переименовываем столбцы 
    names(dataset) = new_names    
    
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


get_probability = function(x) {
  
  freq_table = table(x)
  prop_table = freq_table
  
  if(length( unique(x) ) == 1) {
    
    for(j in 1 : length(freq_table) ) {
      prop_table[j] = 1 / length(x) 
    }
    
  } else {
    
    for(i in 1:length(x)) {
      
      position_value_in_freq_table = which((names(freq_table)) == as.character(x[i]))
      p = freq_table[[position_value_in_freq_table]][1] * (1 / length(x))
      
      prop_table[[position_value_in_freq_table]][1] = p
      
    }
  }
  
  return(prop_table)
  
}

get_expectation = function(x) {
  
  prob_table = get_probability(x)
  
  value_vec = as.numeric(names(prob_table))
  
  return(sum(prob_table * value_vec))
}

get_var = function(x) {
  
  prob_table = get_probability(x)
  expectation = get_expectation(x)
  expectation_vec = rep( expectation, length(prob_table))
  
  value_vec = as.numeric(names(prob_table))
  
  var = sum(prob_table * (value_vec - expectation_vec)^2)
  
  return(var)
  
}

get_sd = function(x) {
  
  var = get_var(x)
  
  return(sqrt(var))
}


rationing = function(data, fun) {
  
  data.list = as.data.frame.list(data)
  
  for(column in 2 : (length(data.list) - 1)) {
    
    value_column =  do.call(fun, data.list[column])
    data.list[column] = data.list[[column]] / value_column
    
  }  
  
  output_file_name = paste(path_output_data, paste("rationing_column", "_", fun, ".csv", sep = ""), sep = "/")
  write.csv2(data.list, file = output_file_name, row.names = FALSE)
  
  for(row in 1 : length(data.list[[1]])) {
    
    data.without.target.and.name = subset(data.list, select = setdiff(names(data), c("target", "Name.")))
    value.row =  do.call(fun, data.without.target.and.name[1,])
    
    for(column in 2 : (length(data.list) - 1)) {
      
      data.list[[column]][row] = data.list[[column]][row] / value.row  
      
    }  
  }
  
  output_file_name = paste(path_output_data, paste("rationing_row", "_", fun, ".csv", sep = ""), sep = "/")
  write.csv2(data.list, file = output_file_name, row.names = FALSE) 
  
}

