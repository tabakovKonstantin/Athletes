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
