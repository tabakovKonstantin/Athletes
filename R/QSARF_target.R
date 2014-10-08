QSARF_target = function( signs ) {
  
  ## Initialization of local variables ##
  trening_data = full_data
  control_data = trening_data
  
  predicted_sign = "target"
  
  real = control_data$target
  
  max_row_with_NA = round(nrow(trening_data) / 2 )
  
  num_trees = 10
  
  
  unique_class = unique(trening_data$target)
  count_element_class = c()
  
  for(class in unique_class) {
    count_element_class = c(count_element_class, length(which(trening_data$target == class)))
  }
  
  min_size_class = rep(min(count_element_class), length(unique_class))
  
  
  if( !count_row_with_NA( signs, trening_data, max_row_with_NA ) ) {
    
    errors = Inf
    
    print("************************************")
    print( "errors:" )
    print( errors )
    
    return( errors )
    
  }
  else {
    
    ## Preparation signs ##
    formula_for_calc = formula_for_calculating(signs, predicted_sign ) 
    
    ## Model building ##
    modelRF = randomForest(formula_for_calc, trening_data, na.action = na.omit, sampsize = min_size_class, ntree=10, maxnodes = 20)
    
    ## Predicting value ##
    predictedRF = predict( modelRF, control_data )
    
    ## Calculation errors ##
    errors = error_for_classification( real, predictedRF )
    
    print("************************************")
    print( "Evaluation model" )
    print(  "Formula for calc:" )
    print(  formula_for_calc )
    print( "errors:" )
    print( errors )
    
    return ( errors )
  }
}
