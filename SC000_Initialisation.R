load_package = function(package_names){
  
  for (package_name in package_names){
    
    if(!require(package = package_name, character.only = T)){
      
      install.packages(pkgs = package_name)
      
    }
    
    require(package_name, character.only = T)
    
  }
  
}

# this function will recode the levels of each of our variables
recode_variable = function(variable_name, variable_mapping, data_table,
                           na_label = "ERROR") {
  
  new_variable_name = paste0(variable_name, "_recode")
  
  if (new_variable_name %in% names(data_table)){
    
    data_table[, (new_variable_name) := NULL]
    
  }
  
  label_type = variable_mapping[variable == variable_name]$label_type[1]
  
  mapping = variable_mapping[variable == variable_name, .(data_label, value)]
  
  data_table = merge(data_table,
                     mapping,
                     by.x = variable_name,
                     by.y = "value",
                     all.x = T,
                     sort = F)
  
  if (label_type == "factor"){
    
    data_table = data_table[, data_label := factor(if_else(is.na(data_label),
                                                           na_label,
                                                           data_label))][]
    
  } else if (label_type == "numeric") {
    
    data_table = data_table[, data_label := as.numeric(if_else(is.na(data_label),
                                                               na_label,
                                                               data_label))][]
    
  }
  
  setnames(data_table,
           "data_label",
           new_variable_name)
  
  return(data_table)
  
}