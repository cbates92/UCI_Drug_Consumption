source("SC000_Initialisation.R")

# this is a handy little function defined in SC000_Initialisation.R that will install and load any packages you don't already have
load_package(c("readr",
               "data.table",
               "readxl",
               "dplyr",
               "ggforce",
               "stringr"))

# we load our dataset
drug_consumption <- read_csv("drug_consumption.data",
                             col_names = FALSE)

drug_consumption = data.table(drug_consumption)

# I've stored the mapping for the 'real values' to something more meaningful in an XL spreadsheet. We load this spready.
variable_mapping <- read_excel("Variable_Mappings.xlsx",
                                sheet = "mapping")

variable_mapping = data.table(variable_mapping)

#================================================================================================================================================================================================
# lets define our varible names in some groups which will likely prove useful for reference later..

demographic_variables = c("age",
                          "gender",
                          "education",
                          "country",
                          "ethnicity")

demographic_variables_recode = paste0(demographic_variables, "_recode")

psych_variables = c("neuroticism",
                    "extraversion",
                    "openess",
                    "agreeableness",
                    "conscientiousness",
                    "impulsiveness",
                    "sensation")

psych_variables_recode = paste0(psych_variables, "_recode")

# define the variable names that relate to substance consumption
usage_variables = c("alcohol", # N
                    "amphetamines", # B
                    "amyl_nitrate",
                    "benzodiazepine", # C
                    "caffeine",
                    "cannabis", # C
                    "chocolate",
                    "cocaine", # A
                    "crack",
                    "ecstacy", # A
                    "heroin", # A
                    "ketamine", # C
                    "legal_highs",
                    "lsd",
                    "methadone",
                    "mushrooms",
                    "nicotine", # N
                    "semeron",
                    "vsa")

# we're going to create a number of different versions of our usage variables, again it'll be useful later if we can easily reference these..
usage_variables_numeric = paste(usage_variables,
                                 "numeric", 
                                sep = "_")

# we're going to create a number of different versions of our usage variables, again it'll be useful later if we can easily reference these..
usage_variables_rationalised = paste(usage_variables,
                                "rationalised", 
                                sep = "_")

# this structure is more complicated. I'm anticipating wanting to refer to different 'CL' levels for each drug collectively..
usage_variables_nested_list = list(CL0 = paste0(usage_variables, "_CL0"),
                                   CL1 = paste0(usage_variables, "_CL1"),
                                   CLX = paste0(usage_variables, "_CLX"),
                                   CL2 = paste0(usage_variables, "_CL2"),
                                   CL3 = paste0(usage_variables, "_CL3"),
                                   CL4 = paste0(usage_variables, "_CL4"),
                                   CL5 = paste0(usage_variables, "_CL5"),
                                   CL6 = paste0(usage_variables, "_CL6"))

# then if we want all of these nest variables..
usage_variables_nested_all = unname(unlist(usage_variables_nested_list))

#================================================================================================================================================================================================
# let's actually start doing useful stuff...

# rename our variables so something more useful
setnames(drug_consumption,
         old = paste0("X", 1:32),
         new = c("id",
                 demographic_variables,
                 psych_variables,
                 usage_variables))

# we use the function recode_variable (defined in script SC000_Initialisation.R) to recode each of our variables using our variable_mapping table (stored in Variable_Mappings.xlsx)
for (variable_name in unique(variable_mapping$variable)){
  
  drug_consumption = recode_variable(variable_name = variable_name,
                                     variable_mapping = variable_mapping,
                                     data_table = drug_consumption)

}

# we want to represent our substance usage variables in a number of ways as per Deliverable #1:

# (1) - remain as default multi class 
#     (we don't have to do anything, this is how data is provided - you can access these column names using 'usage_variables' vector)
#     we are likely to want to rationalise these levels later (i.e create 3 levels instead of 7), for now I've put a placeholder below (4), we can refine this later.

# (2) - transform to numeric 
#     (created in loop below - you can access these column names using 'usage_variables_numeric' vector)

# (3) - create_columns with 0/1 for each usage category whilst accounting for nesting of classes e.g. below
    # Headers      ->    [“> In last week”, “> In last month”, “> In last year”] 
    # In last day  ->     [1, 1, 1] 
    # In last week  ->    [0, 1, 1] 
    # In last month ->    [0, 0, 1] 

# (4) we retionalise our consumption variables into 3 levels:
  # CL0 & CL1 = never used before
  # CL2 & CL3 = infrequent user
  # CL4, CL5 & CL6 = frequent user

for (variable_name in usage_variables){
  
  numeric_usage_name = paste(variable_name, "numeric", sep = "_")
  rationalised_usage_name = paste(variable_name, "rationalised", sep = "_")
  
  drug_consumption = drug_consumption[, (numeric_usage_name) := case_when(get(variable_name) == "CL0" ~ 0, # never used substance
                                                                          get(variable_name) == "CL1" ~ 1, # used substance, greater than 10 years ago
                                                                          get(variable_name) == "CL2" ~ 2, # used substance, less than 10 years ago
                                                                          get(variable_name) == "CL3" ~ 3, # used substance, less than 1 year ago
                                                                          get(variable_name) == "CL4" ~ 4, # used substance, less than 1 month ago
                                                                          get(variable_name) == "CL5" ~ 5, # used substance, less than 1 week ago
                                                                          get(variable_name) == "CL6" ~ 6)][] # used substance, less than 1 day ago
  
  drug_consumption = drug_consumption[, (rationalised_usage_name) := factor(case_when(get(variable_name) == "CL0" ~ "a_never", # never used substance
                                                                               get(variable_name) == "CL1" ~ "a_never", # used substance, greater than 10 years ago
                                                                               get(variable_name) == "CL2" ~ "b_infrequent", # used substance, less than 10 years ago
                                                                               get(variable_name) == "CL3" ~ "b_infrequent", # used substance, less than 1 year ago
                                                                               get(variable_name) == "CL4" ~ "c_frequent", # used substance, less than 1 month ago
                                                                               get(variable_name) == "CL5" ~ "c_frequent", # used substance, less than 1 week ago
                                                                               get(variable_name) == "CL6" ~ "c_frequent"))][] # used substance, less than 1 day ago
  
  drug_summary = drug_consumption[, .N, by = c("id", numeric_usage_name)]
  
  # for a worked example of this logic see 'Usage_Variable_Example.xlsx'
  for (level in 0:6){
  
    
    # levels CL0, CL1 and CL2 are mutually exclusive
    if (level <= 1){
      
      drug_summary = drug_summary[, paste0(variable_name, "_CL", level) := if_else(get(numeric_usage_name) == level, 1, 0)][]
      
    # levels CL3, CL4, CL5 and CL6 are inclusive of levels with higher x value (where x = 5 for CL5 or x = 3 for CL3) 
    } else {
      
      drug_summary = drug_summary[, paste0(variable_name, "_CL", level) := if_else(get(numeric_usage_name) >= level, 1, 0)][]
      
    }
      
  }
  
  drug_summary = drug_summary[, paste0(variable_name, "_CLX") := if_else(get(numeric_usage_name) != 0, 1, 0)][]
  
  # we want to get rid of these before we merge back onto main dataset
  drug_summary[, (numeric_usage_name) := NULL]
  drug_summary[, N := NULL]
  
  drug_consumption = merge(drug_consumption,
                           drug_summary,
                           by = "id",
                           sort = F,
                           all.x = T)
  
}


