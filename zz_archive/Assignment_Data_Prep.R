

drug_consumption <- read_csv("drug_consumption.data",
                             col_names = FALSE)

drug_consumption = data.table(drug_consumption)

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

setnames(drug_consumption,
         old = paste0("X", 1:32),
         new = c("id",
                 "age",
                 "gender",
                 "education",
                 "country",
                 "ethnicity",
                 "neuroticism",
                 "extraversion",
                 "openess",
                 "agreeableness",
                 "conscientiousness",
                 "impulsiveness",
                 "sensation",
                 usage_variables))

replace_usage = function(vector, output_type = "character"){
  
  vector = case_when(vector == "CL0" ~ "a_never",
                     vector == "CL1" ~ "b_gt_10y",
                     vector == "CL2" ~ "c_lt_10y",
                     vector == "CL3" ~ "d_lt_01y",
                     vector == "CL4" ~ "e_lt_01m",
                     vector == "CL5" ~ "f_lt_01w",
                     vector == "CL6" ~ "g_lt_01d")
  
  if (output_type == "factor"){
    vector = factor(vector)
  }
  
  return(vector)
}

drug_consumption[, (usage_variables) := lapply(.SD, 
                                               replace_usage, 
                                               output_type = "factor"), 
                 .SDcols = usage_variables]
