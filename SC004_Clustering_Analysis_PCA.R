source("SC001_Data_Preparation.R")

#==================================================================================================================================================================
# TODO:

# do some kmeans/hierarchial clustering analysis to group together observations with a similar set of pysch_variables

psych_variables_only = as.matrix(drug_consumption[, c(psych_variables), with = F])

within_sum_of_squares = c()
between_sum_of_squares = c()
total_sum_of_squares = c()

results = list()

for (my_repeat in 1:10){
  
  test_vector = 2:18
  
  for (k in test_vector) {
    
    kmeans_output = kmeans(psych_variables_only, centers = k)
    within_sum_of_squares = c(within_sum_of_squares, kmeans_output$tot.withinss)
    between_sum_of_squares = c(between_sum_of_squares, kmeans_output$betweenss)
    total_sum_of_squares = c(total_sum_of_squares, kmeans_output$totss)
    
  }
  
  results[[my_repeat]] = data.table(my_repeat = my_repeat, k = test_vector, within = within_sum_of_squares, between = between_sum_of_squares)
  
}

results = rbindlist(results)

results = results[, j = .(within = mean(within),
                          between = mean(between)), 
                  by = .(k)]

# I think this suggests we go for 8 clusters
plot(diff(-results$within))

#==================================================================================================================================================================

clustering = kmeans(psych_variables_only, centers = 5)

drug_consumption[, psych_cluster := clustering$cluster]

table(drug_consumption$psych_cluster)

temp = drug_consumption[, c(psych_variables, "psych_cluster", "gender_recode"), with = F]

temp = melt(data = temp,
            id.vars = c("psych_cluster", "gender_recode"))

ggplot(temp, aes(x = factor(psych_cluster), y = value)) + geom_boxplot() + facet_wrap("variable", scales = "free")

ggplot(temp, aes(x = variable, y = value)) + geom_boxplot() + facet_wrap("psych_cluster", scales = "free_x")

# TODO: have a look at ggridges package for creating this visualisation
# possibly consider an animated visualisation here? Transitioning from each personality type to the next?

drug_consumption[, mean(heroin_CL3), by = psych_cluster]

#==================================================================================================================================================================

# do some kmeans/hierarchial clustering analysis to find out what drugs show a usage pattern similar to heroin. Group these drugs together as our predicted case.

usage_variables_numeric_only = as.matrix(drug_consumption[, c(usage_variables_numeric), with = F])

usage_variables_numeric_only = t(usage_variables_numeric_only)

within_sum_of_squares = c()
between_sum_of_squares = c()
total_sum_of_squares = c()

results = list()

for (my_repeat in 1:10){
  
  test_vector = 2:17
  
  for (k in test_vector) {
    
    kmeans_output = kmeans(usage_variables_numeric_only, 
                           centers = k)
    
    within_sum_of_squares = c(within_sum_of_squares, kmeans_output$tot.withinss)
    between_sum_of_squares = c(between_sum_of_squares, kmeans_output$betweenss)
    total_sum_of_squares = c(total_sum_of_squares, kmeans_output$totss)
    
  }
  
  results[[my_repeat]] = data.table(my_repeat = my_repeat, 
                                    k = test_vector, 
                                    within = within_sum_of_squares, 
                                    between = between_sum_of_squares)
  
}

results = rbindlist(results)

results = results[, j = .(within = mean(within),
                          between = mean(between)), 
                  by = .(k)]

# I think this suggests we go for 8 clusters
plot(diff(-results$within))
plot(-results$within)

clustering = kmeans(usage_variables_numeric_only, centers = 5)

sort(clustering$cluster)
