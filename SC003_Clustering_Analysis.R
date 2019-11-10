source("SC001_Data_Preparation.R")

#==================================================================================================================================================================
# TODO:

# do some kmeans/hierarchial clustering analysis to group together observations with a similar set of pysch_variables

# psych_variables_only = as.matrix(drug_consumption[, c(psych_variables), with = F])
# 
# within_sum_of_squares = c()
# between_sum_of_squares = c()
# total_sum_of_squares = c()
# 
# results = list()
# 
# for (my_repeat in 1:10){
# 
#   test_vector = 2:18
# 
#   for (k in test_vector) {
# 
#     kmeans_output = kmeans(psych_variables_only, centers = k)
#     within_sum_of_squares = c(within_sum_of_squares, kmeans_output$tot.withinss)
#     between_sum_of_squares = c(between_sum_of_squares, kmeans_output$betweenss)
#     total_sum_of_squares = c(total_sum_of_squares, kmeans_output$totss)
# 
#   }
# 
#   results[[my_repeat]] = data.table(my_repeat = my_repeat, k = test_vector, within = within_sum_of_squares, between = between_sum_of_squares)
# 
# }
# 
# results = rbindlist(results)
# 
# results = results[, j = .(within = mean(within),
#                           between = mean(between)),
#                   by = .(k)]
# 
# # I think this suggests we go for 8 clusters
# plot(diff(-results$within))
# 
# #==================================================================================================================================================================
# 
# clustering = kmeans(psych_variables_only, centers = 5)
# 
# drug_consumption[, psych_cluster := clustering$cluster]
# 
# table(drug_consumption$psych_cluster)
# 
# temp = drug_consumption[, c(psych_variables, "psych_cluster", "gender_recode"), with = F]
# 
# temp = melt(data = temp,
#             id.vars = c("psych_cluster", "gender_recode"))
# 
# ggplot(temp, aes(x = factor(psych_cluster), y = value)) + geom_boxplot() + facet_wrap("variable", scales = "free")
# 
# ggplot(temp, aes(x = variable, y = value)) + geom_boxplot() + facet_wrap("psych_cluster", scales = "free_x")
# 
# # TODO: have a look at ggridges package for creating this visualisation
# # possibly consider an animated visualisation here? Transitioning from each personality type to the next?
# 
# drug_consumption[, mean(heroin_CL3), by = psych_cluster]

#==================================================================================================================================================================

# do some kmeans/hierarchial clustering analysis to find out what drugs show a usage pattern similar to heroin. Group these drugs together as our predicted case.
usage_variables_numeric_only = as.matrix(drug_consumption[, c(usage_variables_numeric), with = F])

usage_variables_numeric_only = t(usage_variables_numeric_only)

within_sum_of_squares = c()
between_sum_of_squares = c()
total_sum_of_squares = c()

results_ss = list()

cluster_dt_list = list()

test_vector = 2:15

set.seed(230)

for (k in test_vector) {
  
  kmeans_output = kmeans(usage_variables_numeric_only, 
                         centers = 4,
                         nstart = 5)
  
  within_sum_of_squares = c(within_sum_of_squares, kmeans_output$tot.withinss)
  between_sum_of_squares = c(between_sum_of_squares, kmeans_output$betweenss)
  total_sum_of_squares = c(total_sum_of_squares, kmeans_output$totss)
  

    
    if(k <= 9){
      
      data_table = data.table(drug = names(kmeans_output$cluster),
                              cluster = kmeans_output$cluster,
                              k = paste0("k0", k))
      
    } else {
      
      data_table = data.table(drug = names(kmeans_output$cluster),
                              cluster = kmeans_output$cluster,
                              k = paste0("k", k))
      
    }
    
    cluster_dt_list[[length(cluster_dt_list) + 1]] = data_table
  
}

results_ss = data.table(k = test_vector, 
                        within = within_sum_of_squares, 
                        between = between_sum_of_squares,
                        pct = between_sum_of_squares / total_sum_of_squares)

results_ss[, ':=' (within_difference = c(-diff(within),NA))]

results_ss = na.omit(results_ss)

#========================================================================================================================
#========================================================================================================================

cluster_dt = rbindlist(cluster_dt_list,
                       use.names = T)

cluster_dt = dcast(cluster_dt,
                   formula = drug ~ k,
                   fun.aggregate = sum,
                   value.var = "cluster")

# this is a very hacky way to enforce the ordering we're after in our kmeans plot
cluster_dt = cluster_dt[order(k08)]

cluster_dt = copy(cluster_dt)

for (column_name in names(cluster_dt)){
  
  if (!(column_name) == "drug"){
    
    cluster_dt[, (column_name) := !duplicated(get(column_name))]
    cluster_dt[, (column_name) := cumsum(get(column_name))]
    
  }
  
}

cluster_dt_melt = melt(cluster_dt,
                       id.vars = "drug",
                       variable.name = "k",
                       value.name = "cluster")

extra = cluster_dt_melt[!duplicated(drug)]

extra[, ':=' (k = "k01", cluster = 1)]

cluster_dt_melt = rbind(extra,
                        cluster_dt_melt)

cluster_dt_melt = merge(cluster_dt_melt,
                        cluster_dt[, .(drug, mapping = k08)],
                        by = "drug",
                        all.x = T,
                        sort = F)

cluster_dt_melt[, ':=' (drug = str_to_title(str_remove(drug,"_numeric")),
                        dummy = paste(mapping, drug, sep ="_"))]

cluster_dt_melt[, max_cluster := max(cluster), by = k]







#================================================================================================================================================
#================================================================================================================================================

# View(cluster_dt)
# 
# results_ss = rbindlist(results_ss)
# 
# results_ss = results_ss[, j = .(within = mean(within),
#                                 between = mean(between)), 
#                         by = .(k)]
# 
# # I think this suggests we go for 8 clusters
# plot(diff(-results_ss$within))
# plot(-results_ss$within)
# 
# clustering = kmeans(usage_variables_numeric_only, centers = 2)
# 
# sort(clustering$cluster)
