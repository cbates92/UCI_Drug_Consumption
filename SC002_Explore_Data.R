source("SC001_Data_Preparation.R")

ggplot(drug_consumption, aes(x = .panel_x, y = .panel_y, colour = gender_recode, fill = gender_recode)) + 
  geom_jitter(alpha = 0.4, size = 1) +
  geom_autodensity(alpha = 0.4, position = "identity") +
  geom_smooth(aes(colour = NULL, fill = NULL), formula = y ~ x, method = "loess") + 
  facet_matrix(vars(c("agreeableness_recode", "conscientiousness_recode",
                      "extraversion_recode", "neuroticism_recode",
                      "openess_recode", "impulsiveness_recode",
                      "sensation_recode")),
               layer.diag = 2, grid.y.diag = FALSE)

ggplot(drug_consumption[, .N, by = .(age_recode, education_recode)], aes(x = education_recode, y = N)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap("age_recode", scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# TODO:

# do some clustering analysis to find out what drugs show a usage pattern
# similar to heroin. Group these drugs together as our predicted case.

# we could probably use kmeans to do this clustering
