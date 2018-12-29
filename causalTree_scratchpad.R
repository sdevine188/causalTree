library(causalTree)
library(rsample)
library(tidyverse)


data(attrition)
glimpse(attrition)

# https://github.com/susanathey/causalTree
# https://github.com/susanathey/causalTree/blob/master/briefintro.pdf

glimpse(simulation.1)
simulation.1 %>% summary()

treatment_summary <- simulation.1 %>% group_by(treatment) %>% summarize(count = n(), mean_y = mean(y))
treatment_summary

treatment_summary %>% gather(key = stats, value = value, -treatment) %>% 
        unite(stats2, stats, treatment) %>% spread(key = stats2, value = value) %>% 
        summarize(treatment_effect = mean_y_1 - mean_y_0)


# note xval argument is how many cross-validation folds
# note from ?causalTree, that regardless of split.Honest arg, causalTree() returns splits from full data
# must run estimat.causalTree() or honest.CausalTree() to get honest estimates
# so get same results below with split.Honest = T as = F
tree <- causalTree(y ~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)
tree <- causalTree(y ~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = F, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)
tree
summary(tree)
attributes(tree)

# rpart_tree for comparison
rpart <- rpart(y ~ x1 + x2 + x3 + x4, data = simulation.1)
rpart
summary(rpart)
attributes(rpart)

# get optimal cp value from cross-validation with minimal cv errr (xerror variable)
tree$cptable
opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opcp

# prune tree
opfit <- prune(tree, opcp)
opfit

# plot
# note that top number is the treatment effect in node, and bottom number is percentage of data in node
rpart.plot(opfit)

# inspect by getting treatment effect after first split at node 3
simulation.1 %>% filter(x1 < -0.06912414) %>%  
        summarize(count = n(), mean_y = mean(y), pct_obs_in_node = n() / nrow(simulation.1))

simulation.1 %>% filter(x1 < -0.06912414) %>% group_by(treatment) %>% 
        summarize(count = n(), mean_y = mean(y)) %>% gather(key = stats, value = value, -treatment) %>% 
        unite(stats2, stats, treatment) %>% spread(key = stats2, value = value) %>% 
        summarize(treatment_effect = mean_y_1 - mean_y_0, 
                  proportion_of_obs_in_node = count_1 / sum(count_1, count_0))


############################################################################


