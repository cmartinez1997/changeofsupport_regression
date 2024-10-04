# NOTE: Use this to develop simulated datasets for testing the Make_X and make_H functions

# For now, this only builds a model matrix at a single site. 
# It should be relatively easy to scale this to multiple sites.

# set.seed(2023)
# 
# # Number of trees
# n_tree <- 20 
# 
# # Number of cores per tree
# n_core <- sample(1:2, n_tree, replace = TRUE, prob = c(3, 1))
# # Make sure there is at least one tree with multiple cores for testing code
# while(sum(n_core == n_tree)) { 
#     n_core <- sample(1:2, n_tree, replace = TRUE, prob = c(3, 1))    
# }
# 
# # Assuming an equal record length for each core -- relax this later
# start_time <- rep(sample(1:20, n_tree, replace = TRUE), times = n_core)
# end_time <- rep(sample(30:40, n_tree, replace = TRUE), times = n_core)
# record_length <- end_time - start_time
# 
# # Tree ID
# tree_id <- rep(1:n_tree)