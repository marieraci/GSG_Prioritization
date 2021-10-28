# Budget and Feature Weights ----

# establish budgets
# various ways to set a budget
# like some percentage threshold like  below
# or using the MSC approach to determine a budget for some target
# priority areas outside of PACs
budget1 <- cellStats(cost3_agg,'sum') * 0.08
# priority areas
budget2 <- cellStats(cost3_agg, "sum") * 0.15
# restoration
budget3 <- cellStats(recov_agg, "sum") * 0.02

# Annual feature weights (fw) 
# 1. Brood 
fws1 <- c(1,1,1,1,10,10,10,1,1,1,1) 
# 2. Winter and Brood
fws2 <- c(1,1,10,10,10,10,10,1,1,10,1)
# 3. Connectivity
fws3 <- c(1,1,1,1,1,1,1,10,1,1,1)
# 4. expert 
fws4 <- c(90,58,117,44,108,56,45,198,102,78,58)

# Multisp fws
# 1. Bood
fws5 <- c(1,1,1,1,10,10,10,1,1,1,1,1,1) 
# 2. Winter and Brood
fws6 <- c(1,1,10,10,10,10,10,1,1,10,1,1,1)
# 3. Connectivity
fws7 <- c(1,1,1,1,1,1,1,10,1,1,1,1,1)
# 3. Expert
fws8 <- c(90,58,117,44,108,56,45,198,102,78,58,18,37)

# Remove Objects - Memory Clear ----

gc()
rm(ccr,brood_polygons,LC_in,nesting_polygons, winter_polygons,
   resist,windp2,DDCT_og,DDCT_ag,DDCT_burns,
   DDCT_roads,og,resp,riparian_polygons)

# Set up Problems ----

# 2 objectives: 
# 1. identify priority areas 
# suitable for disturbance limits/ conservation (public land)
# or easements (private land)
# 2. identify areas suitable for restoration

# 1. PRIORITY AREAS

# 26 problems

# all problems stored in list called q1

q1 <- list(
    p1a <- problem(cost3_agg, nesting1) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_2_agg) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p2a <- problem(cost3_agg, nesting1) %>%
      add_max_utility_objective(budget = budget2) %>%
      add_locked_out_constraints(locked_out_1_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p1b <- problem(cost3_agg, brood1) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_2_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p2b <- problem(cost3_agg, brood1) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_1_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p1c <- problem(cost3_agg, winter1) %>%
      add_max_utility_objective(budget = budget2) %>%
      add_locked_out_constraints(locked_out_2_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p2c <- problem(cost3_agg, winter1) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_1_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p1d <- problem(cost3_agg, annual1) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_2_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p2d <- problem(cost3_agg, annual1) %>%
      add_max_utility_objective(budget = budget2) %>%
      add_locked_out_constraints(locked_out_1_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p1e <- problem(cost3_agg, multisp) %>%
      add_max_utility_objective(budget = budget1) %>%
      add_locked_out_constraints(locked_out_2_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p2e <- problem(cost3_agg, multisp) %>%
      add_max_utility_objective(budget = budget2) %>%
      add_locked_out_constraints(locked_out_1_agg) %>%
      add_neighbor_constraints(2) %>%
      add_binary_decisions() %T>%
      run_calculations(),
    p1f <- p1d %>% add_feature_weights(fws1), 
    p1g <- p2d %>% add_feature_weights(fws1),
    p2f <- p1d %>% add_feature_weights(fws2),
    p2g <- p2d %>% add_feature_weights(fws2),
    p3f <- p1d %>% add_feature_weights(fws3),
    p3g <- p2d %>% add_feature_weights(fws3),
    p4f <- p1d %>% add_feature_weights(fws4),
    p4g <- p2d %>% add_feature_weights(fws4),
    p1i <- p1e %>% add_feature_weights(fws5),
    p1j <- p2e %>% add_feature_weights(fws5),
    p2i <- p1e %>% add_feature_weights(fws6),
    p2j <- p2e %>% add_feature_weights(fws6),
    p3i <- p1e %>% add_feature_weights(fws7),
    p3j <- p2e %>% add_feature_weights(fws7),
    p4i <- p1e %>% add_feature_weights(fws8),
    p4j <- p2e %>% add_feature_weights(fws8)
  )

# clean up
rm(list=ls(pattern="p+[0-9]"))

# 2. RESTORATION

# all restoration problems stored in list called q2

# create edge habitat rasters
# by reclassifying and then using the boundaries function
# break points were chosen by inspecting a histogram of each HSM
nesting_edge <- aggregate(boundaries(reclassify(nesting_ddct, matrix(c(0,0.1,NA,
                                                                       0.1,0.2,1,
                                                                       0.2,0.4,2,
                                                                       0.4,0.6,3,
                                                                       0.6,0.8,4,
                                                                       0.8,1,5)))),
                          fact = 13.41, expand = FALSE)
brood_edge <- aggregate(boundaries(reclassify(brood_ddct, matrix(c(0,0.2,NA,
                                                                   0.2,0.3,1,
                                                                   0.3,0.4,2,
                                                                   0.4,0.6,3,
                                                                   0.6,0.8,4,
                                                                   0.8,1,5)))),
                        fact = 13.41, expand = FALSE)
winter_edge <- aggregate(boundaries(reclassify(winter_ddct, matrix(c(0,0.1,NA,
                                                                     0.1,0.2,1,
                                                                     0.2,0.4,2,
                                                                     0.4,0.6,3,
                                                                     0.6,0.8,4,
                                                                     0.8,1,5)))),
                         fact = 13.41, expand = FALSE)
# use NA areas in cost feature to block out those areas
# in restoration conservation features

# create stack of restoration features 
restore <- stack(fire_agg, nesting_edge,
                 brood_edge, winter_edge,
                 abandon_lek_agg,
                 locked_out_1_agg)

# NAs need to be the same between cost and conservation features
restore <- mask(restore, recov_agg)

# restoration problem at fine scale:
# (
#   p1a <-
#     problem(
#       recov,
#       stack(
#         boundaries(reclassify(nesting_ddct, matrix(
#           c(0, 0.1, NA,
#             0.1, 0.2, 1,
#             0.2, 0.4, 2,
#             0.4, 0.6, 3,
#             0.6, 0.8, 4,
#             0.8, 1, 5)
#         ))),
#         boundaries(reclassify(brood_ddct, matrix(
#           c(0, 0.2, NA,
#             0.2, 0.3, 1,
#             0.3, 0.4, 2,
#             0.4, 0.6, 3,
#             0.6, 0.8, 4,
#             0.8, 1, 5)
#         ))),
#         boundaries(reclassify(winter_ddct, matrix(
#           c(0, 0.1, NA,
#             0.1, 0.2, 1,
#             0.2, 0.4, 2,
#             0.4, 0.6, 3,
#             0.6, 0.8, 4,
#             0.8, 1, 5)
#         ))),
#         fire,
#         abandon_lek,
#         locked_out_1)) %>%
#     add_max_utility_objective(budget = cellStats(cost3,sum)*0.02) %>%
#     add_locked_out_constraints(locked_out_7) %>%
#     add_binary_decisions() %T>%
#     run_calculations()
# )

# at coarse scale:

q2 <- list(
    p1a <- problem(recov_agg, restore) %>%
      add_max_utility_objective(budget = budget3) %>%
      add_locked_out_constraints(locked_out_7_agg) %>%
      add_feature_weights(c(10,10,10,10,10,10)) %>% 
      add_binary_decisions() %T>%
      run_calculations()
  )

# clean up
rm(list=ls(pattern="p+[0-9]"))

# Explore Data in Problems ----

abundance_data <- feature_abundances(q1[[length(q1)]])
# add km^2 column
abundance_data$absolute_abundance_km2 <-
  (abundance_data$absolute_abundance * prod(res(multisp))) %>%
  set_units(m^2) %>%
  set_units(km^2)

# calculate mean and max representations
mean(abundance_data$absolute_abundance_km2)
max(abundance_data$absolute_abundance_km2)
abundance_data$feature[which.max(abundance_data$absolute_abundance_km2)]
# plot histogram
hist(abundance_data$absolute_abundance_km2, main = "Feature abundances")


# Solve Problems - Force solutions, no presolve check ----

i <- 1
# create empty stack
s_stack1 <- stack()
# force solutions
for (i in (1:length(q1))){
  s_stack1 <- addLayer(s_stack1, solve(q1[[i]], force = TRUE)) # add solvable solutions to stack
  names(s_stack1[[i]]) <- paste("p", i, sep = "") # attach problem name to solution
  i <- i + 1
} 

i <- 1
# create empty stack
s_stack2 <- stack()
# force solutions
for (i in (1:length(q2))){
  s_stack2 <- addLayer(s_stack2, solve(q2[[i]], force = TRUE)) # add solvable solutions to stack
  names(s_stack2[[i]]) <- paste("p", i, sep = "") # attach problem name to solution
  i <- i + 1
} 

# Calculate Parameters from Solutions ----

# calculate irreplaceability
irrep1 <- stack()
irrep2 <- stack()

i <- 1
for (i in (1:nlayers(s_stack1))) {
  irrep1 <- addLayer(irrep1,
                     eval_replacement_importance(q1[[i]], s_stack1[[i]], force = TRUE))
  irrep1[[i]][irrep1[[i]]==-Inf] <- NA
  i <- i + 1
}

i <- 1
for (i in (1:nlayers(s_stack2))) {
  irrep2 <- addLayer(irrep2,
                     eval_replacement_importance(q2[[i]], s_stack2[[i]]))
  irrep2[[i]][irrep2[[i]]==-Inf] <- NA
  i <- i + 1
}

# re aggregate to fine scale
irrep1 <- disaggregate(irrep1, fact = 13.41, expand = FALSE)
# match extents
irrep1 <- projectRaster(irrep1,bounds,crs = CRS_obj)

# re aggregate to fine scale
irrep2 <- disaggregate(irrep2, fact = 13.41, expand = FALSE)
# match extents
irrep2 <- projectRaster(irrep2,bounds,crs = CRS_obj)

# Mask out land ownership ----
# 1. mask out private lands
# 2. mask out public lands

irrep_mask <- stack(irrep1, irrep1)
irrep_mask2 <- stack(irrep2, irrep2)

i <- 1
k <- 1
while (i %in% 1:nlayers(irrep1)){
  irrep_mask[[k]] <- mask(irrep1[[i]],pub, maskvalue = 1,
                          updatevalue=NA)
  irrep_mask[[k+1]] <- mask(irrep1[[i]],pri, maskvalue = 1,
                            updatevalue=NA)
  i <- i + 2
  k <- k + 2
}

i <- 1
for (i in (1:nlayers(irrep2))){
  irrep_mask2[[i]] <- mask(irrep2, pub, maskvalue = 1,
                           updatevalue=NA)
  irrep_mask2[[i+1]] <- mask(irrep2, pri, maskvalue = 1,
                           updatevalue=NA)
  i <- i + 1
}
