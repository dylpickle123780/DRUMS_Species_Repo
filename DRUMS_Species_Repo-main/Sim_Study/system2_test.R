list_x = c(-600, -300, 0, 300, 600)
camera_x = rep(list_x, 5)
camera_y = c(-600, -600, -600, -600, -600, 
             -300, -300, -300, -300, -300, 
             0, 0, 0, 0, 0, 
             300, 300, 300, 300, 300, 
             600, 600, 600, 600, 600)

library(spatstat.explore)
library(tidyverse)

point_est_matrix <- matrix(nrow = 100, ncol = 6)
conf_int_matrix <- matrix(nrow = 100, ncol = 11)
se_matrix <- matrix(nrow = 100, ncol = 6)

n.sim <- 100

# CHANGE THIS FROM DATA_LIST2, PRETTY MUCH ANYTHING (MAYBE WE COULD AGREE TO INCREASE TO DATA_LIST3...)
data_list2 <- list()
num_deer2 <- list()

for (sim in 1:n.sim){
  
  tic <- proc.time()[3]
  
  # Generate data
  test_sim_data <- gen_new_YN(rho = 0.99995, sigma = 1000/3,
                                b0 = -13.044, b1 = 0, d = 1000, m = 40000)
  
  # Saves data, true # of deer
  # CHANGE THIS TO WHAT YOU PUT BEFORE
  data_list2[[sim]] <- test_sim_data$true_Y
  num_deer2[[sim]] <- test_sim_data$num_deer
  
  # Processes data into file structure that works with competing methods
  simulation_postprocessing(test_sim_data$true_Y, camera_x = camera_x, camera_y = camera_y, index = sim)
  
  
  # Runs test.R which runs the competing methods and saves the results table
  # CHANGE THIS FIRST ARGUMENT TO LOCATION OF test.R for you
  # CHANGE THIS SECOND ARGUMENT TOO
  system2("RScript", args =c("/Users/jaronso/DRUMS_Species_Repo/test.R", 
                             paste0("/Users/jaronso/DRUMS_Species_Repo/data_directories/data", sim)))

  # Loads results table from system2 command  
  # MAKE THIS ARGUMENT MATCH LINE 51
  load(paste0("/Users/jaronso/DRUMS_Species_Repo/data_directories/data", sim, "/system2_data.RData"))
  
  point_est_matrix[sim,1] <- test_sim_data$num_deer / 4
  conf_int_matrix[sim,1]  <- test_sim_data$num_deer / 4
  se_matrix[sim,1]        <- test_sim_data$num_deer / 4
  
  point_est_matrix[sim,2:6] <- density_estimate_table[,1]
  conf_int_matrix[sim, c(2,4,6,8,10)] <- density_estimate_table[,3]
  conf_int_matrix[sim, c(3,5,7,9,11)] <- density_estimate_table[,4] 
  se_matrix[sim,2:6] <- density_estimate_table[,2]
  
  
  
  toc <- proc.time()[3]
  
  print(sim)
  print(toc-tic)
  print("This is great!")
  print("---------------------------------------")
  
}

conf_int_matrix2 <- conf_int_matrix
se_matrix2 <- se_matrix
point_est_matrix2 <- point_est_matrix

# MAKE THESE NAMES MATCH WHAT YOU PUT BEFORE
save(data_list2, file = "/Users/jaronso/Documents/Datalist/data2.RData")
save(num_deer2, file = "/Users/jaronso/Documents/Datalist/real_deer2.RData")
save(conf_int_matrix2, file = "/Users/jaronso/Documents/Datalist/ci_matrix2.RData")
save(se_matrix2, file = "/Users/jaronso/Documents/Datalist/se_matrix2.RData")
save(point_est_matrix2, file = "/Users/jaronso/Documents/Datalist/pe_matrix2.RData")

combined_data_list2 <- c(data_list2[1:25], data_list1[1:25], data_list6[1:25], data_list5[1:25])
combined_num_deer2 <- c(num_deer2[1:25], num_deer1[1:25], num_deer6[1:25], num_deer5[1:25])
combined_conf_int_matrix2 <- rbind(conf_int_matrix2[1:25,], conf_int_matrix[1:25,], conf_int_matrix6[1:25,], conf_int_matrix5[1:25,])
combined_se_matrix2 <- rbind(se_matrix2[1:25,], se_matrix[1:25,], se_matrix6[1:25,], se_matrix5[1:25,])
combined_point_est_matrix2 <- rbind(point_est_matrix2[1:25,], point_est_matrix[1:25,], point_est_matrix6[1:25,], point_est_matrix5[1:25,])

save(combined_data_list2, file = "/Users/jaronso/Downloads/combined_data_list2.RData")