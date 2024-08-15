# COMBINING MOVEMENT AND CAMERA SIMULATIONS 

# generates true number of detected deer from an inhomogeneous simulation
# gen_new_YN
# Inputs : rho - controls dependency on previous location
#          sigma - controls step size
#          b1 - the beta 1 value
#          b0 - the beta 0 value
#          m - number of timesteps 
#          d - radius of the square area 
#          num_bins - number of bins for each camera
# Outputs : true_Y - the number of deer detected in each bin
gen_new_YN = function(rho = 0.5, sigma = 1, b1 = 0.15, b0 = 0, m = 50, d = 5000, 
                      num_bins = 4, max_dist = 16,
                      tau = 10/3, theta = 0.3, detect_psi = sqrt(75), camera_x,
                      camera_y){
  
  # run movement simulation
  result = movement_sim_clusters_arima(rho = rho, sigma = sigma,
                                     true_b1 = b1, true_b0 = b0, m = m, d = d,
                                     tau = tau, theta = theta)
  
  # generate random setup of cameras
  x = result$x
  y = result$y
  
  # generate random camera angle directions
  start_theta = runif(length(camera_y),min = 0, max = 2*pi-1)
  end_theta = start_theta+1
  
  # simulate camera detections 
  true_Y = camera_sim(camera_x = camera_x, camera_y = camera_y, X = x, Y = y, 
                  end_theta = end_theta, start_theta = start_theta, max_dist = max_dist,
                  num_bins = num_bins, psis = rep(detect_psi,length(camera_y)))
  
  # return the number of detections per camera bin
  # return(true_Y)
  
  # This line is for use with the sim study code
  return(list(true_Y = true_Y, num_deer = ncol(x)))
}


# generate detections in camera bins 
# gen_binned_data
# Inputs : final_data_matrix - data from detection simulation
#          num_bins - the number of bins per camera
#          num_cams - the number of cameras
#          view_dist - the max viewing distances of each camera
# Outputs : bin_detect - num_bins x num_cams matrix containing the number of sightings for each bin
gen_binned_data <- function(final_data_matrix, num_bins = 4, num_cams = 25, view_dist) {
  
  bin_detect <- matrix(0,nrow = num_bins, ncol = num_cams)
  bin_radius <- view_dist / num_bins
  
  distances <- sqrt(final_data_matrix[,1]^2 + final_data_matrix[,2]^2)
  final_data_matrix <- cbind(final_data_matrix, distances)
  
  if (nrow(final_data_matrix) == 0) {
    return (bin_detect)
  }
  
  for (i in 1:nrow(final_data_matrix)) {
    if((final_data_matrix[i,5] %/% bin_radius+1)>num_bins){
      bin_detect[num_bins, final_data_matrix[i,3]] <- 
        bin_detect[num_bins, final_data_matrix[i,3]] + 1
    }
    else{
      bin_detect[((final_data_matrix[i,5] %/% bin_radius)+1), final_data_matrix[i,3]] <- 
        bin_detect[((final_data_matrix[i,5] %/% bin_radius)+1), final_data_matrix[i,3]] + 1
    }
  }                             
  
  return (bin_detect)
}
