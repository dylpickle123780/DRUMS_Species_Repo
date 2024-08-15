# SIMULATING MOVEMENT 

# Simulate covariate space
# covariate_generation
# Inputs : 
# Outputs : idw_data is an idw object giving a covariate value across the spatial plane
covariate_generation = function(){
  # Creates peaks for idw
  test_data <- as.data.frame(cbind(c(500, 500, -500, -500, 0),
                                   c(500, -500, 500, -500, 0), 
                                   c(-1, 1, 1, -1, 0)))
  
  # Colnames for idw
  colnames(test_data) <- c("x", "y", "z")
  
  # Ppp for idw
  test_ppp <- ppp(x = test_data$x, 
                  y = test_data$y, 
                  window = owin(xrange = c(-1000, 1000), yrange = c(-1000, 1000)),
                  marks = test_data$z)
  
  idw_data <- idw(test_ppp, power = 1.5)
  return(idw_data)
}

# Simulate movement using an inhomogeneous process
# movement_sim_clusters_arima
# Inputs : m - number of movement timesteps 
#          d - radius of the square area to model over
#          rho - controls dependency on previous location
#          sigma - controls step size 
#          true_b1 - the true beta 1 value
#          true_b0 - the true beta 0 value
#          tau - controls strength of cluster
#          theta - controls how many in a cluster
# Outputs : list[[x]] - m x n matrix of deer X-locations 
#           list[[y]] - m x n matrix of deer Y-locations 
#           Note : n is the number of deer simulated

movement_sim_clusters_arima <- function(m = 50, d = 10, rho = 0.9, sigma = 1,
                                       true_b1 = 0, true_b0 = 0, tau = 10/3, theta = 0.3, rho2 = 0.2){
  
  require(spatstat)
  require(RcppRoll)
  # generate a full path for each object
  # homes_to_path
  # Inputs : home - home location (X or Y) 
  # Outputs : the full path of a deer
  homes_to_path <- Vectorize(function(home, rho_val){
    return(sqrt((1-rho_val^2)*sigma^2)* as.vector(arima.sim(n = m,list(ar = rho_val))
    )+home)
  })
  
  homes_to_path_clusters <- function(home){
    return(sqrt((1-rho2^2)*tau^2)* as.vector(arima.sim(n = m - 1,list(ar = rho2))
    )+home)
  }
  
  # function determining lambda from true_b0, true_b1, and y
  # lambda_func
  # Inputs : x - X-location
  #          y - Y-location
  # Outputs : lambda - parameter for poisson distribution
  # lambda_func = function(x,y){
  #   estimated_covariate <- interp.im(idw_data, x, y)
  #   lambda = exp(true_b0 + true_b1*estimated_covariate)
  #   return(lambda)
  # }
  
  # generates home locations from poisson process
  home_locations <- rpoispp(exp(true_b0), win = owin(c(d[1],d[2]),c(d[3],d[4])))
  
  # assign home locations
  home_locations_y = home_locations$y
  home_locations_x = home_locations$x
  
  # n = number of simulated deer
  n = length(home_locations_x)
  
  # assign the number of deer in a cluster
  num_in_cluster = rpois(n,theta)
  
  #initialize main deer movement matrices
  movement_matrix_x <- matrix(data = NA, nrow = m, ncol = n+sum(num_in_cluster))
  movement_matrix_y <- matrix(data = NA, nrow = m, ncol = n+sum(num_in_cluster))
  
  # assign main deer movement matrices
  movement_matrix_x[1:m,1:n] = homes_to_path(home_locations_x, runif(n, 0.9999, 0.99999))
  movement_matrix_y[1:m,1:n] = homes_to_path(home_locations_y, runif(n, 0.9999, 0.99999))
  
  # assign first clustered locations
  if (sum(num_in_cluster) > 0){
    movement_matrix_x[1,(n+1):(n+sum(num_in_cluster))] = rep(movement_matrix_x[1,1:n],times = num_in_cluster)
    movement_matrix_y[1,(n+1):(n+sum(num_in_cluster))] = rep(movement_matrix_y[1,1:n],times = num_in_cluster)
  }
  
  # Assigns 2 to m positions
  current_col <- n+1
  # Checks that there are deer
  if (n > 0){
    # For each of the clusters
    for (i in 1:length(num_in_cluster)){
      # For each extra cluster deer
      if (num_in_cluster[i]>0){
        for (j in 1:num_in_cluster[i]){
          movement_matrix_x[2:m,current_col] = homes_to_path_clusters(movement_matrix_x[1:m-1, i])
          movement_matrix_y[2:m,current_col] = homes_to_path_clusters(movement_matrix_y[1:m-1, i])
          current_col <- current_col + 1
        } 
      }
    }
  }
  
  return(list(x = movement_matrix_x,y = movement_matrix_y))
  
}

