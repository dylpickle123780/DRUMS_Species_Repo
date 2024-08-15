# SIMULATING CAMERA DETECTIONS

# determines the probability of camera detection
# detection_prob
# Inputs : radius - the distance to the camera
#          psi - controls camera detection ability
# Outputs : detection probability 
detection_prob = function(radius,psi){
  return(exp(-radius^2/psi^2))
}

# simulates camera detection
# camera_sim
# Inputs : X - X-locations
#          Y - Y-locations
#          camera_x - Camera X-locations
#          camera_y - Camera Y-locations
#          start_thetas - Camera beginning view angles
#          end_thetas - Camera end view angles
#          psis - control camera detection probabilities
#          num_bins - number of bins to divide camera viewing area
#          max_dist - maximum camera viewing distance
# Outputs : final_data_matrix - matrix containing 
#           relative X location in camera
#           relative Y location in camera
#           camera number
#           timestep
#           Note: n is number cameras
camera_sim = function(X, Y, camera_x, camera_y, start_thetas, end_thetas, 
                       psis = sqrt(75), num_bins = 4, max_dist = 16){
  # define bin radius
  bin_rad = max_dist/num_bins
  
  # initialize Y_matrix
  Y_matrix = matrix(data = 0, nrow = num_bins, ncol = length(camera_x))
  true_detection = 0
  final_data_matrix = matrix(ncol = 4)[-1,]
  # iterate through each camera
  for (i in 1:length(camera_x)){
    
    # calculate distance and angle from camera
    radius = sqrt((X-camera_x[i])^2+(Y-camera_y[i])^2)
    bin = ceiling(radius/bin_rad)
    angle = atan2(Y-camera_y[i],X-camera_x[i])
    
    # determine if in range
    angle = ifelse(angle<0,angle+2*pi,angle)
    in_angle = (angle<end_thetas[i])&(angle>start_thetas[i])&(radius<max_dist)
    
    # create matrix of probabilites of detection
    detection_matrix = ifelse(in_angle,detection_prob(radius,psis[i]),0)
  
    # create matrix of detections
    # 1 if detected, 0 otherwise
    final_detect = matrix(rbinom(length(c(X)),1,prob = detection_matrix),
                          nrow = nrow(X),ncol = ncol(Y))
    
    # Assign detection per camera
    final_data_matrix = rbind(final_data_matrix,
                              cbind(X[which(final_detect==1)]-camera_x[i],
                                    abs(Y[which(final_detect==1)]-camera_y[i]),
                                    rep(i,sum(final_detect)),
                                    which(final_detect==1) %% (nrow(X)+1)))
  }
  
  # return detection matrix
  return(final_data_matrix) 
}


# NOT USED 
camera_sim = function(X, Y, camera_x, camera_y, start_thetas, end_thetas, psis = 8, max_dist = 14){
  detection_matrices = vector("list",length = length(camera_x))
  for (i in 1:length(camera_x)){
    #calculate distance and angle from camera
    radius = sqrt((X-camera_x[i])^2+(Y-camera_y[i])^2)
    angle = atan2(Y-camera_y[i],X-camera_x[i])

    #in range or not
    angle = ifelse(angle<0,angle+2*pi,angle)

    in_angle = (angle<end_thetas[i])&(angle>start_thetas[i])&(radius<max_dist)

    detection_matrix = ifelse(in_angle,detection_prob(radius,psis[i]),0)
    final_detect = matrix(rbinom(length(X),1,prob = detection_matrix),nrow = nrow(X),ncol = ncol(Y))

    #Assign per camera
    detection_matrices[[i]] = final_detect
  }

  names(detection_matrices) <- paste("Camera",1:length(camera_x))

  return(detection_matrices)
}
