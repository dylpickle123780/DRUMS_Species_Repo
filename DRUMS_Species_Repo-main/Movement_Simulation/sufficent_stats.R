#This function provides the summary statistics for the cluster size
#Inputs:
#final_data_matrix - matrix that holds the X,Y,Cam #, M
#Outputs (as a vector):
# avg_n_deer - average number of deer per timestamp at each camera
# prop_multi_deer_image - number images with multiple deer / total number of images
cluster_stat = function(final_data_matrix){
  #gives the number of each camera that has collected images
  cams = unique(as.vector(final_data_matrix[,3]))
  #print(final_data_matrix)
  #print(cams)
  
  #initializes list
  deer_per_image = c()
  
  for(i in 1:(length(cams))){

    #counts_per_m holds the number of deer per image for each camera
    counts_per_m = as.vector(table(final_data_matrix[(which(final_data_matrix[,3]==cams[i])),4]))
    #print(final_data_matrix[(which(final_data_matrix[,3]==cams[i])),4])
    #print(table(final_data_matrix[(which(final_data_matrix[,3]==cams[i])),4]))
    #print(counts_per_m)
    #deer_per_image accumulates the counts of deer per image for all images
    deer_per_image = c(deer_per_image, counts_per_m)
  }
  #print(deer_per_image)
  #averages the counts of deer for each image
  avg_n_deer = sum(deer_per_image) / length(deer_per_image)
  
  
  n_multi_deer_image = length(which(deer_per_image!=1)) #number of images with 1 deer
  #below gives the number of images with more than one deer / total number of images
  prop_multi_deer_image = ((n_multi_deer_image)/(length(deer_per_image)))
  
  
  return_vector = c(avg_n_deer, prop_multi_deer_image)
  return(return_vector)
}

#This function gives the sufficent statistic for the sigma. It gives the sum of
#the obs x-coordinates of the deer squared at each time stamp in a sequence.
#This process is repeated for the y_coordinates. etc.(sum(x_i^2)) and sum(y_i^2).
#It allows gives the sum of the previous step times the next step for both x and y
#coordinates (ex) (sum((x_i)(x_i+1)) and sum((y_i)(y_i+1))
#This is only for deer observed in sequences
#INPUTS:
#final_detection_matrix - matrix that holds the X,Y,Cam #, M
#seq_matrix - matrix that holds camera, start M, end M
#camera_x - vector of camera x-locations
#camera_y - vector of camera y -locations
#OUTPUTS (as a vector):
#stat_x1 - summary statistic for the x coordinates (sum(x_i^2))
#stat_y1 -summary statistc for the y coordinates (sum(x_i^2))
#stat_x2 -sum stat for the x coordinates sum((x_i)(x_i+1)
#stat_y2 -sum stat for the y coordinates sum((y_i)(y_i+1))

sigma_sum = function(final_detection_matrix, seq_matrix,camera_x, camera_y){
  #initializes values
  stat_x1 = 0
  stat_y1 =0
  stat_x2 =0
  stat_y2=0
  
  for(i in 1:nrow(seq_matrix)){
    
    #pulls out data entries from final_dection_matrix that are in the sequence
    in_seq = subset(final_detection_matrix,
                    (final_detection_matrix[,3]==seq_matrix[i,1])&
                      (final_detection_matrix[,4]>=seq_matrix[i,2])&
                      (final_detection_matrix[,4]<=seq_matrix[i,3]))
    
    #adjusts for the camera locations
    #in_seq[,1]=in_seq[,1]+camera_x[seq_matrix[i,1]]
    #in_seq[,2]=in_seq[,2]+camera_y[seq_matrix[i,1]]
    
    #squares values and adds them to the running total of the sums of x^2 and y^2
    squared = in_seq^2
    stat_x1 = stat_x1 + sum(squared[,1])
    stat_y1 = stat_y1 + sum(squared[,2])
    
    
    #averages the x,y coordinates for images with multiple deer and sorts the matrix
    #by time stamp (m)
    m = sort(unique(in_seq[,4]))
    if(length(m)==nrow(in_seq)){#if not duplicate deer skip for loop to save time :)
      temp_matrix1 = in_seq[order(in_seq[,4]),]
      temp_matrix1 = temp_matrix1[,-3]
    }
    else{
      temp_matrix1 = matrix(data=NA, ncol=3, nrow=length(m))
      temp_matrix1[,3] = m
      for(i in 1:length(m)){
        temp_matrix1[i,1]=(mean(in_seq[which(in_seq[,4]==m[i]),1]))
        temp_matrix1[i,2]=(mean(in_seq[which(in_seq[,4]==m[i]),2]))
      }
      
    }
    #creates a temporary matrix where the data is shifted down one row
    temp_matrix2 = rbind(temp_matrix1, c(0,0,0))
    temp_matrix2 = temp_matrix2[-1,]
    
    #finds the the sum of x_i*x_i+1 and sum of y_i*y_i+1 and adds it to the 
    #running sum
    mult_matrix = temp_matrix1*temp_matrix2
    stat_x2 = stat_x2 + sum(mult_matrix[,1])
    stat_y2 =stat_y2 + sum(mult_matrix[,2])
  }
  
  return_vector = c(stat_x1, stat_y1, stat_x2, stat_y2)
  #print(return_vector)
  return(return_vector)
}


#This function gives the summary statistics for psi
#Inputs:
#bin_detect - num_bins x num_cams matrix containing the number of sightings for each bin
#Num_bins = number of bins 
#Outputs: a vector of the average number of detections per camera for each bin
bin_stat = function(bin_detect){
  return_vector = c()
  for(i in 1:nrow(bin_detect)){
    return_vector = c(return_vector, (sum(bin_detect[i,])/(ncol(bin_detect))))
  }
  return(return_vector)
}



