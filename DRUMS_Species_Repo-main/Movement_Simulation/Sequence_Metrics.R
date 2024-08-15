# CRITERIA FOR DEFINING A SEQUENCE
# instance of >= 1 viewings 
# if there are 4 missed viewings in a row, the sequence ends

# define and extract sequences of images
# gen_sequences
# Input : detection_matrices - list of matrices containing camera detections
# Output : 3 x num_sequences matrix containing camera number, start and end times
gen_sequences <- function(final_data_matrix) {
  
  # initialize variables 
  sequences <- matrix(, nrow = 0, ncol = 3)
  
  # simplify data
  det <- final_data_matrix[,3:4]
  #print(dim(det))
  #print(nrow(det))
  #print(det)
  det <- unique(det)
  #print(dim(det))
  #print(nrow(det))
  #print(det)
  
  in_seq <- FALSE
  cam_num <- 0
  prev_idx <- 1000000
  start_idx <- 1000000
  
  if((is.null(nrow(det))==TRUE)){
    
  }
  else{
    for (i in 1:nrow(det)) {
      
      # case 1 : in sequence 
      if (in_seq) {
        
        # extend sequence
        if (det[i,2] <= (prev_idx + 4) & det[i,2] > prev_idx & det[i,1] == cam_num) {
          prev_idx <- det[i,2]
        }
        
        else {
          sequences <- rbind(sequences, cbind(cam_num, start_idx, prev_idx))
          in_seq <- FALSE
        }
        
      }
      
      # case : not in sequence
      if (!in_seq & nrow(det) >= (i+1)) {
        if (det[i+1,2]  <= (det[i,2] + 4) & det[i+1,2] > (det[i,2]) & 
            det[i,1] == det[i+1,1]) {
          
          in_seq <- TRUE
          cam_num <- det[i,1]
          start_idx <- det[i,2]
          prev_idx <- start_idx
          
        }
      }
      
      
    }
  }
    
  
  
  # return a list of sequences
  return (sequences)
}

# generates dummy matrices to test functions 
# gen_test_matrices 
# Inputs : n - number of matrices to generate
#          row - number of rows for each matrix
#          col - number of columns for each matrix
# Outputs : detection_matrices - list of n row x col matrices
gen_test_matrices <- function(n = 10, row = 10, col = 10) {
  
  # initialize
  detection_matrices <- list()
  
  # randomly assign matrices to contain 0, 1
  for (i in sequence(1:n)) {
    detection_matrices[[i]] <- matrix(rbinom(row*col,1,.5),row,col)
  }
  
  # return list of matrices
  return (detection_matrices)
}

