simulation_postprocessing <- function(detection_data, camera_x, camera_y, max_dist = 16, time_steps = 40000, index){
  
  sequence_data <- as.data.frame(gen_sequences(detection_data)) #%>% slice(-38)
  
  sequence_data <- sequence_data %>% mutate(sequence_id = 1:nrow(sequence_data))
  
  colnames(sequence_data) <- c("Cam_ID", "Start", "End", "sequence_id")
  
  
  final_results <- as.data.frame(matrix(ncol = 24, nrow = nrow(detection_data)))
  colnames(final_results) <- c("project_id", "deployment_id", "image_id", "filename",
                               "sequence_id", "location", "wi_taxon_id", "class",
                               "order", "family", "genus", "species", "common_name",
                               "timestamp", "bounding_boxes", "camera_name", "transect_id",
                               "frame_id", "detection_idx", "detection_confidence", "depth",
                               "world_x", "world_y", "world_z")
  
  
  final_results$world_x              <- detection_data[,1]
  final_results$world_z              <- detection_data[,2]
  final_results$deployment_id        <- detection_data[,3]
  final_results$timestamp            <- mdy_hms("01-01-2024 00:00:00") + 10 * detection_data[,4]
  final_results$project_id           <- rep(2007160, nrow(final_results))
  final_results$image_id             <- 1:nrow(detection_data)
  #final_results$filename             <- # ?
  # final_results$sequence_id          <- 1:nrow(final_results)
  #final_results$location             <- # character variable of locations?
  #final_results$wi_taxon_id          <- # ?
  final_results$class                <- "Mammalia"
  final_results$order                <- "Cetartiodactyla"
  final_results$family               <- "Cervidae"
  final_results$genus                <- "Odocoileus"
  final_results$species              <- "virginianus"
  final_results$common_name          <- "White-tailed Deer"
  #final_results$bounding_boxes       <- # ?
  final_results$camera_name          <- "Reconyx HC500 Hyperfire"
  #final_results$transect_id          <- # ?
  #final_results$frame_id             <- 1:nrow(final_results) ABANDONED
  #final_results$detection_idx        <- 0
  #final_results$detection_confidence <- # ?
  final_results$depth                <- sqrt(detection_data[,1]^2 + detection_data[,2]^2)
  final_results$world_y              <- 0
  
  final_results <- final_results %>% group_by(timestamp, deployment_id) %>% mutate(frame_id = sample.int(1000000, 1))
  
  for (i in 1:nrow(detection_data)){
    cam <- detection_data[i,3]
    time <- mdy_hms("01-01-2024 00:00:00") + 10 * detection_data[i,4]
    
    # Check if the sequence data, which already has assigned sequence ids, has
    # a row that captures this time and camera. If it does, give it that sequence id.
    # Otherwise, give it a unique one by adding i to the amount of sequences.
    if (nrow(filter(sequence_data, Cam_ID == cam & 
                                   time >= mdy_hms("01-01-2024 00:00:00") + 10 * Start & 
                                   time <= mdy_hms("01-01-2024 00:00:00") + 10 * End)) == 1){
      final_results$sequence_id[i] <- filter(sequence_data, Cam_ID == cam & 
                                               time >= mdy_hms("01-01-2024 00:00:00") + 10 * Start & 
                                               time <= mdy_hms("01-01-2024 00:00:00") + 10 * End)[[4]]
    }
    else {
      final_results$sequence_id[i] <- nrow(sequence_data) + i
    }
  }
  

  
  final_sequences <- as.data.frame(matrix(ncol = 26, nrow = nrow(sequence_data)))
  colnames(final_sequences) <- c("project_id", "deployment_id", "sequence_id", "is_blank",
                                 "identified_by", "wi_taxon_id", "class", "order", "family",
                                 "genus", "species", "common_name", "uncertainty", "start_time",
                                 "end_time", "group_size", "age", "sex", "animal_recognizable",
                                 "individual_id", "individual_animal_notes", "behavior",
                                 "highlighted", "markings", "cv_confidence", "license")
  
  
  final_sequences$project_id              <- 2007160
  final_sequences$deployment_id           <- sequence_data[,1]
  final_sequences$sequence_id             <- 1:nrow(sequence_data)
  final_sequences$is_blank                <- 0
  #final_sequences$identified_by           <-
  #final_sequences$wi_taxon_id             <-
  final_sequences$class                   <- "Mammalia"
  final_sequences$order                   <- "Cetartiodactyla"
  final_sequences$family                  <- "Cervidae"
  final_sequences$genus                   <- "Odocoileus"
  final_sequences$species                 <- "virginianus"
  final_sequences$common_name             <- "White-tailed Deer"
  #final_sequences$uncertainty             <-
  final_sequences$start_time              <- (mdy_hms("01-01-2024 00:00:00") + 10*sequence_data[,2])
  final_sequences$end_time                <- (mdy_hms("01-01-2024 00:00:00") + 10*sequence_data[,3])
  # final_sequences$group_size              <- 1 # TO DO THIS PROBABLY IS BAD
  #final_sequences$age                     <-
  #final_sequences$sex                     <-
  #final_sequences$animal_recognizable     <-
  #final_sequences$individual_id           <-
  #final_sequences$individual_animal_notes <-
  #final_sequences$behavior                <-
  #final_sequences$highlighted             <-
  #final_sequences$markings                <-
  #final_sequences$cv_confidence           <-
  #final_sequences$license                 <-
    

  final_sequences$group_size <- (final_results %>%
                                         group_by(sequence_id,timestamp) %>%
                                         summarize(count = n()) %>% 
                                         ungroup() %>% 
    group_by(sequence_id) %>% 
    summarize(group_size = max(count)) %>% as.data.frame())[1:nrow(final_sequences),2]


  
  final_deployments <- as.data.frame(matrix(ncol = 29, nrow = length(camera_x)))
  colnames(final_deployments) <- c("project_id", "deployment_id", "placename", "longitude",
                                   "latitude", "start_date", "end_date", "bait_type",
                                   "bait_description", "feature_type",
                                   "feature_type_methodology", "camera_id", "camera_name",
                                   "quiet_period", "camera_functioning", "sensor_height",
                                   "height_other", "sensor_orientation", "orientation_other",
                                   "plot_treatment", "plot_treatment_description", 
                                   "detection_distance", "subproject_name", "subproject_design",
                                   "event_name", "event_description", "event_type",
                                   "recorded_by", "remarks")
  
  final_deployments$project_id        <- 2007160
  final_deployments$deployment_id     <- 1:25
  # final_deployments$placename
  final_deployments$longitude         <- camera_x
  final_deployments$latitude          <- camera_y
  final_deployments$start_date        <- (mdy_hms("01-01-2024 00:00:00"))
  final_deployments$end_date          <- (mdy_hms("01-01-2024 00:00:00") + 10*time_steps)
  # final_deployments$bait_type
  # final_deployments$bait_description
  # final_deployments$feature_type
  # final_deployments$feature_type_methodology
  final_deployments$camera_id <- 1:25
  final_deployments$camera_name <- "Reconyx HC500 Hyperfire"
  # final_deployments$quiet_period
  # final_deployments$camera_functioning
  # final_deployments$sensor_height
  # final_deployments$height_other
  # final_deployments$sensor_orientation
  # final_deployments$orientation_other
  # final_deployments$plot_treatment
  # final_deployments$plot_treatment_description
  final_deployments$detection_distance <- max_dist
  # final_deployments$subproject_name
  # final_deployments$subproject_design
  # final_deployments$event_name
  # final_deployments$event_description
  # final_deployments$event_type
  # final_deployments$recorded_by
  # final_deployments$remarks
  
  
  path <- paste0("/Users/jaronso/DRUMS_Species_Repo/data_directories/data", index)
  dir.create(path)
  
  write_csv(final_results, paste0(path, "/sim_results.csv"))
  write_csv(final_sequences, paste0(path, "/sequences.csv"))
  write_csv(final_deployments, paste0(path, "/deployments.csv"))
  write_csv(final_results[,1:15], paste0(path, "/images_2007160.csv"))
  
}
