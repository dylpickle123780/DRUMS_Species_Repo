circleFun <- function(center = c(0,0),diameter = 1, npoints = 100, start_theta, end_theta){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy, orig_x = center[1], orig_y = center[2], 
         angle = ifelse(atan2(yy - center[2], xx - center[1]) > 0, atan2(yy - center[2], xx - center[1]), atan2(yy - center[2], xx - center[1]) + 2 * pi),
         contained = ifelse(atan2(yy - center[2], xx - center[1]) > 0, atan2(yy - center[2], xx - center[1]), atan2(yy - center[2], xx - center[1]) + 2 * pi) > start_theta & 
           ifelse(atan2(yy - center[2], xx - center[1]) > 0, atan2(yy - center[2], xx - center[1]), atan2(yy - center[2], xx - center[1]) + 2 * pi) < end_theta))
}

graphing = function(X, Y, D, camera_x, camera_y, start_theta, end_theta, detection_matrix, view_dist){
  require(ggplot2)
  require(viridis)
  require(dplyr)
  require(stringr)
  colors_list = c("1"="red","0"="black")
  
  # Getting path data for circle and filtering out empty places
  full <- as.data.frame(matrix(nrow = 0, ncol = 0))
  for (i in 1:(length(camera_x))){
    full <- as.data.frame(rbind(full, circleFun(c(camera_x[i], camera_y[i]), view_dist*2, npoints = 8000, start_theta[i], end_theta[i])))
    full <- rbind(full, rep(NA, 6))
  }
  # full <- mutate(full, angle = atan2(full$y-rep(camera_y, length(full$y)),full$x-rep(camera_x, length(full$x))))
  # full <- filter(full, (angle > start_theta & angle < end_theta) | is.na(angle))
  full <- filter(full, contained != FALSE | is.na(contained))
  
  cameras <- as.data.frame(cbind(camera_x, camera_y))
  
  # Fixing matrices
  detects <- as.vector(detection_matrix[[1]])

  for (i in 2:length(camera_x)){
    detects <- detects + as.vector(detection_matrix[[i]])
  }
  
  final_detects <- detects > 0
  detect_cols <- ifelse(final_detects, rep("red", length(final_detects)), rep("black", length(final_detects)))
  
  #geom_path will do open circles, geom_polygon will do filled circles
  
  #Plot points coloring with detection status
  ggplot()+
    geom_point(aes(x = c(X[which(final_detects>0)]),y = c(Y[which(final_detects>0)])), size = 0.7, color = "red")+
    geom_point(data = cameras, mapping = aes(x = camera_x, y = camera_y),color = "blue")+
    geom_segment(aes(x = c(camera_x), y = c(camera_y), 
                     yend = c(camera_y + view_dist*sin(start_theta)), xend = c(camera_x + view_dist*cos(start_theta))))+
    geom_segment(aes(x = camera_x, y = camera_y,
                     yend = c(camera_y + view_dist*sin(end_theta)), xend = c(camera_x + view_dist*cos(end_theta)))) + 
    coord_cartesian(xlim = c(-55,55),
                    ylim = c(-30,30))+
    xlab("X")+
    ylab("Y")+
    # scale_color_manual(values = colors_list,name = "Detection")+
    geom_path(aes(x = full$x, y = full$y)) +
    geom_path(aes(x = move_x, y = move_y), linewidth = 0.2) +
    labs(x = "", y = "") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    
  # return(hist(full$angle))
}

