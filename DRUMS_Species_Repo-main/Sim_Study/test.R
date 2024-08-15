args = commandArgs(trailingOnly = TRUE)

suppressWarnings({
source("/Users/jaronso/Documents/CT_Density/Workflow/data-pre.R")
source("/Users/jaronso/Documents/CT_Density/Workflow/get_activity_intervals.R")
source("/Users/jaronso/Documents/CT_Density/Workflow/density_func.R")
source("/Users/jaronso/Documents/CT_Density/STE_TTE/STE_TTE_fn.R")
source("/Users/jaronso/Documents/CT_Density/REST/REST_fn.R")
source("/Users/jaronso/Documents/CT_Density/DistanceSampling/CTDS_fn.R")
})

this_result <- densityEst(data_dir = args[1], 
                                      species = "White-tailed Deer", 
                                      results_file = "sim_results.csv")
  
density_estimate_table <- this_result$density_estimate_table

save(density_estimate_table, file = paste0(args[1], "/system2_data.RData"))