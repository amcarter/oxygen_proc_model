source("~/../git/oxygen_proc_model/src/batch_nhd_retrevial.R")
source("~/../git/oxygen_proc_model/src/oxygen_dynamics_functions.R")

# modeled oxygen dynamics in streams:

# mass balance equation:
# dVC/dt = -(dq_x/dx + dq_y/dy + dq_z/dz) + S
#   V = volume
#   C = concentration
#   q_xyz = mass fluxes due to advective and dispersive transport
#   S = sinks and sources


# Assumptions:
# 1. uniform flow/well mixed:
#   dq_x/dx + dq_y/dy = 0



# Physical processes:####
# Temperature ####
# gas exchange ####
# lope <- function(lat, long){
point <- sf::st_sfc(sf::st_point(c(long, lat)), crs = 4269)
comid <- nhdplusTools::discover_nhdplus_id(point)
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = comid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)


r5
r6
r7

}