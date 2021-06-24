library(nhdplusTools)
library(sf)

# water velocity as a function of Q
#   v = eQ^f
#   where:  e = e^-1.64 = 0.194
#           f = 0.285  from Raymond 2012

calc_vel_from_disch <- function(q_m3s){
  v_ms = exp(-1.64) * q_m3s ^ 0.285
  return(v_ms)
  
}

# model K600 based on Raymond equations

calc_K600_raymond5 <- function(slope, v_ms){
  K600_mean = v_ms * slope * 2841 + 2.02
  K600_max = v_ms * slope * (2841 + 107) + 2.02 + 0.209
  K600_min = v_ms * slope * (2841 - 107) + 2.02 - 0.209
  return(c(K600_mean, K600_min, K600_max))
}


#### get slope from nhd

get_nhd_slope <- function(lat, long){
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