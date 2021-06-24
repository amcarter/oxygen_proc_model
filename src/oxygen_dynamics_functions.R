# physical functions for modeling DO
# data retrevial functions in batch_nhd_retrevial.R

# water velocity as a function of Q
#   v = eQ^f
#   where:  e = e^-1.64 = 0.194
#           f = 0.285  from Raymond 2012

calc_vel_from_disch <- function(q_m3s){
  v_ms = exp(-1.64) * q_m3s ^ 0.285
  return(v_ms)
  
}

# model K600 based on Raymond equation 5
calc_k600_raymond5 <- function(slope, v_ms){
  k600_mean = v_ms * slope * 2841 + 2.02
  # k600_max = v_ms * slope * (2841 + 107) + 2.02 + 0.209
  # k600_min = v_ms * slope * (2841 - 107) + 2.02 - 0.209
  return(k600_mean)
}

# convert k600 (m/d) to k
kO2fromk600<-function(temp, k600) {
  sa = 1568
  sb = -86.04
  sc = 2.142
  sd = -0.0216
  se = -0.5
  kO2 = k600 * ((sa + sb * temp + sc * temp ^2 + sd * temp ^3)/600)^se
}

