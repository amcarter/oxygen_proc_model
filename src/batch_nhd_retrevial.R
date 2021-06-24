# adapted from batch summary nhd data created by
#Mike Vlah (vlahm13@gmail.com)

#1. acquire NHDPlusV2 COMIDs based on lat/long
#2. acquire NHDPlusV2 VPU IDs based on lat/long
#4. use COMID and VPU to acquire NHDPlusV2 data for your sites

#see NHDPlusV2 docs 
#1. ftp://ftp.horizon-systems.com/NHDplus/NHDPlusV21/Documentation/NHDPlusV2_User_Guide.pdf

#NOTE: these tools necessarily download a lot of large datasets and store them
#in memory. keep an eye on your usage.

library(dplyr)
library(plyr)
# remotes::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)
# remotes::install_github("jsta/nhdR")
library(nhdR)
library(sf)

# 1. setup and helper functions ####

#set your working directory to the location of your site data file.
#   site data must include latitude and longitude columns (decimal degrees)
# setwd('C:/Users/Alice Carter/Desktop/untracked')
WGS84 = 4326 #EPSG code for coordinate reference system, 
#   this might have to change for some gps points

comid_from_point = function(lat, long, CRS) {
  pt = sf::st_point(c(long, lat))
  ptc = sf::st_sfc(pt, crs=CRS)
  COMID = nhdplusTools::discover_nhdplus_id(ptc)
  if(! length(COMID)) COMID = NA
  return(COMID)
}

vpu_from_point = function(lat, long, CRS) {
  pt = sf::st_point(c(long, lat))
  ptc = sf::st_sfc(pt, crs=CRS)
  VPU = nhdR::find_vpu(ptc)
  return(VPU)
}

#this acquires nhdplusv2 data for a single site by COMID.
#it's just a thin wrapper around nhdR::nhd_plus_load
nhdplusv2_from_comid = function(VPU, COMID, component, DSN, quiet=FALSE) {
  
  if(! quiet){
    message(paste0('The nhdR package downloads NHDPlusV2 components to ',
                   nhdR:::nhd_path(), '. Unfortunately this cannot be changed.',
                   ' Fortunately, each component need only be downloaded once.'))
  }
  
  data = nhdR::nhd_plus_load(vpu=VPU, component=component,
                             dsn=DSN, approve_all_dl=TRUE)
  
  colnames(data)[colnames(data) == 'ComID'] = 'COMID'
  colnames(data)[colnames(data) == 'ReachCode'] = 'REACHCODE'
  data = data[data$COMID == COMID,]
  
  return(data)
}

#this calls nhdplusv2_from_comid repeatedly to get data for all your sites.
#the dataframe must include COMID and VPU columns
nhdplusv2_bulk = function(site_df, nhdplusv2_sets, quiet=FALSE){
  
  nhdplus_data = data.frame()
  if(any(is.na(site_df$COMID))) stop('you have missing COMIDs')
  
  for(j in 1:nrow(site_df)){
    for(i in 1:length(setlist)){
      print(paste(j, nhdplusv2_sets[[i]]))
      
      if(i == 1 || initerr){
        row_base = try(nhdplusv2_from_comid(site_df$VPU[j],
                                            site_df$COMID[j], 
                                            names(setlist[i]), setlist[[i]],
                                            quiet=quiet))
        if('try-error' %in% class(row_base) || nrow(row_base) > 1){
          initerr = TRUE
          row_base = data.frame(COMID=site_df$COMID[j])
        } else {
          initerr = FALSE
        }
      } else {
        row_ext = try(nhdplusv2_from_comid(site_df$VPU[j],
                                           site_df$COMID[j], 
                                           names(setlist[i]), setlist[[i]],
                                           quiet=quiet))
        if(! 'try-error' %in% class(row_ext) && nrow(row_ext) == 1){
          row_base = left_join(row_base, row_ext)
        }
      }
      
    }
    
    if(nrow(row_base) > 1){
      row_base = data.frame(COMID=site_df$COMID[j])
    }
    
    nhdplus_data = rbind.fill(nhdplus_data, row_base)
  }
  
  return(nhdplus_data)
}

# get NHDPlusV2 data ####
get_nhd_data <- function(sites, crs = 4326, #WGS84
                         vars = c(COMID, STREAMORDE, SLOPE, REACHCODE, AREASQKM, 
                                  TOTDASQKM, MAXELEVSMO, MINELEVSMO)){
  #COMID is the NHD identifier for any reach in the continental U.S.
  #add COMIDs to your site table. If this doesn't work, update nhdplusTools
  sites$COMID = unlist(mapply(comid_from_point, sites$latitude,
                              sites$longitude, crs))
  sites = sites[! is.na(sites$COMID),]
  
  #VPU == NHD vector processing unit. NHDPlusV2 data are downloaded per VPU.
  #add VPUs to your site table and determine reach proportions.
  sites$VPU = unlist(mapply(vpu_from_point, sites$latitude,
                            sites$longitude, crs))
  
  #construct list of DSN=component pairs to acquire. see NHDPlus docs for more.
  setlist = list('NHDPlusAttributes'='ElevSlope')
  
  #retrieve NHDPlusV2 data
  nhdplusv2_data = nhdplusv2_bulk(sites, setlist, quiet=TRUE)
  
  #nhd variable names do not have consistent naming conventions. sometimes they're
  #all caps; other times camel case. here's a crude way to deal with that.
  colnames(nhdplusv2_data) = toupper(colnames(nhdplusv2_data))
  nhdplusv2_data = nhdplusv2_data[, ! duplicated(colnames(nhdplusv2_data))]
  
  #pick out the variables you want, then join them to your site data
  nhdplusv2_data = select(nhdplusv2_data, vars)
  sites = left_join(sites, nhdplusv2_data, by='COMID')
  
  return(sites)
}
