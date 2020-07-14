# packages and options ----------------------------------------------------
library(tidyverse)
library(data.table)
library(sf)
library(mapview)
library(rdwd)

# options(viewer = NULL)


# global settings and data ------------------------------------------------
data(geoIndex)  # stations
data(fileIndex) # index of all files (updated by rdwd package)
data(metaIndex) # stations * products

fileIndex = as.data.table(fileIndex)
geoIndex = as.data.table(geoIndex)
metaIndex = as.data.table(metaIndex)

glimpse(fileIndex)
glimpse(geoIndex)
glimpse(metaIndex)

geoIndex %>% filter(recentfile)
geoIndex %>% filter(!recentfile)

# convert stations to sf and save output
geoIndex %>% 
  # filter(nonpublic == 0) %>% 
  select(id, name, state, elevation = ele, recent = recentfile, lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  write_rds(path = "data/geoindex.rds", compress = "gz")


# workflow file index creation --------------------------------------------
# creates a file index from a apecific directory on the DWD server
# ind = indexFTP(folder = "daily/kl/historical")
# ind = indexFTP(folder = "daily/kl/recent")
# ind = indexFTP(folder = "monthly/kl")

# creates a file index from input arguments
# ind = createIndex(paths = "daily/kl/historical/tageswerte_00699_19490101_19580630_hist.zip", fname = "")

# creates a file index from input arguments
# ind = selectDWD(res = "monthly", var = "kl", per = "h", id = c(1, 3), outvec = TRUE)


# workflow download and import data ---------------------------------------
# download and return name of downloaded files
ind_zip = dataDWD(file = ind, 
                  dir = "data_download", 
                  overwrite = TRUE, 
                  read = FALSE)

# import downloaded files
dwd_data = readDWD(file = ind_zip, fread = TRUE)
dwd_data = rbindlist(l = dwd_data, use.names = TRUE)

# alternatively include read = TRUE in dataDWD
dwd_data = dataDWD(file = ind_zip, 
                   dir = "data_download", 
                   overwrite = TRUE, 
                   read = TRUE) %>%
  rbindlist(use.names = TRUE)


# monthly data ------------------------------------------------------------
ind = indexFTP(folder = "monthly/kl", dir = "data_download/monthly")
ind = ind %>% str_subset(pattern = "\\.pdf$|.\\.txt", negate = TRUE)
ind %>% str_subset(pattern = ".\\.zip$", negate = TRUE)

# joinbf (bf = base folder) required as index is created from indexFTP
ind_zip = dataDWD(file = ind, 
                  joinbf = TRUE, 
                  dir = "data_download/monthly", 
                  overwrite = TRUE, 
                  read = FALSE)

data_monthly = readDWD(file = ind_zip, 
                       fread = TRUE, 
                       quiet = TRUE) %>%
  rbindlist(use.names = TRUE)

# rename variables (see content of each zipfile - historical and recent)
data_monthly[, c("MESS_DATUM_BEGINN", "MESS_DATUM_ENDE", "eor") := NULL]
setnames(x = data_monthly, 
         old = c("STATIONS_ID", "MESS_DATUM", "QN_4", 
                 "MO_N", "MO_TT", "MO_TX", "MO_TN", "MO_FK", 
                 "MX_TX", "MX_FX", "MX_TN", "MO_SD_S", 
                 "QN_6", "MO_RR", "MX_RS"), 
         new = c("id", "date", "quality_4", 
                 "mean_cloudcoverage", "mean_airtemp", "mean_airtemp_max", "mean_airtemp_min", "mean_wind",
                 "max_airtemp", "max_wind", "min_airtemp", "sum_sun", 
                 "quality_6", "sum_rain", "max_rain"))

# quality flags
# source: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/DESCRIPTION_obsgermany_climate_monthly_kl_historical_en.pdf
# The quality levels "Qualitätsniveau" (QN) given here apply for the respective following columns. The values
# are the minima of the QN of the respective daily values. QN denotes the method of quality control, with which
# erroneous values are identified and apply for the whole set of parameters at a certain time. For the individual
# parameters there exist quality bytes in the internal DWD data base, which are not published here. Values
# identified as wrong are not published. Various methods of quality control (at different levels) are employed to
# decide which value is identified as wrong. In the past, different procedures have been employed. The quality
# procedures are coded as following:
#   quality level (column header: QN_)
# 1- only formal control during decoding and import
# 2- controlled with individually defined criteria
# 3- ROUTINE control with QUALIMET and QCSY
# 5- historic, subjective procedures
# 7- ROUTINE control, not yet corrected
# 8- quality control outside ROUTINE
# 9- ROUTINE control, not all parameters corrected
# 10- ROUTINE control finished, respective corrections finished

# check for duplicates
data_monthly = unique(data_monthly)

dup = data_monthly[, N := .N, by = c("id", "date")]
dup = dup[N > 1]
dup = dup[order(id, date)]

# remove duplicates
dup = dup[, lapply(.SD, mean, na.rm = TRUE), by = c("id", "date")]
dup = dup[, lapply(.SD, function(x) ifelse(is.nan(x), NA, x))]

# apply removal to entire dataset
data_monthly = data_monthly[, lapply(.SD, mean, na.rm = TRUE), by = c("id", "date")]
data_monthly = data_monthly[, lapply(.SD, function(x) ifelse(is.nan(x), NA, x))]

# reconvert date from numeric to date
data_monthly[, date := lubridate::as_date(date)]
data_monthly[, month := lubridate::month(date)]
data_monthly[, year:= lubridate::year(date)]

# save result
write_rds(x = data_monthly,
          path = "data/monthly_kl.rds", 
          compress = "gz")
# data_monthly = read_rds(path = "data/monthly_kl.rds")


# monthly data metadata ---------------------------------------------------
# index of zip files
ind_zip = list.files(path = "data_download/monthly", 
                     pattern = ".\\.zip$") %>% 
  str_c("data_download/monthly/", .)

# index of meta files
# NOTE: not all metafiles are present in all zips!
ind_meta = ind_zip %>% 
  map(~unzip(zipfile = .x, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset(pattern = "^Metadaten_.*.\\.txt$") %>% 
        str_sub(end = -11)) %>% 
  unlist() %>% 
  unique()

data_meta = map(.x = ind_zip, 
                .f = function(x){
                  
                  # x = ind_zip[1]
                  
                  files = x %>% 
                    unzip(list = TRUE) %>% 
                    pull(Name) %>% 
                    str_subset(pattern = "^Metadaten") %>% 
                    str_subset(pattern = ".\\.txt$")
                  
                  meta = map(.x = files, 
                             .f = function(y){
                               
                               # y = files[1]
                               
                               fread(cmd = paste("unzip -p", x, y), 
                                     header = TRUE, 
                                     sep = ";", fill = TRUE) %>% 
                                 setnames(old = names(.), new = str_to_lower((names(.)))) %>% 
                                 filter(str_detect(string = stations_id, 
                                                   pattern = "^generiert|^Legende", 
                                                   negate = TRUE))
                               
                             }) %>% 
                    set_names(nm = files %>% str_sub(end = -11))
                  
                  return(meta)
                  
                })

data_monthly_meta = ind_meta %>% 
  map(.f = function(x){
    
    # x = ind_meta[1]
    data_meta %>% 
      map_df(~.x[[x]])
    
  }) %>% 
  set_names(nm = ind_meta)

# save result
write_rds(x = data_monthly_meta,
          path = "data/monthly_kl_meta.rds", 
          compress = "gz")

# data_monthly_meta = read_rds(path = "data/monthly_kl_meta.rds")

