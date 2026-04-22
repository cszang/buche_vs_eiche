library(ncdf4)



# What has been done before (outside R):
# - re-aggregate from one file per GCM/RCM/RCP/year to one file per GCM/RCM/RCP
# - reproject from native (strange projection) to Lambert Conformal Conic; ETRS 1989 LCC; EPSG 3034

test_nc <- nc_open("~/LocalData/LFU_Ens/RCP_85/ICHEC-EC-EARTH_r12i1p1/CLMcom-CCLM4-8-17/tas/tas_Bayern-lcc5_ICHEC-EC-EARTH_rcp85_r12i1p1_CLMcom-CCLM4-8-17_LfU2020_19510101-21001231FULL.nc")
test_nc

lons <- ncvar_get(test_nc, "lon")
lats <- ncvar_get(test_nc, "lat")

# Bausenberg
lon <- 11.00
lat <- 50.28

# which lon/lat-combination corresponds to our study area?
lon_study <- which.min(abs(lon - lons))
lat_study <- which.min(abs(lat - lats))

temp <- ncvar_get(test_nc, "temp", start = c(lon_study, lat_study, 1),
                  count = c(1, 1, -1))

start_date <- as.Date("1951-01-01")
end_date <- as.Date("2100-12-31")
date_sequenz <- seq.Date(start_date, end_date, by = "1 day")
head(date_sequenz)
length(date_sequenz)
length(temp)

# remove leap-days from date sequenz, because the data is based on a constant
# 365-year
date_sequenz <- date_sequenz[!grepl(x = date_sequenz, pattern = "-02-29$")]
length(date_sequenz)

test_temp <- data.frame(
  date = date_sequenz,
  temp = temp
)

head(test_temp)

library(lubridate)

test_temp2 <- data.frame(
  year = year(date_sequenz),
  month = month(date_sequenz),
  day = day(date_sequenz),
  temp = temp
)

head(test_temp2)

# applying over all GCM/RCM/RCPs and the two variables prec and temp

# same for all files:

# extract grid coords
lons <- ncvar_get(nc_test, "lon")
lats <- ncvar_get(nc_test, "lat")
# get nearest grid point for coords
lon_min <- which.min(abs(lon - lons))
lat_min <- which.min(abs(lat - lats))
# dates
date_start <- as.Date("1951/01/01")
date_start2 <- as.Date("1970/01/01")
date_end <- as.Date("2100/12/31")
# 365-day years...
dates <- seq.Date(date_start, date_end, by = "1 day")
# remove leap-days
dates2 <- dates[!grepl(x = dates, pattern = "-02-29$")]

# traverse file system to identify available combinations of GCMs/RCMs
lfu_path <- "~/LocalData/LFU_Ens"
combis <- list.files(lfu_path, recursive = TRUE)
combis_split <- strsplit(combis, "/")
models <- data.frame(
  rcp = sapply(combis_split, "[", 1),
  gcm = sapply(combis_split, "[", 2),
  rcm = sapply(combis_split, "[", 3),
  variable = sapply(combis_split, "[", 4),
  file = combis
)

models_prec <- models[models$variable == "pr", ]
models_temp <- models[models$variable == "tas", ]

# extract all prec

n_prec <- nrow(models_prec)
for (i in 1:n_prec) {
  cat(i, "...\n")
  nc <- tryCatch(nc_open(paste0(lfu_path, "/", models_prec$file[i])),
                 error = function(e) e)
  if (!any(class(nc) == "error")) {
    var_extract <- ncvar_get(nc, "prec", start = c(lon_min, lat_min, 1),
                             count = c(1, 1, -1))
    start_date <- ncdf4::ncatt_get(nc, "time")$units
    time_regex <- regexec(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date)
    start_date <- as.Date(substr(start_date, start = time_regex[[1]][1], stop = time_regex[[1]][1] + attr(time_regex[[1]] - 1, "match.length")))
    date_end <- as.Date("2100/12/31")
    # 365-day years...
    dates <- seq.Date(start_date, date_end, by = "1 day")
    # remove leap-days
    dates2 <- dates[!grepl(x = dates, pattern = "-02-29$")]
    time_nc <- ncvar_get(nc, "time")
    if (length(dates2) != length(time_nc)) {
      dates2 <- dates2[1:length(time_nc)]
    }
    nc_close(nc)
    
    var_df <- data.frame(
      year = year(dates2),
      month = month(dates2),
      day = day(dates2),
      variable = var_extract
    )
    
    # daily to monthly:
    agg <- aggregate(var_df$variable, by = list(var_df$year, var_df$month),
                     FUN = sum) # for precip!
    names(agg) <- c("year", "month", "prec")
    agg$rcp <- models_prec$rcp[i]
    agg$gcm <- models_prec$gcm[i]
    agg$rcm <- models_prec$rcm[i]
    if (i == 1) {
      AGG <- agg
    } else {
      AGG <- rbind(AGG, agg)
    }
  }
}

# extract all temp

n_temp <- nrow(models_temp)
for (i in 1:n_temp) {
  cat(i, "...\n")
  nc <- tryCatch(nc_open(paste0(lfu_path, "/", models_temp$file[i])),
                 error = function(e) e)
  if (!any(class(nc) == "error")) {
    var_extract <- ncvar_get(nc, "temp", start = c(lon_min, lat_min, 1),
                             count = c(1, 1, -1))
    start_date <- ncdf4::ncatt_get(nc, "time")$units
    time_regex <- regexec(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date)
    start_date <- as.Date(substr(start_date, start = time_regex[[1]][1], stop = time_regex[[1]][1] + attr(time_regex[[1]] - 1, "match.length")))
    date_end <- as.Date("2100/12/31")
    # 365-day years...
    dates <- seq.Date(start_date, date_end, by = "1 day")
    # remove leap-days
    dates2 <- dates[!grepl(x = dates, pattern = "-02-29$")]
    time_nc <- ncvar_get(nc, "time")
    if (length(dates2) != length(time_nc)) {
      dates2 <- dates2[1:length(time_nc)]
    }
    nc_close(nc)
    
    var_df <- data.frame(
      year = year(dates2),
      month = month(dates2),
      day = day(dates2),
      variable = var_extract
    )
    
    # daily to monthly:
    agg <- aggregate(var_df$variable, by = list(var_df$year, var_df$month),
                     FUN = mean) # for temp!
    names(agg) <- c("year", "month", "temp")
    agg$rcp <- models_temp$rcp[i]
    agg$gcm <- models_temp$gcm[i]
    agg$rcm <- models_temp$rcm[i]
    if (i == 1) {
      AGG_temp <- agg
    } else {
      AGG_temp <- rbind(AGG_temp, agg)
    }
  }
}

AGG_all <- dplyr::full_join(AGG, AGG_temp)
nrow(AGG_all)
head(AGG_all)

AGG_all <- AGG_all %>% dplyr::select(rcp, gcm, rcm, year, month, temp, prec) %>% 
  dplyr::arrange(rcp, gcm, rcm, year, month)

write.csv2(AGG_all, "data/lfu_cordex_ensemble_monthly.csv", row.names = FALSE)

# Plot projection data

# step 0: annual data
projections <- AGG_all
projections_yearly <- aggregate(projections$temp, by = list(
  year = projections$year,
  rcm = projections$rcm,
  gcm = projections$gcm,
  rcp = projections$rcp), mean)
projections_yearly_prec <- aggregate(projections$prec, by = list(
  year = projections$year,
  rcm = projections$rcm,
  gcm = projections$gcm,
  rcp = projections$rcp), sum)

# step 1: good time axis

# projections$date <- as.Date(paste0(projections$year, "-", projections$month, "-15"))
# 
# plot(range(projections$date), c(-15, 35), type = "n",
#      xlab = "Time", ylab = "Temperature (°C)")
# set1 <- projections[projections$rcp == "RCP_85" &
#                       projections$gcm == "MPI-M-MPI-ESM-LR_r1i1p1" &
#                       projections$rcm == "UHOH-WRF361H", ]
# lines(set1$date, set1$temp)

plot(range(projections_yearly$year), c(-15, 35), type = "n",
     xlab = "Time", ylab = "Temperature (°C)")
set1 <- projections_yearly[projections_yearly$rcp == "RCP_85" &
                             projections_yearly$gcm == "MPI-M-MPI-ESM-LR_r1i1p1" &
                             projections_yearly$rcm == "UHOH-WRF361H", ]
lines(set1$year, set1$x)

# tidyverse

library(ggplot2)
ggplot(projections_yearly) +
  geom_line(aes(year, x, colour = rcm, linetype = gcm)) +
  facet_wrap(~ rcp) +
  ylab("Temperature (°C)") + 
  xlab("Year") +
  xlim(c(2020, 2100))

ggsave("Figures/cordex_temp_ensemble_yearly_2020.pdf")

ggplot(projections_yearly) +
  geom_smooth(aes(year, x, colour = rcm, linetype = gcm), se = FALSE) +
  facet_wrap(~ rcp) +
  ylab("Temperature (°C)") + 
  xlab("Year") +
  xlim(c(2020, 2100))

ggsave("Figures/cordex_temp_ensemble_trends_2020.pdf")

ggplot(projections_yearly_prec) +
  geom_line(aes(year, x, colour = rcm, linetype = gcm)) +
  facet_wrap(~ rcp) +
  ylab("Precipitation sum (mm)") +
  xlab("Year")

ggsave("Figures/cordex_prec_ensemble_yearly.pdf")

ggplot(projections_yearly_prec) +
  geom_line(aes(year, x, colour = rcm, linetype = gcm)) +
  facet_wrap(~ rcp) +
  ylab("Precipitation sum (mm)") +
  xlab("Year") +
  xlim(c(2020, 2100))

ggsave("Figures/cordex_prec_ensemble_yearly_2020.pdf")

ggplot(projections_yearly_prec) +
  geom_smooth(aes(year, x, colour = rcm, linetype = gcm), se = FALSE) +
  facet_wrap(~ rcp) +
  ylab("Precipitation sum (mm)") +
  xlab("Year")

ggsave("Figures/cordex_prec_ensemble_trend.pdf")

ggplot(projections_yearly_prec) +
  geom_smooth(aes(year, x, colour = rcm, linetype = gcm), se = FALSE) +
  facet_wrap(~ rcp) +
  ylab("Precipitation sum (mm)") +
  xlab("Year") +
  xlim(c(2020, 2100))

ggsave("Figures/cordex_prec_ensemble_trend_2020.pdf")


