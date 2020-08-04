# packages and options ---------------------------------------------------------
library(tidyverse)
library(data.table)
library(sf)
library(leaflet)
library(mapview)
library(ggsci)
library(ggiraph)

# options(viewer = NULL)


# global settings --------------------------------------------------------------
# scales::show_col(pal_locuszoom(alpha = 0.6)(7))
cpal = pal_locuszoom(alpha = 0.6)(7)

parameter_monthly = tribble(
  ~parameter, ~label, ~label_plot, ~unit,
  "mean_airtemp",       "Air temperature (monthly avg)",     "temp",     "°C",
  "mean_airtemp_max",   "Air max temperature (monthly avg)", "max temp", "°C",
  "max_airtemp",        "Air max temperature (monthly max)", "max temp", "°C",
  "mean_airtemp_min",   "Air min temperature (monthly avg)", "min temp", "°C",
  "min_airtemp",        "Air min temperature (monthly min)", "min temp", "°C",
  "sum_rain",           "Rainfall (monthly total)",          "rain",     "mm",
  "max_rain",           "Rainfall (monthly max)",            "rain",     "mm",
  "mean_cloudcoverage", "Cloud coverage (monthly avg)",      "clouds",   "okta",
  "sum_sun",            "Sun hours (monthly total)",         "sun",      "h",
  "mean_wind",          "Wind (monthly avg)",                "wind",     "Bft"
) %>% 
  as.data.table()

Encoding(parameter_monthly$unit) = "UTF-8"

theme_cdcexpl = theme(
  text = element_text(size = 14, colour = "#676A6D"),
  axis.line = element_line(size = 0.25, colour = "#676A6D"),
  axis.ticks = element_line(size = 0.25, colour = "#676A6D"),
  axis.text.x = element_text(size = 16, margin = margin(t = 10)),
  axis.text.y = element_text(size = 16, margin = margin(r = 10)),
  axis.title.y = element_text(size = 18, margin = margin(r = 10)),
  legend.title = element_text(size = 18, margin = margin(r = 5)),
  legend.text = element_text(size = 18, margin = margin(r = 5)),
  panel.background = element_blank(),
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)


# load data --------------------------------------------------------------------
# data and metadata
data_monthly = read_rds(path = "data/monthly_kl.rds")
data_monthly_meta = read_rds(path = "data/monthly_kl_meta.rds")

# vars and period by station
coverage = map_df(.x = parameter_monthly$parameter, .f = function(x){
  
  # x = "mean_airtemp"
  period = data_monthly[!is.na(get(x)), .SD, .SDcols = c("id", "year", x)]
  period = period[, .(start = min(year), end = max(year)), by = id]
  period[, parameter := x]
  
})

coverage = coverage[parameter_monthly, on = "parameter"]

# stations
id_subset = coverage[end == 2020 & start <= 1961][["id"]]
id_subset = unique(id_subset)

data_stations = read_rds(path = "data/geoindex.rds") %>% 
  filter(id %in% id_subset) %>% 
  group_by(id) %>% 
  slice(1)

# initial map of stations
obs_station_select = data_stations %>% filter(id == 430)

m = leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addCircleMarkers(data = data_stations,
                   radius = 3, 
                   stroke = FALSE,
                   fillColor = cpal[5], fillOpacity = 1, 
                   group = "obs_station_all",
                   layerId = data_stations$id,
                   label = mapview:::makeLabels(x = data_stations, zcol = "name")) %>% 
  addCircleMarkers(data = obs_station_select, 
                   radius = 10, 
                   stroke = FALSE,
                   fillColor = cpal[1], fillOpacity = 1, 
                   group = "obs_station_sel",
                   label = mapview:::makeLabels(x = obs_station_select, zcol = "name")) %>% 
  addEasyButton(
    button = easyButton(
      icon = "fa-globe", 
      title = "Zoom to full extent",
      onClick = JS("function(btn, map){ map.setZoom(5); }")
    )
  ) %>% 
  addMiniMap(tiles = providers$CartoDB.Positron, 
             toggleDisplay = TRUE, 
             zoomLevelOffset = -3) %>% 
  setView(lng = 10.5, lat = 51.3, zoom = 5)


# tab 1: observations ----------------------------------------------------------
obs_plot_function = function(data, 
                             id_plot, 
                             param_plot, 
                             period_plot){
  
  # data = data_monthly
  # id_plot = 430
  # param_plot = "Air temperature (monthly avg)"
  # period_plot = c(2021, 2022)
  
  if (is.null(param_plot)) {
    param_plot = coverage[id == id_plot][["label"]][1]
  }
  
  # get varname from parameter
  yvar = parameter_monthly[label %in% param_plot][["parameter"]]
  ylab_unit = parameter_monthly[label %in% param_plot][["unit"]]
  ylab_label = parameter_monthly[label %in% param_plot][["label_plot"]]
  
  # subset data to selected station and parameter (full period)
  cols_keep = c("id", "date", "year", "month", yvar)
  data = data[id == id_plot, ..cols_keep]
  setnames(x = data, old = yvar, new = "parameter")
  
  # mean and deviation per month
  data_ref = data[year >= 1961 & year <= 1990, 
                  .(parameter_mean = mean(parameter, na.rm = TRUE)), 
                  by = month]
  data = data[data_ref, on = "month"]  
  data[, parameter_dev := parameter - parameter_mean]
  data[, parameter_dev_sign := ifelse(parameter_dev < 0, "below average", "above average")]
  # data[, parameter_dev_sign := forcats::fct_relevel(.f = parameter_dev_sign, c("above average", "below average"))]
  
  # second filter on period
  # capture period_plot is NULL
  if (is.null(period_plot)) {
    period_plot = c(2000, 2020)
    # period_plot = melt(data = coverage[id == id_plot &
    #                                      label == param_plot,
    #                                    .(start, end)],
    #                    measure.vars = c("start", "end"))[["value"]]
    # if (period_plot[1] < period_plot[2] - 20) period_plot[1] = period_plot[2] - 20
  }
  
  # capture period_plot is not yet updated
  # if (period_plot[1] < min(data[["year"]])) period_plot[1] = min(data[["year"]])
  # if (period_plot[1] > max(data[["year"]])) period_plot[1] = min(data[["year"]])
  # if (period_plot[2] < min(data[["year"]])) period_plot[2] = max(data[["year"]])
  # if (period_plot[2] > max(data[["year"]])) period_plot[2] = max(data[["year"]])
  
  data = data[year >= period_plot[1] & year <= period_plot[2]]  
  
  # plot values
  p_param = ggplot(data = data, 
                   mapping = aes(x = date, 
                                 y = parameter)) + 
    geom_bar_interactive(stat = "identity",
                         mapping = aes(tooltip = str_c(month.abb[month], "-", year, ": ", 
                                                       round(x = parameter, digits = 1))), 
                         fill = cpal[7],
                         color = cpal[7]) + 
    scale_x_date(name = "") +
    scale_y_continuous(name = str_c(ylab_label, " [", ylab_unit, "]"), 
                       expand = c(0.1, 0.1)) + 
    theme_cdcexpl  

  # plot deviation
  p_dev = ggplot(data = data,
                 mapping = aes(x = date, 
                               y = parameter_dev, 
                               color = parameter_dev_sign,
                               fill = parameter_dev_sign)) +
    geom_bar_interactive(stat = "identity",
                         mapping = aes(tooltip = str_c(month.abb[month], "-", year, ": ", 
                                                       round(x = parameter_dev, digits = 1))),
                         show.legend = FALSE) +
    scale_x_date(name = "") +
    scale_y_continuous(name = str_c(ylab_label, " - deviation [", ylab_unit, "]"),
                       expand = c(0.1, 0.1)) +
    scale_fill_manual("deviation", values = cpal[c(1, 5)]) +
    scale_color_manual("deviation", values = cpal[c(1, 5)]) +
    labs(caption = "Source: Deutscher Wetterdienst") + 
    theme_cdcexpl
  
  p_list = list("p_param" = p_param, "p_dev" = p_dev)
  
}

