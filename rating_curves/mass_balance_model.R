# Angus Watters
# American Whitewater
# Mass Balance Model at Poudre River


# ================
# ---- Steps: ----
# ================

# 1. Use functions from rating_utils.R to retrieve data from these locations:
        # Poudre Park 
        # Poudre Canyon Mouth
        # Poudre Valley Canal
        # North Fork Poudre
        # Northern Poudre Supply Canal 

# 2. Compare flow data at Poudre Park and Poudre Canyon Mouth, and evaluate relationship
# 3. add flow from Poudre Valley Canal (if needed)
# 4. subtract flow from North Fork Poudre (if needed)
# 5. add flow from Northern Poudre Supply Canal (if needed)

# ============================
# ---- Mass Balance Model ----
# ============================

library(tidyverse)
library(httr)
library(jsonlite)
library(logger)
library(here)

# source(paste0(here(),"rating_utils.R"))
# ====================================
# ---- Functions to get flow data ----
# ====================================

# Function for calling CDSS API for Telemetry station data
get_station_data <- function(
                            type = c("telemetrytimeseriesraw", "telemetrytimeserieshour",
                                     "telemetrytimeseriesday", "telemetryratingtable"), 
                             abbrev,
                             param = c("DISCHRG", "GAGE_HT", "AIRTEMP"), 
                             start_date, 
                             end_date
                             ) {
  
  # base URL for CDSS Telemetry station API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/"
  
  # format start and end date for URL 
  start <- gsub("-", "%2F", start_date)
  end <- gsub("-", "%2F", end_date)
  
  # create specific URL w/ WDID to call API
  url <- paste0(base, type, "/?dateFormat=spaceSepToSeconds&abbrev=", abbrev, "&endDate=", end, "&parameter=", param, "&startDate=", start)
  
  # GET request to CDSS API
  cdss_api <- httr::GET(url) %>%
    content(as = "text") %>% 
    fromJSON() %>% 
    bind_rows() 
  
  # Tidy data 
  station_data <- cdss_api$ResultList %>% 
    dplyr::select(
            station  = abbrev,
            date     = measDate,
            parameter, 
            value    = measValue
                  ) %>% 
    mutate(
            time     = substr(date, 12, 19),
            date     = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),
                                  format = "%Y-%m-%d %H:%M")
          ) %>% 
    dplyr::relocate(station, date, time, parameter, value)

}


# Function for calling CDSS API for Telemetry station data
get_structure_data <- function(
                              type = c("divrecday", "divrecmonth", "divrecyear", "stagevolume"),
                              wdid
                              ) {
  # base URL for CDSS diversion records API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
  
  # create specific URL w/ WDID to call API
  url <- paste0(base, type, "/?dateFormat=spaceSepToSeconds&wcIdentifier=*Total+(Diversion)*&wdid=", wdid)
  
  # GET request to CDSS API
  cdss_api <- httr::GET(url) %>%
    content(as = "text") %>% 
    fromJSON() %>% 
    bind_rows() 
  
  # Tidy data 
  structure_data <- cdss_api$ResultList %>%  
    dplyr::select(
              wdid,
              date     = dataMeasDate,
              value    = dataValue,
              unit     = measUnits
                ) %>% 
    mutate(
             time     = substr(date, 12, 19),
             date     = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),  
                                   format = "%Y-%m-%d %H:%M")
             ) %>% 
    dplyr::relocate(wdid, date, time, unit, value)
  
}

# ===========================
# ---- Read in flow data ----
# ===========================

# get flow data for Poudre Canyon mouth
poudre_canyon_mouth <- get_station_data(
                                        type        = "telemetrytimeserieshour",
                                        abbrev      = "CLAFTCCO",
                                        param       = "DISCHRG",
                                        start_date  = "03-22-2019",
                                        end_date    = "12-29-2021"
                                        )

# get flow data for Poudre Valley Canal
poudre_valley_canal <- get_structure_data(
                                        type        = "divrecday",
                                        wdid        = "0300907"
                                        )

# get flow data for North Fork Poudre river
north_fork_poudre <- get_station_data(
                                        type        = "telemetrytimeserieshour",
                                        abbrev      = "CLANSECO",
                                        param       = "DISCHRG",
                                        start_date  = "03-22-2019",
                                        end_date    = "12-29-2021"
                                      )

# get flow data for Poudre Valley Canal
poudre_valley_canal <- get_structure_data(
                                        type        = "divrecday",
                                        wdid        = "0300905"
                                      )
# get flow data for North Fork Poudre river
muncanco <- get_station_data(
                                type        = "telemetrytimeserieshour",
                                abbrev      = "MUNCANCO",
                                param       = "DISCHRG",
                                start_date  = "03-22-2019",
                                end_date    = "12-29-2021"
                              )

# get flow data at Northern Poudre Supply Canal
poudre_park         <- get_flow_data(sensor_name = "Poudre Park")

# =============================================
# ---- Poudre Park vs. Poudre Canyon Mouth ----
# =============================================

# poudre park
pp  <- poudre_park %>% 
  tibble() %>% 
  group_by(datetime) %>% 
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  ) %>% 
  rename(flow_pp = flow_cfs)

# poudre canyon mouth
pcm <- poudre_canyon_mouth %>% 
  tibble() %>% 
  # filter(date == "2019-03-24") %>% 
  rename(flow_pcm = value)

# Join Poudre Park & Poudre Canyon mouth
pp_pcm <- left_join(
  pp,
  dplyr::select(pcm, datetime, flow_pcm), 
  by = "datetime"
  )

# Timeseries of flow at Poudre Canyon Mouth (red) & Poudre Park 
ggplot() +
  geom_point(data = pp_pcm, aes(x = datetime, y = flow_pp, col = "Poudre Park")) +
  geom_point(data = pp_pcm, aes(x = datetime, y = flow_pcm, col = "Poudre Canyon Mouth"), alpha = 0.2) +
  labs(
    title  = "Flow at Poudre Park and Poudre Canyon Mouth",
    y      = "Flow (cfs)",
    x      = "Date (hourly)",
    colour = " "
  ) +
  theme_bw() +
  scale_colour_manual(values = c("red", "black")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  scale_x_datetime(date_breaks = "6 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
    )

# Poudre park flow vs. Poudre Canyon mouth flow
ggplot() +
  geom_point(data = pp_pcm, aes(x = flow_pcm, y = flow_pp)) +
  # geom_point(data = pp_pcm, aes(x = flow_pp, y = flow_pcm)) +
  labs(
    title = "Flow at Poudre Park vs Poudre Canyon Mouth",
    y     = "Poudre Canyon mouth flow (cfs)",
    x     = "Poudre Park flow (cfs)"
  ) +
  theme_bw() +
  # scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  # scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 10)
  )

# =====================================================
# ---- Add Poudre Valley Canal flow to Poudre Park ----
# =====================================================


# Daily flow values at Poudre Valley Canal
pvc <- poudre_valley_canal %>% 
  tibble() %>% 
  filter(datetime >= min(pp_pcm$datetime))

# mean daily flow at Poudre Park & Poudre Canyon Mouth 
pp_pcm_daily <- pp_pcm %>%
  mutate(date = as.POSIXct(as.character(substr(pp_pcm$datetime, 0, 10)), format = "%Y-%m-%d")) %>% 
  group_by(date) %>% 
  summarise(
    flow_pcm = mean(flow_pcm, na.rm = T),
    flow_pp  = mean(flow_pp, na.rm = T)
    )

mun <- muncanco %>% 
  tibble() %>% 
  group_by(date) %>% 
  summarise(
    flow_mun = mean(value, na.rm = T)
  )
north <- north_fork_poudre %>% 
  tibble() %>% 
  mutate(date = as.POSIXct(as.character(substr(datetime, 0, 10)), format = "%Y-%m-%d")) %>% 
  group_by(date) %>% 
  summarise(
    flow_north = mean(value, na.rm = T)
  )

# Join Poudre valley canal w/ Poudre Canyon mouth flow
poudre_aggregate <- pvc %>% 
  dplyr::select(date, flow_pvc = value) %>% 
  left_join(
      pp_pcm_daily,
      by = "date"
    ) %>%
  left_join(
    dplyr::select(mun, date, flow_mun),
    by = "date"
  ) %>%
  left_join(
    dplyr::select(north, date, flow_north),
    by = "date"
  ) %>%
  mutate(
    flow_pcm_pv     = flow_pvc + flow_pcm,
    flow_pcm_pv_mun = flow_pvc + flow_pcm + flow_mun,
    flow_agg        = flow_pvc + flow_pcm + flow_mun - flow_north
  )

pcm_pv_plot <- 
  ggplot() +
  geom_point(data = poudre_aggregate, 
             aes(x = flow_pcm_pv, y = flow_pp)
  ) +
  geom_line(data = poudre_aggregate, 
             aes(x = flow_pp, y = flow_pp)
  ) +
  labs(
    title  = "Poudre Canyon Mouth + Poudre Valley Canal",
    y      = "Poudre Park",
    x      = "Poudre Canyon Mouth + Poudre Valley Canal",
    colour = " "
  ) +
  theme_bw() +
  # scale_colour_manual(values = c("blue", "red", "black", "green")) +
  # scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  # scale_x_datetime(date_breaks = "4 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )

pcm_pv_mun_plot <- 
  ggplot() +
  geom_point(data = poudre_aggregate, 
             aes(x = flow_pcm_pv_mun, y = flow_pp) 
  ) +  
  geom_line(data = poudre_aggregate, 
                 aes(x = flow_pp, y = flow_pp)
  ) +
  labs(
    title  = "Canyon Mouth + Poudre Valley Canal + Monroe",
    y      = "Poudre Park",
    x      = "Canyon Mouth + Poudre Valley Canal + Monroe",
    colour = " "
  ) +
  theme_bw() +
  # scale_colour_manual(values = c("blue", "red", "black", "green")) +
  # scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  # scale_x_datetime(date_breaks = "4 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )

all_agg_plot <- 
  ggplot() +
  geom_point(data = poudre_aggregate, 
             aes(x = flow_agg, y = flow_pp)
  ) +
  geom_line(data = poudre_aggregate, 
            aes(x = flow_pp, y = flow_pp)
  ) +
  labs(
    title  = "Canyon Mouth + Poudre Valley Canal + Monroe - North",
    y      = "Poudre Park",
    x      = "Canyon Mouth + Poudre Valley Canal + Monroe - North",
    colour = " "
  ) +
  theme_bw() +
  # scale_colour_manual(values = c("blue", "red", "black", "green")) +
  # scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  # scale_x_datetime(date_breaks = "4 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )

pcm_pv_plot + pcm_pv_mun_plot + all_agg_plot


# Plot Poudre Park + Poudre Canyon Mouth + Poudre Valley Canal + Sum of Poudre Canyon Mouth + Poudre Valley Canal
poudre_plot <- ggplot() +
  geom_point(data = poudre_aggregate, aes(x = date, y = flow_pvc,  col = "Poudre Valley Canal")) + 
  geom_point(data = poudre_aggregate, aes(x = date, y = flow_pcm,  col = "Poudre Canyon Mouth")) + 
  geom_point(data = poudre_aggregate, aes(x = date, y = flow_pp,  col = "Poudre Park")) + 
  geom_point(data = poudre_aggregate, aes(x = date, y = flow_agg,  col = "Poudre Canyon Mouth + Poudre Valley Canal + Monroe - North fork")) +
  # geom_point(data = pp_pcm, aes(x = datetime, y = flow_pcm, col = "Poudre Canyon Mouth")) +
  # geom_point(data = pp_pcm, aes(x = datetime, y = flow_pcm, col = "Poudre Canyon Mouth")) +
  labs(
    title  = "Flow @  Poudre Canyon Mouth, Poudre Park, Poudre Valley Canal, & (Poudre Canyon Mouth + Poudre Valley Canal)",
    y      = "Flow (cfs)",
    x      = "Date (daily)",
    colour = " "
  ) +
  theme_bw() +
  scale_colour_manual(values = c("blue", "red", "black", "green")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  scale_x_datetime(date_breaks = "4 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )
library(patchwork)

ggplot() +
  geom_point(data = poudre_aggregate, 
             aes(x = flow_agg, y = flow_pp)
             ) +
  labs(
    title  = "Poudre Canyon Mouth + Poudre Valley Canal + Muncanco - North fork",
    y      = "Poudre Park",
    x      = "Poudre Canyon Mouth + Poudre Valley Canal + Muncanco - North fork",
    colour = " "
  ) +
  theme_bw() +
  # scale_colour_manual(values = c("blue", "red", "black", "green")) +
  # scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  # scale_x_datetime(date_breaks = "4 months") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )

# =====================================================
# ---- Add Muncanco ----
# =====================================================


muncanco <- tibble(muncanco)


# Join Poudre valley canal w/ Poudre Canyon mouth flow
poudre_aggregate <- poudre_aggregate %>% 
  left_join(
    dplyr::select(muncanco, date, flow_mun = value),
    by = "date"
  ) %>%
  mutate(
    flow_agg = flow_pvc + flow_pcm
  )


















