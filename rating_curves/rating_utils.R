
# scrape a page from  Poudre Rock Report
scrape_page <- function(n) {
  
  logger::log_info("Scraping page {n}")
  
  site <- paste0(
    "http://www.poudrerockreport.com/category/flows/page/",
    as.character(n)
    # "http://www.poudrerockreport.com/category/flows/page/", as.character(page[8])
  )
  
  articles <- rvest::read_html(site) %>%
    rvest::html_elements("article")
  
  # extract titles
  titles <- rvest::html_attrs(articles) %>%
    lapply(`[`, "aria-label") %>%
    unlist(use.names = FALSE)
  
  # extract dates
  dates <- rvest::html_elements(articles, "time") %>%
    rvest::html_text2()
  
  categories <- rvest::html_elements(articles, ".entry-categories") %>%
    rvest::html_text2() %>%
    stringr::str_remove_all("Filed Under: ")
  
  # blog entry content text contains stage and flow observations at several sites
  entry_content = rvest::read_html(site) %>%
    html_nodes(".entry-content") %>%
    html_text()
  
  # ---- HEWLETT ----
  # locates the string "Hewlett" and select a bunch of characters just after it. 
  J = str_locate(entry_content, pattern = "Hewlett") # locate
  hewlett_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
  
  # break "Hewlett"-containing strings into chunks separated by spaces
  comps <- str_split(hewlett_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  hewlett_stage = c()
  for (i in 1:nrow(comps)) {
    # find "Feet"
    I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    hewlett_stage[i] = as.numeric(comps[i,min(I)-1])
  }
  
  # ---- RUSTIC ----
  # locates the string "Rustic" and select a bunch of characters just after it. 
  J = str_locate(entry_content, pattern = "Rustic") # locate
  rustic_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
  
  # break "Rustic"-containing strings into chunks separated by spaces
  comps <- str_split(rustic_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  rustic_stage = c()
  for (i in 1:nrow(comps)) {
    
    # find "Feet"
    I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    rustic_stage[i] = as.numeric(comps[i,min(I)-1])
    
  }
  # remove special characters EXCEPT period
  title_content <- stringi::stri_replace_all_fixed(
    titles, 
    c("{", "}", "~", "[", "]", "(", ")","!", "@", "<", ">"),
    c("", "", "", "", "","", "","", "", "", ""),
    vectorize_all = FALSE
  )
  
  pine_stage <- str_extract(title_content, "\\d+\\.*\\d*") %>% 
    as.numeric()
  
  # convert post date to Date class
  date   <- lubridate::mdy(dates)
  # date = as.Date(dates, format = "%B %d %Y")
  
  data.frame(
    title        = titles,
    date         = date,
    pineview     = pine_stage,
    hewlett      = hewlett_stage,
    rustic       = rustic_stage,
    page         = n,   # page         = 187,
    categories   = categories
  ) %>% 
    mutate( 
      time = format( # extract 4 digit military time from title and convert to hour, minutes
        as.POSIXct(
          sprintf("%04s", stri_extract_last(title, regex = "\\d{4}")),
          format  = "%H%M",
          origin  = "1970-01-01",
          tz      = "UTC"
        ), "%H:%M"
      )
    ) %>% 
    mutate(
      time = case_when(
        str_detect(title, regex('noon', ignore_case = T)) == TRUE ~ "12:00",
        str_detect(title, regex('noon', ignore_case = T)) == FALSE ~ time
        )
      ) %>%
    mutate(
      time = format(
        round(
          strptime(paste("2001-01-01", time), format="%Y-%m-%d %H:%M"), 
          units="hours"), 
        format="%H:%M")) %>% 
    mutate(datetime = as.POSIXct(paste(as.character(date), time),  format = "%Y-%m-%d %H:%M")) %>% 
    dplyr::relocate(title, categories, page, date, time, datetime, pineview, hewlett, rustic)
}

# Function that retrieves flow data frame https://opendata.fcgov.com/ by sensor name and returns a tidy dataframe.
get_flow_data <- function(sensor_name) {
  
  url <- "https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name="
  
  sensor_url <- paste0(url, gsub(" ", "%20", sensor_name))

    flow_data <- RSocrata::read.socrata(
    url = sensor_url                    # 'https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name=Poudre%20Park'
  )
  
  flow_data2 <- flow_data %>% 
    dplyr::select(sensor_name, timestamp, stage_ft, flow_cfs) %>% 
    dplyr::mutate(
      flow_cfs   = as.numeric(flow_cfs),
      stage_ft   = as.numeric(stage_ft),
      date       = lubridate::ymd(as.Date(timestamp)),
      time       = format(timestamp, "%H:%M")
    ) %>% 
    dplyr::select(sensor_name, date, time, stage_ft, flow_cfs) %>% 
    mutate(
      time      =   format(
          round(
            strptime(paste("2001-01-01", time), format="%Y-%m-%d %H:%M"), 
            units="hours"), 
          format="%H:%M"
        ),
      datetime  =  as.POSIXct(paste(as.character(date), time),  format = "%Y-%m-%d %H:%M")
    ) %>% 
    dplyr::relocate(sensor_name, date, time, datetime, stage_ft, flow_cfs)
}

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
      time        = substr(date, 12, 19),
      date        = as.POSIXct(paste(as.character(substr(date, 0, 10))),
                                format = "%Y-%m-%d"), 
      datetime    = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),
                                format = "%Y-%m-%d %H:%M")
      ) %>% 
    dplyr::relocate(station, date, time, datetime, parameter, value)
  
}

# Function for calling CDSS API for Telemetry station data
get_structure_data <- function(
  type = c("divrecday", "divrecmonth", "divrecyear", "stagevolume"),
  wdid
) {
  # base URL for CDSS diversion records API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
  
  # format start and end date for URL 
  # start <- paste0(gsub("-", "%2F", start_date), "_00%3A00")
  # end   <- paste0(gsub("-", "%2F", end_date), "_00%3A00")
  
  # create specific URL w/ WDID to call API (with dates)
  # url <- paste0(base, type, "/?dateFormat=spaceSepToSeconds&min-dataMeasDate=", start, "&max-dataMeasDate=", end, "&wcIdentifier=*Total+(Diversion)*&wdid=", wdid)
  
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
      time        = substr(date, 12, 19),
      date        = as.POSIXct(paste(as.character(substr(date, 0, 10))),
                               format = "%Y-%m-%d"), 
      datetime    = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),
                               format = "%Y-%m-%d %H:%M")
    ) %>% 
    dplyr::relocate(wdid, date, time, datetime, unit, value)
  
}
