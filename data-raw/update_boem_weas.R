#' Was the data updated for renewable energy lease areas and wind planning areas from BOEM?
#' 
#' @return TRUE if updated, otherwise FALSE.
#' 
weas_updated = function() {
  
  # query ArcGIS REST service to get latest edits
  url <- 'https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/BOEM_Wind_Planning_and_Lease_Areas/FeatureServer'
  active_layer_info <- jsonlite::fromJSON(httr::content(httr::POST(paste0(url, '/0'), query = list(f = "json"), encode = "form", config = httr::config(ssl_verifypeer = FALSE)), as = "text"))
  planning_layer_info <- jsonlite::fromJSON(httr::content(httr::POST(paste0(url, '/2'), query = list(f = "json"), encode = "form", config = httr::config(ssl_verifypeer = FALSE)), as = "text"))
  
  # load data to get latest downloads
  load(file = here::here('data', 'boem_wea_outlines.rda'))
  
  # extract dates
  active_update <- lubridate::as_datetime(active_layer_info$editingInfo$lastEditDate / 1000) |> as.Date()
  planning_update <- lubridate::as_datetime(planning_layer_info$editingInfo$lastEditDate / 1000) |> as.Date()
  active_recent <- boem_wea_outlines |> subset(LEASE_STAGE == 'Active') |> (`[[`)("UPDATED") |> unique() |> as.Date(format = '%Y/%m/%d')
  planning_recent <- boem_wea_outlines |> subset(LEASE_STAGE == 'Planning') |> (`[[`)("UPDATED") |> unique() |> as.Date(format = '%Y/%m/%d')
  
  # was the geodatabase updated?
  if ((active_update > active_recent) | (planning_update > planning_recent)) {
    
    # do nothing, this will result in a GH action success
    stop('BOEM WEAs NOT Updated.')
    
  } else {
    
    # set a failure and stop conditional GH action (everything is up to date)
    stop('BOEM WEAs NOT Updated.')
    
  }
  
}

# were BOEM WEAs updated?
weas_updated()
