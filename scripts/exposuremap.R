library(rvest)
library(data.table)
library(ggmap)
library(magrittr)
library(leaflet)

Sys.setenv(http_proxy = "http://govnext-proxy.finance.wa.gov.au:80")
Sys.setenv(https_proxy = "http://govnext-proxy.finance.wa.gov.au:80")

url <- 'https://www.wa.gov.au/government/covid-19-coronavirus/covid-19-coronavirus-locations-visited-confirmed-cases'

webpage <- read_html(url)

# Date updated
last_update <- html_nodes(webpage, 'p')[1] %>% as.character()
last_update <- gsub('<p>','',last_update)
last_update <- gsub('</p>','',last_update)

# Sites
sites <- html_table(webpage)[[1]] %>% as.data.table()

# Clean up Data
setnames(sites, c('Date_Time', 'Suburb', 'Location', 'Date_updated', 'Health_advice'))
sites[,Date_updated := as.Date(Date_updated, '%d/%m/%Y')]

## Modify Date_Time
sites_Date_Time <- sapply(sites[,Date_Time], function(x) strsplit(x, '\n\t\t\t'))

## Modify Health advice
Health_Advice_Mapping <- data.table(
  Health_Advice_Full = c('Get tested immediately and isolate for 14 days from the date of last exposure, unless directly advised otherwise by the Department of Health. If you have not been contacted by the Department of Health phone 13 COVID (13 26843).',
                         'Get tested immediately and isolate until you receive a negative result, unless directly advised otherwise by the Department of Health.', 
                         'Get tested immediately. Unless symptomatic, you are not required to isolate while awaiting negative result.', 
                         'Monitor for symptoms unless directly advised otherwise by the Department of Health. If symptoms develop, get tested and isolate.'),
  Health_Advice_Short = c('Test and Iso for 14d', 
                          'Test and Iso until negative',
                          'Test and Iso if symptomatic',
                          'Monitor for symptoms'),
  Marker_Color = c('black', 'red', 'orange', 'blue'),
  Health_Advice_Severity = c(4:1)
)
fwrite(Health_Advice_Mapping, 'data/Health_Advice_Mapping.csv')

sites <- merge(sites, Health_Advice_Mapping, by.x = 'Health_advice', by.y = 'Health_Advice_Full', all.x = TRUE, sort = FALSE) 

## Modify Location Data
# sites_Location <- sapply(sites[,Location], function(x) strsplit(x, '\n\t\t\t'))
# sites[, Location_Description := sapply(sites_Location, function(x) {
#   if(length(x) == 2) {
#     x[1]
#   } else {
#     paste0(x[1], '\n', x[2])
#   }
# })]
# sites[, Location_Address := sapply(sites_Location, function(x) x[length(x)])]
# sites[, Location := NULL]
setnames(sites, 'Suburb', 'Location_Suburb')


sites[,.N, keyby = Suburb]

# Geocode location
register_google(key = Sys.getenv('google_api_key_maps'))
sites[, Merged_Location := paste0(Location, ', ', Suburb, ', Western Australia')]
geocoded_Location <- lapply(sites[, unique(Merged_Location)], function(x) {
  geocode(x, output = "latlona", source = "google") %>% as.data.table()
})
geocoded_Location <- lapply(geocoded_Location, function(x) {
  x[,c(1,2)]
}) %>% rbindlist()
geocoded_Location <- cbind(data.table(Merged_Location = sites[, unique(Merged_Location)]), geocoded_Location)

geocoded_Location[grep('Winthrop Village Shopping Centre', Merged_Location), c('lon', 'lat') := list(115.8317998827398, -32.05857861019292)]
geocoded_Location[intersect(grep("Coles", Merged_Location), grep('Gladstone Rd & Marlboro Avenue', Merged_Location)), c('lon', 'lat') := list(116.04676596739527, -31.888049830385764)]
geocoded_Location[intersect(grep("Bakers Delight", Merged_Location), grep('Gladstone Rd & Marlboro Avenue', Merged_Location)), c('lon', 'lat') := list(116.04753799538145, -31.885066103198028)]
geocoded_Location[grep('Settlers Tavern', Merged_Location), c('lon', 'lat') := list(115.07362499812572, -33.949563184759185)]
geocoded_Location[grep('P&N Bank Cockburn Gateways', Merged_Location), c('lon', 'lat') := list(115.85806722691746, -32.13224877511291)]
geocoded_Location[intersect(grep("H&M", Merged_Location), grep('3 Forrest Place', Merged_Location)), c('lon', 'lat') := list(115.85897625390224, -31.95209711832687)]
geocoded_Location[grep('Smokemart & GiftBox & Vape Square - Warwick Grove', Merged_Location), c('lon', 'lat') := list(115.81049222691064, -31.844062559298845)]

geocoded_Location[grep('Bus route',Merged_Location), c('lon', 'lat') := list(NA, NA)]
geocoded_Location[grep('Bus Route',Merged_Location), c('lon', 'lat') := list(NA, NA)]
geocoded_Location[grep('train ride',Merged_Location), c('lon', 'lat') := list(NA, NA)]
geocoded_Location[grep('Train ride',Merged_Location), c('lon', 'lat') := list(NA, NA)]

fwrite(geocoded_Location, 'data/Geocoded_Location.csv')

# sites[, lat := NULL]
# sites[, lon := NULL]

sites <- merge(sites, geocoded_Location, by = 'Merged_Location', all.x = TRUE, sort = FALSE)

sites[, Merged_Location := NULL]

addAwesomeMarkers_for_Marker_Color <- function(map, color) {
  data.filt <- sites[!is.na(lon) & Marker_Color == color, .(Date_Time, Location, Suburb, Date_updated, Health_Advice_Short, Marker_Color, lon, lat)]
  labels <- data.filt[, paste0(
    "<strong>Exposure Date & Time</strong><br/>", Date_Time,
    "<br/><strong>Location</strong><br/>", Location,
    "<br/><strong>Suburb</strong><br/>", Suburb,
    "<br/><strong>Date Updated</strong><br/>", Date_updated,
    "<br/><strong>Health Advice</strong><br/>", Health_Advice_Short)]
  labels <- gsub('\t', '', labels)
  labels <- gsub('\n\n', '<br/>', labels)
  labels <- gsub('\n', '<br/>', labels)
  labels <- gsub('<br/>.<br/>', '<br/>', labels)
  labels <- labels %>% lapply(htmltools::HTML)
  
  addAwesomeMarkers(
    map,
    data = data.filt,
    group = Health_Advice_Mapping[Marker_Color == color, Health_Advice_Short],
    lng = ~lon,
    lat = ~lat,
    icon = ~awesomeIcons(
      icon = 'ios-close',
      library = 'ion',
      markerColor = Marker_Color
    ),
    label = labels
  )
}

leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers_for_Marker_Color('black') %>%
  addAwesomeMarkers_for_Marker_Color('red') %>%
  addAwesomeMarkers_for_Marker_Color('yellow') %>%
  addAwesomeMarkers_for_Marker_Color('blue') %>%
  addLayersControl(
    overlayGroups = Health_Advice_Mapping[, Health_Advice_Short],
    options = layersControlOptions(collapsed = FALSE)
  )

sites_Date_Time <- sites[,Date_Time]
sites_Date_Time <- gsub('\t', '', sites_Date_Time)
sites_Date_Time <- gsub('\n.\n', '\n', sites_Date_Time)
sites_Date_Time <- gsub('\n\n', '\n', sites_Date_Time)
sites_Date_Time <- gsub('\n', ' ', sites_Date_Time)
sites_Date_Time <- gsub('-', 'to', sites_Date_Time)

sites_Date_Time_By_Day <- lapply(sites_Date_Time, function(x){
  x <- gsub('/', '-', x)
  x_split <- str_split(x, ' ')[[1]]
  x_split <- x_split[grepl("[0-9]{2}-[0-9]{2}-[0-9]{2}", x_split)]
  x_split <- sapply(x_split, function(x_date) {
    x_date <- gsub(' ', '', x_date)
    x_date <- gsub(' ', '', x_date)
    for(Day in c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) {
      if(grep(Day, x_date) %>% length() > 0) {
        x_date <- substr(x_date, nchar(Day) + 2, nchar(x_date))
      }
    }
    if(grep('at', x_date) %>% length() > 0) {
      x_date <- substr(x_date, 1, nchar(x_date) - nchar('at') - 1)
    }
    x_date <- gsub('at', '', x_date)
    if(nchar(x_date) == 10) {
      as.Date(x_date, format = '%d-%m-%Y')
    } else if(nchar(x_date) == 8) {
      as.Date(x_date, format = '%d-%m-%y')
    }
  })
  if(length(x_split) > 1) {
    seq(head(x_split,1), tail(x_split,1), 1) %>% as.Date(format = '%Y-%m-%d', origin = '1970-01-01')
  } else {
    x_split %>% as.Date(format = '%Y-%m-%d', origin = '1970-01-01')
  }
})

sites_by_day <- lapply(1:length(sites_Date_Time_By_Day), function(i) {
  output <- cbind(
    data.table(
      Date_Time_Day = sites_Date_Time_By_Day[[i]]
    ),
    sites[rep(i, length(sites_Date_Time_By_Day[[i]]))]
  )
}) %>% rbindlist()

sites_by_day[, start := Date_Time_Day]
sites_by_day[, end := Date_Time_Day + 1]


library(leaftime)

leaflet() %>%
  addTiles() %>%
  addTimeslider(data = sites_by_day)

labels <- sites_by_day[, paste0(
  "<strong>Exposure Date & Time</strong><br/>", Date_Time,
  "<br/><strong>Location</strong><br/>", Location,
  "<br/><strong>Suburb</strong><br/>", Suburb,
  "<br/><strong>Date Updated</strong><br/>", Date_updated,
  "<br/><strong>Health Advice</strong><br/>", Health_Advice_Short)]
labels <- gsub('\t', '', labels)
labels <- gsub('\n\n', '<br/>', labels)
labels <- gsub('\n', '<br/>', labels)
labels <- gsub('<br/>.<br/>', '<br/>', labels)
labels <- paste0('<div>', labels, '</div>')
labels <- labels %>% sapply(htmltools::HTML)
sites_by_day[, label := labels]
shared_sites_by_day <- SharedData$new(sites_by_day[!is.na(lon)])

bscols(widths = c(3,NA),
       list(
         # filter_checkbox("cyl", "Cylinders", shared_sites_by_day, ~cyl, inline = TRUE),
         filter_slider("date", "Date", shared_sites_by_day, ~Date_Time_Day, step = 1, animate = TRUE, width = "100%")#,
         # filter_select("auto", "Automatic", shared_sites_by_day, ~ifelse(am == 0, "Yes", "No"))
       ),
       leaflet(width = '100%', height = 800) %>%
         addTiles() %>%
         addAwesomeMarkers(
           data = shared_sites_by_day,
           group = ~Health_Advice_Short,
           lng = ~lon,
           lat = ~lat,
           icon = awesomeIcons(
             icon = 'ios-close',
             library = 'ion',
             markerColor = ~Marker_Color
           ),
           label = ~label
         )
)

leaflet(sites[!is.na(lon)]) %>%
  addTiles() %>%
  leaflet.extras::addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~Health_Advice_Severity/4,
    radius = 15,
    blur = 15,
    group = heatmap
  )


leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers_for_Marker_Color('black') %>%
  addAwesomeMarkers_for_Marker_Color('red') %>%
  addAwesomeMarkers_for_Marker_Color('yellow') %>%
  addAwesomeMarkers_for_Marker_Color('blue') %>%
  leaflet.extras::addHeatmap(
    data = sites[!is.na(lon)],
    lng = ~lon,
    lat = ~lat,
    intensity = ~Health_Advice_Severity/4,
    radius = 15,
    blur = 15,
    group = 'Heat Map'
  ) %>%
  leaflet.extras::addHeatmap(
    data = sites[1],
    lng = ~lon,
    lat = ~lat,
    intensity = NA,
    radius = 0,
    blur = 0,
    group = 'No Heat Map'
  ) %>%
  addLayersControl(
    baseGroups = c('Heat Map', 'No Heat Map'),
    overlayGroups = c(Health_Advice_Mapping[, Health_Advice_Short]),
    options = layersControlOptions(collapsed = FALSE)
  )
