library(rvest)

# Sys.setenv(http_proxy = "http://govnext-proxy.finance.wa.gov.au:80")
# Sys.setenv(https_proxy = "http://govnext-proxy.finance.wa.gov.au:80")

url <- 'https://www.wa.gov.au/government/covid-19-coronavirus/covid-19-coronavirus-locations-visited-confirmed-cases'

webpage <- read_html(url)

# folder <- 'C:/Users/zdtf0342/OneDrive - Treasury WA/COVID/'
# folder <- 'C:/Users/pli/Documents/COVID/'
# folder <- 'C:/Users/zdtf0342/Documents/wacovid/'
folder <- Sys.getenv('WACOVID-FOLDER')

# Date updated
previous_update <- readLines(paste0(folder, 'data/Last_Updated.txt'))
current_update <- html_nodes(webpage, 'p')[1]
current_update <- as.character(current_update)
current_update <- gsub('<p>','',current_update)
current_update <- gsub('</p>','',current_update)

if(current_update != previous_update) {
  library(data.table)
  library(ggmap)
  library(magrittr)
  library(leaflet)
  library(stringr)
  library(leaflet.extras)
  library(git2r)
  
  # Sites
  sites_test <- html_table(webpage)[[1]] %>% as.data.table()
  sites_monitor <- html_table(webpage)[[2]] %>% as.data.table() %>% .[,-c('')]
  sites_monitor[`Exposure date & time` == 'Friday 28/01/2022\n\t\t\t2:15pm to 2:30pm', `Health advice` := `Date updated`]
  sites_monitor[`Exposure date & time` == 'Friday 28/01/2022\n\t\t\t2:15pm to 2:30pm', `Date updated` := `Location`]
  sites_monitor[`Exposure date & time` == 'Friday 28/01/2022\n\t\t\t2:15pm to 2:30pm', `Location` := `Suburb`]
  sites_monitor[`Exposure date & time` == 'Friday 28/01/2022\n\t\t\t2:15pm to 2:30pm', `Suburb` := 'Eaton']
  
  sites <- rbind(sites_test, sites_monitor)
  
  # Clean up Data
  setnames(sites, c('Date_Time', 'Suburb', 'Location', 'Date_updated', 'Health_advice'))
  sites[,Date_updated := as.Date(Date_updated, '%d/%m/%Y')]
  sites[, Date_Time := gsub('/1/2022,', '/01/2022', Date_Time)]
  sites[, Date_Time := gsub('/01/2022,', '/01/2022', Date_Time)]
  sites[Date_updated == '2022-01-28', Date_Time := gsub('02/2022', '01/2022', Date_Time)]
  
  setorder(sites, -Date_updated)
  
  ## Modify Date_Time
  sites_Date_Time <- sapply(sites[,Date_Time], function(x) strsplit(x, '\n\t\t\t'))
  
  # Health Advice
  Health_Advice_Mapping <- fread(paste0(folder,'data/Health_Advice_Mapping.csv'))
  sites <- merge(sites, Health_Advice_Mapping, by.x = 'Health_advice', by.y = 'Health_Advice_Full', all.x = TRUE, sort = FALSE) 
  
  # Geocode Location
  register_google(key = Sys.getenv('google_api_key_maps'))
  geocoded_Location <- fread(paste0(folder,'data/Geocoded_Location.csv'), encoding = 'UTF-8')
  sites[, Merged_Location := paste0(Location, ', ', Suburb, ', Western Australia')]
  
  new_Merged_location <- sites[!(Merged_Location %in% geocoded_Location$Merged_Location), unique(Merged_Location)]
  new_geocoded_Location <- lapply(new_Merged_location, function(x) {
    geocode(x, output = "latlona", source = "google") %>% as.data.table()
  })
  new_geocoded_Location <- lapply(new_geocoded_Location, function(x) {
    x[,c(1,2)]
  }) %>% rbindlist()
  new_geocoded_Location <- cbind(data.table(Merged_Location = new_Merged_location), new_geocoded_Location)
  
  if(new_geocoded_Location[,.N] != 0) {
    geocoded_Location <- rbind(new_geocoded_Location, geocoded_Location)
  }
  fwrite(geocoded_Location, paste0(folder,'data/Geocoded_Location.csv'))
  
  sites <- merge(sites, geocoded_Location, by = 'Merged_Location', all.x = TRUE, sort = FALSE)
  sites[, Merged_Location := NULL]
  
  fwrite(sites[, .(Date_Time, Location, Suburb, Date_updated, Health_advice, Health_Advice_Short, Health_Advice_Severity, Marker_Color, lon, lat)],
         paste0(folder, 'data/Sites.csv'))
  
  # By Date and Time by Day
  sites_Date_Time <- sites[,Date_Time]
  sites_Date_Time <- gsub('\t', '', sites_Date_Time)
  sites_Date_Time <- gsub('\n.\n', '\n', sites_Date_Time)
  sites_Date_Time <- gsub('\n\n', '\n', sites_Date_Time)
  sites_Date_Time <- gsub('\n', ' ', sites_Date_Time)
  sites_Date_Time <- gsub('-', 'to', sites_Date_Time)
  
  sites_Date_Time_By_Day <- lapply(sites_Date_Time, function(x){
    print(x)
    x <- gsub('/', '-', x)
    x_split <- str_split(x, ' ')[[1]]
    x_split <- x_split[grepl("[0-9]{1}-[0-9]{2}-[0-9]{2}", x_split)]
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
      } else if(nchar(x_date) == 9) {
        as.Date(paste0('0', x_date), format = '%d-%m-%Y')
      }
    })
    x_split <- x_split[order(x_split)]
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
  
  sites_by_day[Date_Time_Day == '2021-01-04', Date_Time_Day := as.Date('2022-01-04')]
  
  fwrite(sites_by_day[, .(Date_Time_Day, Date_Time, Location, Suburb, Date_updated, Health_advice, Health_Advice_Short, Marker_Color, lon, lat)],
         paste0(folder, 'data/Sites_by_Day.csv'))
  
  # sites_by_day[, start := Date_Time_Day]
  # sites_by_day[, end := Date_Time_Day + 1]
  
  writeLines(current_update, paste0(folder, 'data/Last_Updated.txt'))
  
  # Run Dashboard
  rmarkdown::render(
    paste0(folder, "scripts/dashboard.Rmd"),
    output_file = paste0(folder, "dashboard.html")
  )
  
  repo <- repository(folder)
  config(repo, user.name="Peter Li", user.email="pli@westswan.com")
  commit(repo, message = paste0('Updated on ', substr(current_update, 15, nchar(current_update))), all = TRUE)
  push(repo, credentials = cred_token(token = "GITHUB_PAT"))
}
