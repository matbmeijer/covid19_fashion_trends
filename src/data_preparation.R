############################# Pushing it to Github #############################
# in RStudio, click on the "Tools" menu and select "Shell"
# Run the following command: git push -u origin master
# it might ask you for your git username and password. Supply this information, make sure it is correct


############################ Functional Parameters #############################

# Environment
debug<-FALSE
update_time<-FALSE


end_date <- as.Date("2020-04-19")
start_date <- end_date - 3*365
covid19_deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
fashion_categories_url <- "./input/label_descriptions.json"
internet_usage_url <- "http://api.worldbank.org/v2/en/indicator/IT.NET.USER.ZS?downloadformat=csv"

############################## System Parameters ###############################

# Knitr parameters
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

################################## Libraries ###################################

library("jsonlite")
library("tidyr")
library("dplyr")
library("wbstats")
library("janitor")
library("tidyr")
library("gtrendsR")
library("ggplot2")
library("stringr")

################################## Functions ###################################

# Concatonate formula
conc<-function(x, y=",", decr=FALSE, unique=TRUE){
  x<-sort(unique(x), decreasing = decr)
  return(paste0(x, collapse = y))
}

# Divider function for formatting
div<-function(x, n=80){
  if(nchar(x)>0){
    x<-paste0(" ", paste(trimws(x), collapse = " "), " ")
  }
  m<-floor((n-nchar(x))/2)
  ret<-paste0(paste0(rep("#", m), collapse = ""), x, paste0(rep("#", n-m-nchar(x)), collapse = ""))
  clipr::write_clip(ret)
  return(cat(ret))
}

# Write Excel file
write_excel <-function(df, filename, directory=NULL, date=NULL, verbose=TRUE){
  hs <- openxlsx::createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12, fontName="Arial Narrow", fgFill = "#4F80BD")
  if(is.null(date)){
    date<-as.character(format(Sys.time(),"%Y%m%d"))
  }else{
    date<-as.character(date)
  }
  if(is.null(directory)&&dirname(filename)=="."){
    directory<-here::here()
  }else if(dirname(filename)!="."){
    directory<-dirname(filename)
  }
  
  filename<-basename(filename)
  if(!grepl(".xlsx$", filename)){
    filename<-paste0(date,"_", paste0(trimws(gsub("\\.XLS$|\\.XLSX$|\\.xls$","",filename)),".xlsx"))
  }else{
    filename<-paste0(date,"_", paste0(trimws(gsub("\\.XLS$|\\.XLSX$|\\.xls$","",filename))))
  }
  file<-file.path(directory, filename)
  openxlsx::write.xlsx(df,
                       file = file,
                       gridLines=F,
                       tabColour="blue",
                       zoom=80,
                       col.names=TRUE,
                       row.names=FALSE,
                       colWidths="auto",
                       borders = "columns",
                       headerStyle = hs)
  if(verbose){
    message("File created at", file)
  }
}

################################################################################
################################### Process ####################################
################################################################################

############################# Get CoronaVirus data #############################

# Read data and gather columnwise,reformat date, group by country
covid_L0 <- read.csv(covid19_deaths_url, stringsAsFactors = FALSE) %>% 
  clean_names() %>%
  gather(key="date", value="deaths", starts_with("x")) %>%
  group_by(country_region, date) %>% 
  summarise(deaths=sum(deaths, na.rm = TRUE)) %>%
  ungroup()

# Obtain countrycodes and remove non identified countries, reorder data
covid_L1<-covid_L0 %>% mutate(date=as.Date(date, "x%m_%d_%y"),
                              country_code=countrycode::countrycode(country_region, origin = 'country.name',destination = 'iso2c', warn = FALSE)) %>%
  filter(!is.na(country_code)) %>%
  select(country_region, country_code, date, deaths)

# Define event 0 as moment with 100 deaths, keep only some countries
event_L0<-covid_L1 %>% group_by(country_code) %>%
  filter(deaths>=100) %>%
  filter(date==min(date)) %>% 
  ungroup() %>%
  select(-deaths) %>%
  rename("covid_19_date_0"="date")

# Keep only countries with a large internet penetration

internet_usage_L0 <- wbstats::wb(indicator = "IT.NET.USER.ZS", startdate = 2010, enddate = 2020)
internet_usage_L1 <- internet_usage_L0 %>% 
  mutate(date=as.integer(date)) %>%
  group_by(iso3c) %>%
  filter(date==max(date)) %>%
  ungroup() %>%
  rename("date_internet_usage"="date",
         "internet_usage"="value") %>%
  select(iso2c, date_internet_usage, internet_usage)

# Keep only countries where at least 50% population uses internet
event_L1 <- event_L0 %>% 
  left_join(internet_usage_L1, by=c("country_code"="iso2c")) %>%
  filter(internet_usage>50)

################## Define COVID affected countries to analyze ##################
country_info_L0 <- event_L1 %>% distinct(country_region, country_code) %>% mutate(id=1)

############################ Get fashion categories ############################

fashion_categories_L0 <- jsonlite::fromJSON(fashion_categories_url)$categories

fashion_categories_L1 <- fashion_categories_L0 %>% 
  select(name) %>% 
  mutate(name = strsplit(as.character(name), ",")) %>%
  unnest(name) %>%
  mutate(name=stringr::str_trim(name),
         id=1)

########################## Bring everything together ###########################

analysis_data_L0 <- fashion_categories_L1 %>% 
  full_join(country_info_L0, by="id") %>% 
  select(-id) %>%
  mutate(start_date=start_date,
         end_date=end_date) %>%
  left_join(event_L0, by=c("country_region", "country_code"))


