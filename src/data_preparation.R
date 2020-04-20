############################# Pushing it to Github #############################
# in RStudio, click on the "Tools" menu and select "Shell"
# Run the following command: git push -u origin master
# it might ask you for your git username and password. Supply this information, make sure it is correct


############################ Functional Parameters #############################

# Environment

update_data <- FALSE
update_time <- FALSE

if(!udpate_time){
  end_date <- as.Date("2020-04-19")
} else {
  end_date <- Sys.Date()
}

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
library("stringr")
library("data.table")
library("prophet")
library("ggplot2")
library("plotly")

################################## Functions ###################################

# custom color palette
color_palette_v2<-function(){
  return(c(rgb(255, 51, 30, maxColorValue = 255),
           rgb(128,128,128, maxColorValue = 255),
           rgb(0, 167, 0,  maxColorValue = 255),
           rgb(54, 37, 167, maxColorValue = 255),
           rgb(255, 212, 0, maxColorValue = 255),
           rgb(101, 125, 212, maxColorValue = 255),
           rgb(217, 36, 104, maxColorValue = 255),
           rgb(8, 126, 139, maxColorValue = 255),
           rgb(211, 211, 211, maxColorValue = 255),
           rgb(0,0,255, maxColorValue = 255),
           rgb(138,43,226, maxColorValue = 255))
  )
}


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
if(update_data){
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
  
  # Define event 0 as moment with 100 deaths, keep only countries where covid19 has hit for at least two weeks
  event_L0<-covid_L1 %>% group_by(country_code) %>%
    filter(deaths>=100) %>%
    filter(date==min(date)) %>% 
    ungroup() %>%
    select(-deaths) %>%
    rename("covid_19_date_0"="date") %>%
    filter(covid_19_date_0<(end_date-14))
  
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
  
  #Remove categories that reflect more attributes
  categories_to_remove <- 
    c("lapel",
      "epaulette",
      "sleeve",
      "pocket",
      "neckline",
      "buckle",
      "bead",
      "zipper",
      "applique",
      "bow",
      "flower",
      "fringe",
      "ribbon",
      "rivet",
      "ruffle",
      "sequin",
      "tassel")
  
  fashion_categories_L2 <- fashion_categories_L1 %>% filter(!name %in% categories_to_remove)
  
  ########################## Bring everything together ###########################
  
  # Look which csv files have been downloaded
  downloaded_files<-dir(pattern = "csv", recursive = TRUE, full.names = TRUE)
  downloaded_files<-downloaded_files[grepl("/output/", downloaded_files)]
  
  gtrends_L0 <- fashion_categories_L2 %>% 
    full_join(country_info_L0, by="id") %>% 
    select(-id) %>%
    mutate(start_date=start_date,
           end_date=end_date,
           gtrends_range = sprintf("%s %s",start_date, end_date)) %>%
    left_join(event_L0, by=c("country_region", "country_code")) %>%
    mutate(file_name = sprintf("./output/%s_%s.csv", country_code, name),
           downloaded = file_name %in% downloaded_files)
  
  to_download_L0 <- gtrends_L0 %>% filter(!downloaded)
  
  
  ####################### Download Data from Google Trends #######################
  
  for(i in seq_along(to_download_L0[[1]])){
    
    print(to_download_L0$file_name[i])
    
    res_L0<-try(gtrendsR::gtrends(keyword = to_download_L0$name[i],
                                  time = to_download_L0$gtrends_range[i],
                                  geo = to_download_L0$country_code[i],
                                  onlyInterest = TRUE))
    if(class(res_L0) == "try-error") {
      stop("Limit has been reached", call. = FALSE)
    }
    write.csv(res_L0$interest_over_time, to_download_L0$file_name[i])
    Sys.sleep(2)
  }
  
  
  ################################ Bind all data #################################
  
  downloaded_files<-dir(pattern = "csv", recursive = TRUE, full.names = TRUE)
  downloaded_files<-downloaded_files[grepl("/output/", downloaded_files)]
  
  # Remove summary files
  downloaded_files<-downloaded_files[!grepl("expected_vs_covid_google_searches", downloaded_files)]
  
  # Keep only non-empty tables
  downloaded_files<-downloaded_files[file.size(downloaded_files)>3]
  df_list<-lapply(downloaded_files, read.csv, stringsAsFactors = FALSE)
  df_L0<-data.table::rbindlist(df_list)
  
  # Delete list
  rm(df_list)
  
  # Remove unnecessary data
  df_L1 <- df_L0 %>%
    select(-c("gprop", "category", "time", "X")) %>%
    mutate(date=as.Date(date))
  
  ############################ Attach COVID-19 dates #############################
  df_L2 <- event_L1 %>% 
    select(country_region, country_code, covid_19_date_0) %>%
    inner_join(df_L1, by=c("country_code"="geo"))
  
  # Check if any NA
  any(is.na(df_L2))
  
  # Before COVID-19 100 deaths and after COVID-19 100 deaths dummy, remove low observations
  df_L3 <- df_L2 %>% 
    mutate(covid19_situation = case_when(date < (covid_19_date_0-7) ~ "Before 100 deaths caused by COVID-19",
                                         date >= (covid_19_date_0-7) ~ "After 100 deaths caused by COVID-19")) %>%
    group_by(country_code, keyword) %>%
    mutate(hits_median=median(hits)) %>%
    ungroup() %>%
    filter(hits_median>10) %>%
    select(-hits_median)
  
  
  ######################### Data to predict for Prophet ##########################
  prophet_L0 <- df_L3 %>% filter(covid19_situation == "Before 100 deaths caused by COVID-19")
  
  # Define periods to predict
  prophet_L1 <- prophet_L0 %>%
    group_by(country_code) %>%
    mutate(periods=as.numeric((end_date-7-max(date))/7)) %>%
    ungroup() %>%
    rename("ds"="date", "y"="hits")
  
  # Function to run prophet on each group
  prophet_modelling<-function(x) {
    df <- x[,c("ds", "y")]
    periods<-max(x$periods)
    m <- prophet::prophet(df, weekly.seasonality = TRUE)
    future <- make_future_dataframe(m, periods = periods, freq = "week") %>% top_n(periods, ds)
    forecast <- predict(m, future) %>% 
      select(ds, yhat) %>% 
      rename("y"="yhat") %>%
      mutate(country_region = x$country_region[1],
             country_code = x$country_code[1],
             covid_19_date_0 = x$covid_19_date_0[1],
             keyword = x$keyword[1],
             periods = periods,
             covid19_situation= "Expected behaviour without COVID-19") %>%
      select(country_region,
             country_code,
             covid_19_date_0,
             ds,
             y,
             keyword,
             covid19_situation,
             periods)
    
    y<-rbind(x, forecast)
    rm(x, forecast, m, periods)
    return(y)
  }
  
  # Loop through predictions
  prophet_L2 <- prophet_L1 %>%
    group_by(country_code, keyword) %>%
    do(prophet_modelling(.)) %>% ungroup()
  
  prophet_L2$periods<-NULL
  
  
  # Bring everything together
  df_L4 <- df_L3 %>% 
    filter(covid19_situation=="After 100 deaths caused by COVID-19") %>%
    rename("ds"="date", "y"="hits") %>%
    union_all(prophet_L2) %>%
    mutate(covid19_situation = case_when(covid19_situation == "After 100 deaths caused by COVID-19" ~ "Consumer behaviour after 100 deaths in home country caused by COVID-19",
                                         covid19_situation == "Before 100 deaths caused by COVID-19" ~ "Consumer behaviour before COVID-19 in home country",
                                         covid19_situation == "Expected behaviour without COVID-19" ~ "Expected consumer behaviour without COVID-19"),
           keyword=stringr::str_to_title(keyword)) %>%
    rename("Date"="ds",
           "Google Search Value"="y",
           "Fashion Category"="keyword",
           "Country"="country_region",
           "Country Code"="country_code",
          "COVID-19 Situation"  = "covid19_situation",
          "COVID-19 Date 0" = "covid_19_date_0")
    
  # Save summary
  fwrite(df_L4, file = "./output/summary/expected_vs_covid_google_searches.csv")
} else {
  df_L4 <- read.csv(file = "./output/summary/expected_vs_covid_google_searches.csv", stringsAsFactors = FALSE, check.names = FALSE)
}

############################# Plotting countrywise #############################
df_L5 <- df_L4 %>%
  mutate(Date = as.Date(Date),
         covid_19_date_0 = as.Date(covid_19_date_0)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    `COVID-19 Situation`=gsub("Consumer behaviour after 100 deaths in home country caused by COVID-19",
                              "Consumer behaviour after 100 deaths\nin home country caused by COVID-19", 
                              `COVID-19 Situation`),
    `COVID-19 Situation`=gsub("Consumer behaviour before COVID-19 in home country",
                              "Consumer behaviour before\nCOVID-19 in home country", 
                              `COVID-19 Situation`),
    `COVID-19 Situation`=gsub("Expected consumer behaviour without COVID-19",
                              "Expected consumer\nbehaviour without COVID-19", 
                              `COVID-19 Situation`))


############################# Plot full time line ##############################
country_codes <- unique(df_L5$`Country Code`)

for(i in country_codes){
  plot_df_L0 <- df_L5 %>% filter(`Country Code` == i)
  plot_L0<-plot_df_L0 %>% ggplot(aes(x=Date, y=`Google Search Value`, color=`COVID-19 Situation`)) + 
    geom_line(size=0.6, alpha=0.8) + 
    geom_vline(xintercept=(plot_df_L0$covid_19_date_0[1]-7), linetype="dotted") + 
    facet_wrap(~`Fashion Category`) + 
    theme_minimal() +
    labs(title = sprintf("Google Searches for different fashion categories in %s", plot_df_L0$Country[1]),
         color = "COVID-19 situation") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45)) +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=1))) +
    scale_color_manual(values=color_palette_v2())
  ggsave(filename = sprintf("./output/plots/%s_plot1.png", i), plot_L0, width = 15)
  
  ################################ Plot last year ################################
  plot_L1<-plot_df_L0 %>% 
    filter(year(Date)==2020) %>%
    ggplot(aes(x=Date, y=`Google Search Value`, color=`COVID-19 Situation`)) + 
    geom_line(size=0.6, alpha=0.8) + 
    geom_vline(xintercept=(plot_df_L0$covid_19_date_0[1]-7), linetype="dotted") + 
    facet_wrap(~`Fashion Category`) +
    theme_minimal() +
    labs(title = sprintf("Google Searches for different fashion categories in %s", plot_df_L0$Country[1]),
         color = "COVID-19 situation") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45)) +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=1))) +
    scale_color_manual(values=color_palette_v2())
  ggsave(filename = sprintf("./output/plots/%s_plot2.png", i), plot_L1, width = 15)
}  
