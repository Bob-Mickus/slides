# Pre-processing ----------

edit_raw_data <- function(raw_data){
  crime_dat_edit <- raw_data %>%
    select(Date, `Primary Type`, Description, Arrest, Domestic, 
           `Location Description`, Latitude, Longitude) %>%
    rename(Crime = `Primary Type`,
           Location = `Location Description`) %>%
    mutate(datetime = mdy_hms(Date, tz = "America/Chicago"),
           Location = str_to_title(Location),
           Description = str_to_title(Description),
           Crime = str_to_title(Crime),
           Arrest = case_when(Arrest == "true" ~ "Arrest made",
                              Arrest == "false" ~ "No arrest made"),
           Domestic = case_when(Domestic == "true" ~ "Domestic",
                                Domestic == "false" ~ "Not domestic"),
           Crime = case_when(Crime %in% c("Non - Criminal", 
                                          "Non-Criminal (Subject Specified)") 
                                                ~ "Non-Criminal",
                              TRUE ~ Crime)) %>%
    filter(year(datetime) == 2016,
           !is.na(Latitude), !is.na(Longitude))
}


edit_raw_data_baseR <- function(raw_data){
  edit_data <- raw_data[ , c("Date", "Primary Type", "Description",
                               "Arrest", "Domestic", "Location Description", 
                               "Latitude", "Longitude")]
  names(edit_data)[which(names(edit_data) == "Primary Type")] <- "Crime"
  names(edit_data)[which(names(edit_data) == "Location Description")] <- "Location"
  
  edit_data$datetime <- mdy_hms(edit_data$datetime, tz = "America/Chicago")
  edit_data$Location <- str_to_title(edit_data$Location)
  edit_data$Description <- str_to_title(edit_data$Description)
  edit_data$Crime <- str_to_title(edit_data$Crime)
  edit_data$Arrest <- ifelse(edit_data$Arrest == "true", "Arrest made", "No arrest made")
  edit_data$Domestic <- ifelse(edit_data$Domestic == "true", "Domestic", "Not domestic")
  edit_data$Crime <- ifelse(edit_data$Crime %in% c("Non - Criminal", 
                                         "Non-Criminal (Subject Specified)") ,
                            "Non-Criminal", edit_data$Crime)
  
  edit_data[year(datetime) == 2016, ]
  
}


# App functions -----------

filter_crime_data <- function(crime_dat, crime, start, end, group){
  
  # filter data based on user inputs
  # give group variable generic name with 
  crime_subset <- crime_dat %>%
    filter(Crime == crime,
           datetime > start, 
           datetime < end) %>%
    rename_("Group" = group) 
  
  # Categorize only top 5 values of group variable
  top5_group <- get_top5_vals(crime_subset)

  crime_subset <- crime_subset %>%
    mutate(Group_plot = case_when(Group %in% top5_group$Group ~ Group,
                                  TRUE ~ "Other"))
  
  if("Other" %in% crime_subset$Group_plot){
    factor_levels <- rev(c(top5_group$Group, "Other"))
  } else {
    factor_levels <- rev(c(top5_group$Group))
  } 

  # Set defined colors to group plotting variable 
  colors <- color_palette(length(factor_levels))
  
  crime_subset %>%
    mutate(Group_plot = factor(Group_plot, levels = factor_levels),
           Group_plot_color = factor(Group_plot,
                                     labels = colors))
}

create_barplot_data <- function(crime_filter, 
                                time_measure,
                                north, east, south, west){
  if(is.null(north)){
    north <- 41.96766
    east <- -87.37907
    south <- 41.78872
    west <- -87.88033
  }
  
  # filter data by map boundaries
  loc_data <- crime_filter %>%
    filter(Latitude < north, Latitude > south,
           Longitude < east, Longitude > west)
  
  # create time unit variable 
  if(time_measure == "hour"){
    time_data <- loc_data %>%
      mutate(time_unit = hour(datetime) %>% factor(levels = 0:23)) 
  } else if(time_measure == "day of week"){
    time_data <- loc_data %>%
      mutate(time_unit = wday(datetime, label = TRUE)) 
  } else if(time_measure == "month"){
    time_data <- loc_data %>%
      mutate(time_unit = month(datetime, label = TRUE)) 
  }
  
  # create table with rows = time unit and cols = group value
  table_data <- table(time_data$time_unit, time_data$Group_plot)
  
  # create data frame with rows for each combination of time unit and group value
  long_data <- time_data %>%
    group_by(time_unit, Group_plot) %>%
    summarize(n = n())
  
  color_vec <- levels(crime_filter$Group_plot_color)
  names(color_vec) <- levels(crime_filter$Group_plot)
  
  list(table_data = table_data,
       long_data = long_data,
       colors = color_vec,
       time_measure = time_measure)
}

plot_barplot_by_time_loc <- function(barplot_data_list){

  # generate barplot using ggplot2 library

  barplot_data <- barplot_data_list$long_data
  color_vec <- barplot_data_list$colors
  
  ggplot(data = barplot_data, aes(x = time_unit, y = n, fill = Group_plot)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = levels(barplot_data$time_unit), drop=FALSE) +
    scale_fill_manual("", values = color_vec) + 
    xlab(barplot_data_list$time_measure) +
    ylab("Total")
  
}

## Helper functions -------------

get_top5_vals <- function(crime_subset){
  
  top5_locs <- crime_subset %>%
    filter(Group != "Other") %>%
    group_by(Group) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    filter(row_number() %in% 1:5)
  
  top5_locs
}

get_num_subclass <- function(crime_dat){
  main_crimes <- unique(crime_dat$Crime)
  
  purrr::map_df(main_crimes, function(crime){
    crime_sub <- crime_dat %>%
      filter(Crime == crime)
    num_desc <- crime_sub %>% 
      pull(Description) %>% 
      unique %>% 
      length
    num_locs <- crime_sub %>% 
      pull(Location) %>% 
      unique %>% length
    data.frame("Crime" = crime, "Num_desc" = num_desc, "Num_locs" = num_locs)
  })
  
}

color_palette <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



