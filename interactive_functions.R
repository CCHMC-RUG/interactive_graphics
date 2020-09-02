
ufp_hchart <- function(d, pl_colored = FALSE) {
  
  dh <- d %>% 
    arrange(Date_Time) %>% 
    mutate(char_time = as.character(Time),
           time12 = format(strptime(char_time, format='%H:%M:%S'), '%I:%M:%S %p'),
           lzt = substr(time12, 1, 1),
           lzt = str_replace(lzt, '0', ''),
           tt = substr(time12, 2, 11),
           t_ampm = paste0(lzt, tt),
           dt_ampm = paste(Date, t_ampm),
           ufp_seq = row_number(),
           dt_utc = lubridate::ymd_hms(Date_Time, tz = "UTC"), 
           c_ufp = scales::comma(UFP_conc250, accuracy = 1))
  
  x <- c("Time", "UFP")
  y <- sprintf("{point.%s}", c("t_ampm", "c_ufp"))
  tltip <- tooltip_table(x, y)
  
  ### UFP category series
  s1 <- filter(dh, ufp_cat == 1)
  s2 <- filter(dh, ufp_cat == 2)
  s3 <- filter(dh, ufp_cat == 3)
  s4 <- filter(dh, ufp_cat == 4)
  s5 <- filter(dh, ufp_cat == 5)
  
  ### function to assign NA values to empty series
  
  ufp_na_fun <- function(s) {
    r1 <- dh[1, ]
    if(nrow(s) == 0) {
      d <- bind_rows(s, r1)
      d$UFP_conc250 <- NA
      d
    } else {
      s
    }
  }
  
  s_na_list <- map(list(s1, s2, s3, s4, s5), ufp_na_fun)
  
  s1 <- s_na_list[[1]]
  s2 <- s_na_list[[2]]
  s3 <- s_na_list[[3]]
  s4 <- s_na_list[[4]]
  s5 <- s_na_list[[5]]
  
  pal <- viridis::plasma(5)
  
  # #plotlines
  pl_50 <- list(
    color = hex_to_rgba('#e6e6e6', alpha = .75), value = 5e4, width = 1)
  pl_100 <- list(
    color = hex_to_rgba('#e6e6e6', alpha = .75), value = 10e4, width = 1)
  pl_150 <- list(
    color = hex_to_rgba('#e6e6e6', alpha = .75), value = 15e4, width = 1)
  pl_200 <- list(
    color = hex_to_rgba('#e6e6e6', alpha = .75), value = 20e4, width = 1)
  pl_250 <- list(
    color = hex_to_rgba('#e6e6e6', alpha = .75), value = 25e4, width = 1)
  
  # colored plotlines 
  pl_0c <- list(
    color = hex_to_rgba(pal[1], alpha = .5), value = 0, width = 2)
  pl_25c <- list(
    color = hex_to_rgba(pal[2], alpha = .5), value = 2.5e4, width = 2)
  pl_50c <- list(
    color = hex_to_rgba(pal[3], alpha = .5), value = 5e4, width = 2)
  pl_75c <- list(
    color = hex_to_rgba(pal[4], alpha = .5), value = 7.5e4, width = 2)
  pl_100c <- list(
    color = hex_to_rgba(pal[5], alpha = .5), value = 10e4, width = 2)
  
  #plotbands
  
  pb1 <- list(
    color = hex_to_rgba(pal[1], alpha = .25), from = 0, to = 2.5e4)
  pb2 <- list(
    color = hex_to_rgba(pal[2], alpha = .25), from = 2.5e4, to = 5e4)
  pb3 <- list(
    color = hex_to_rgba(pal[3], alpha = .25), from = 5e4, to = 7.5e4)
  pb4 <- list(
    color = hex_to_rgba(pal[4], alpha = .25), from = 7.5e4, to = 1e5)
  pb5 <- list(
    color = hex_to_rgba(pal[5], alpha = .1), from = 1e5, to = 2.5e5)
  
  h <- highchart() %>%
    hc_add_series(s1, hcaes(datetime_to_timestamp(dt_utc), UFP_conc, color = ufp_pal), name = "<25K",
                  type = "scatter", showInLegend = TRUE, color = pal[1], marker = list(symbol = "circle")) %>%
    hc_add_series(s2, hcaes(datetime_to_timestamp(dt_utc), UFP_conc, color = ufp_pal), name = ">25K - 50K",
                  type = "scatter", showInLegend = TRUE, color = pal[2], marker = list(symbol = "circle")) %>%
    hc_add_series(s3, hcaes(datetime_to_timestamp(dt_utc), UFP_conc, color = ufp_pal), name = ">50K - 75K",
                  type = "scatter", showInLegend = TRUE, color = pal[3], marker = list(symbol = "circle")) %>%
    hc_add_series(s4, hcaes(datetime_to_timestamp(dt_utc), UFP_conc, color = ufp_pal), name = ">75K - 100K",
                  type = "scatter", showInLegend = TRUE, color = pal[4], marker = list(symbol = "circle")) %>%
    hc_add_series(s5, hcaes(datetime_to_timestamp(dt_utc), UFP_conc, color = ufp_pal), name = ">100K",
                  type = "scatter", showInLegend = TRUE, color = pal[5], marker = list(symbol = "circle")) %>%
    hc_tooltip(crosshairs = TRUE, headerFormat = "", useHTML = TRUE, table = TRUE,
               sort = TRUE, pointFormat = tltip) %>%  
    hc_xAxis(type = 'datetime', dateTimeLabelFormats = list(minute = '%I:%M',
                                                            hour ='%I:%M'),
             title = list(text = 'TIME',
                          offset = 35,
                          style = list(fontSize = '16px',
                                       color = 'black')),
             tickInterval = 1.8e6,
             labels = list(y = 25, x = -16)) %>% 
    hc_yAxis(max = 25e4,
             gridLineWidth = 0,
             tickInterval = 5e4,
             title = list(text = 'UFP (p/cc)',
                          offset = 60,
                          style = list(fontSize = '16px',
                                       color = 'black')),
             ceiling = 25e4) %>% 
    hc_plotOptions(series = list(states = list(inactive = list(opacity = 1))))
  
  if (pl_colored == FALSE) {
    h %>% 
      hc_xAxis(offset = 5) %>% 
      hc_yAxis(plotLines = list(pl_50, pl_100, pl_150, pl_200, pl_250))
  } else {
    h %>% 
      hc_yAxis(plotLines = list(pl_0c, pl_25c, pl_50c, pl_75c, pl_100c),
               plotBands = list(pb1, pb2, pb3, pb4, pb5))
  }
}

ufp_deck <- function(d, num) {
  
  lon_vctr <- d$lon[!is.na(d$lon)]
  lat_vctr <- d$lat[!is.na(d$lat)]
  
  if (sum(is.na(d$lat)) == nrow(d)) {
    message("No GPS data was recorded during the sampling session.") 
  } else {
    d <- pufpR::ufp_sf(d) %>% 
      filter(!is.na(ufp_pal))
    
    pal <- viridis::plasma(5)
    
    deck_legend <- legend_element(
      title = "UFP Concentration (p/cc)"
      , variables = c(" <25K", " >25K - 50K", " >50 - 75K", " >75K - 100K", " >100K")
      , colours = pal
      , colour_type = "fill"
      , variable_type = "category"
    )
    deck_legend <- mapdeck_legend(deck_legend)
    
    mapdeck(style = mapdeck_style("light"), pitch = 60) %>%
      add_column(
        data = d
        , radius = 10
        , elevation = "UFP_conc250"
        , layer_id = "hex_layer"
        , fill_colour = "ufp_pal"
        , tooltip = "UFP_conc250"
        , fill_opacity = .7
        , highlight_colour = "#00ccffff"
        , legend = deck_legend
        , auto_highlight = TRUE
        , elevation_scale = .01
        , focus_layer = TRUE
      ) %>% 
      mapdeck_view(location = c(lon_vctr[1], 
                                lat_vctr[1]),
                   zoom = 13)
  }
}

