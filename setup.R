### --------------------------------------------------------------- ###
## Functions

##### ------------------ F U N C T I O N  ------------------------#####
#' Get coordinates of a city per country
#'
#' Get the coordinates of a City, given the Country 2-letter code.
#' The search can be supported by region, state and/or county (optional).
#' The function uses open street maps nominatim api.
#'
#' @param City Name of the city
#' @param Country the 2-letter code of the country
#' @param Region Optional, region name. This option is NOT RECOMENDED, it's functioning is not trusted yet.
#' @param State Optional, state name
#'
#' @return Returns a 1-row data frame, containing latitude and longitude
#' 
#' @details All the variables used by the function must be strings
#' @export

coords_from_city <- function(City,
                             Country,
                             Region = NULL,
                             State = NULL){
    require('RJSONIO')
    CityCoded <- gsub(' ','%20',City) #remove space for URLs
    CountryCoded <- paste("&country=", Country, sep = '')
    if(!missing(State)){
        State <- paste("&state=", gsub(' ','%20',State), sep = '')
    }
    if(!missing(Region)){
        Region <- paste("&region=", gsub(' ','%20',Region), sep = '')
    }
    ## get data
    url <- paste(
        "http://nominatim.openstreetmap.org/search?city="
      , CityCoded
      , CountryCoded
      , State
      , Region
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    ## retrieve coords
    if(is.vector(x)){
            message(paste('Found', x[[1]]$display_name))
            lon <- x[[1]]$lon
            lat <- x[[1]]$lat
            osm_name <- x[[1]]$display_name
            coords <- data.frame('lon' = lon, 'lat' = lat, 'osm_name' = osm_name)
    }
    ## When x is not a vector
    else{
        message(paste('No results found for', City, Country))
        coords <- data.frame('lon' = NA, 'lat' = NA)
    }
    ## return a df
    coords
}



##### ------------------ F U N C T I O N  ------------------------#####
#' Retrieve coordinates from a list of countries and send it to the global db
#'
#' Given a list of cities and country(ies) in a data frame
#' the function iterates over the values to find the coordinates, using
#' the function \code{coords_from_city} and send the results to data.frame <db>
#' set as global.
#'
#' @param dat The df containing the data. It MUST contain a collumn called 'ID' with UNIQUE identification numbers starting from 1, and at least names of the cities, country and date of registration of the Org.
#' @param city String with the name of the column with the city names
#' @param country String with the name of the column with the country codes (2-letter)
#' @param state Optional. String with the name of the column with the state names
#' @param region NOT RECOMENDED. String with the name of the column with the region names
#'
#' @return Modifies the data.frame <db> providing it the ID, city name, osm_name and country code obtained for the data inside the tibble \code{dat}, together with the latitude and longitude of the city, per organization.
#'
#' @details The data must be provided as a df, the unique identification numbers must be provided in a column called "ID" that goes from 1 to nrow.
#'
#' The function uses \code{coords_from_city} for the web scrapping, therefore the values \code{state} and \code{region} are optional.
#'
#' The results of the query will be added to the dataframe ONLY when a single result is found. If more
#' than one result is found, the first entry will be added, if none are found, the details will be printed on screen and the entry will
#' not be added to the db data frame.
#'
#' @export
#'
#' 

## Probably is better to initialize db in the shiny app, to avoid erasing it on func call
db.empty <- data.frame(ID = integer(0),
                 City = character(0),
                 osm_name = character(0),
                 lon = numeric(0),
                 lat = numeric(0))
db <- db.empty
get_coords <- function(dat,
                       city = 'City',
                       country = 'Country',
                       region = NULL,
                       state = NULL){
    ##require(tidyverse)
    df_len <- length(dat[[city]])
    ## -- Iteration to web-scrap data -- ##
    ccount <- 0
    ## For loop to webscrapping
    for(i in dat$ID){
        print(paste('Entry', i))
        ## Check if the city details are already there
        if(dat[[city]][i] %in% db$City){
            ## If so, get them
            message(paste0(dat[[city]][i], " already in DB!"))
            searched.city <- subset(db, db$City == dat[[city]][i])[1,]
            coords <- data.frame(osm_name = searched.city$osm_name[1],
                                 lon = searched.city$lon[1],
                                 lat = searched.city$lat[1])
        }
        ## Else, do the webscrap
        else{
            ## Else
            if(missing(region) && missing(state)){
                coords <- coords_from_city(dat[[city]][i],
                                           dat[[country]][i])
            }
            if(!missing(region)){
                coords <- coords_from_city(dat[[city]][i],
                                           dat[[country]][i],
                                           Region = dat[[region]][i])
            }
            if(!missing(state)){
                coords <- coords_from_city(dat[[city]][i],
                                           dat[[country]][i],
                                           State = dat[[state]][i])
            }
        }
        ## DB send query ONLY if coords were found
        if(is.na(coords$lon[1])){
            ccount <- ccount + 1
        }
        else{
            ## Modify global db
            db[i,] <<- c(as.integer(dat$ID[i]),
                        dat[[city]][i],
                        coords$osm_name,
                        coords$lon[1],
                        coords$lat[1])
        }
        message(paste('Completed', (i/df_len)*100, '%'))
    }
    ## Close db
    if(ccount == 0){
        message("WEB SCRAP FOR COORDINATES SUCCESFULLY FINISHED!")
    }
    else{
        message(paste("WEB SCRAP FOR COORDINATES SEARCH FINISHED.",
                      ccount, "ENTRIES NOT FOUND"))
    }
}

##### ------------------ F U N C T I O N  ------------------------#####
#' Makes the map, per year
#'
#' 
rwd_copyright <- ('R WhiteDwarf')
rwd_copyright_color <- '#777777'
## Dots colors
rwd_dots <- '#1c2e64'#'#574166'
## Maps colors
ocean_color <- '#5e86a8'
borders_color <- '#c0c0c0'
empty_countries_fill <- '#c9c4b6'#'#a0aaca'#'#f0f0f0'
target_country_fill <- '#ffffff'
cities_text_color <- '#a0a0a0'
legend_text_color <- '#283151'
map_by_year <- function(.df,
                        year){
    require(tidyverse)
    require(maps)
    require(stringr)
    require(ggplot2)
    ## Constants based on the table
    rat <- 1.3
    country.name <- .df$Country[1]
    #col.lat <- "lat"
    #col.lon <- "lon"
    col.start.year <- "Year"
    col.cities <- "City"
    ## Constant for colors of the points
    ##rwd_dots <- rwd_dots
    ## Main data to plot
    filt <- .df  %>%
        mutate(city_name = str_to_sentence(!!ensym(col.cities)),
               lon = parse_number(lon),
               lat = parse_number(lat)) %>%
        filter(!!ensym(col.start.year) <= year) %>%
        group_by(city_name) %>%
        summarise(x = median(lon, na.rm = T),
                  y = median(lat, na.rm = T),
                  n = n()) %>%
        mutate(dot_size = ifelse(n == 1, 2.5,
                          ifelse(n >= 2 & n <= 5, 5,
                          ifelse(n >= 6 & n <= 10, 10,
                          ifelse(n >= 11 & n <= 30, 15,
                          ifelse(n >= 31 & n <= 50, 20,
                          ifelse(n >= 51 & n <= 100, 25,
                          ifelse(n >= 101 & n <= 200, 35,
                          ifelse(n >= 201 & n <= 300, 40,
                          ifelse(n >= 301, 45, NA))))))))))
    ## Total orgs per year
    total_final <- sum(filt$n)
    ## make a df with only the country to overlap
    mdata_country <- map_data('world')[map_data('world')$region == country.name,]
    ## Limits of the map
    x.limit <- c(min(mdata_country$long), max(mdata_country$long))
    y.limit <- c(min(mdata_country$lat), max(mdata_country$lat))
    x.copyright <- max(mdata_country$long) - 2
    y.copyright <-  min(mdata_country$lat) + 1
    ##################### MAIN MAP #######################
    p1 <- ggplot() +
        ## FIRST LAYER whole map, chose color of fill
        geom_polygon(data = map_data("world"),
                     aes(x=long, y=lat, group = group),
                     color = borders_color,
                     fill = empty_countries_fill,
                     alpha = 8/10) +
        ## Now choose the color of the country in this fill
        geom_polygon(data = mdata_country,
                     aes(x=long, y=lat, group = group),
                     color = borders_color,
                     fill = target_country_fill) +
        coord_map() +
        coord_fixed(1.3) +
        ## DOTS: size per value
        geom_point(data = filt,
                   aes(x, y, size = dot_size),
                   color = rwd_dots,
                   alpha = 7/10,
                   shape = 19) +
        scale_size_identity('',
                            breaks = c(2.5, 5, 10, 15, 20, 25, 35, 40),
                            labels = c(1, '2-5', '6-10', '11-30',
                                       '31-50', '51-100', '101-200', '>300'),
                            guide = guide_legend(label.position = 'bottom',
                                                 label.vjust = 0,
                                                 nrow = 1)) +
        geom_point(data = filter(filt, n == 1),
                 aes(x, y,),
                 color = rwd_dots,
                 shape = 19,
                 size = 2.5) +
        geom_point(data = filter(filt, n >= 2 & n <= 5),
                 aes(x, y,),
                 color = rwd_dots,                 
                 shape = 19,
                 size = 5) +
        ## COPYRIGHT RWD
        geom_text(aes(x = x.copyright, y = y.copyright,
               label = rwd_copyright),
                size = 3,
                family = 'Mono',
               color = rwd_copyright_color) +
        ## Themes
        theme_bw() +
        theme(legend.position = 'none',
              title = element_text(face="bold",
                                   size = 15, color = legend_text_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = ocean_color),
              axis.line = element_line(colour = "black"),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    ## Details for map cut internally
    p1 <- p1 + coord_fixed(rat, xlim = x.limit, ylim = y.limit)
    ## DEBUG
    print(filt)
    return(p1)
}



### -------------------------------------------------------------- ###
## Create testing data
t.dat <- data.frame(City = c("Ciudad de Mexico",
                             "Tijuana",
                             "Ciudad de Mexico",
                             "Guadalajara",
                             "Queretaro",
                             "Tijuana",
                             "Ciudad de Mexico",
                             "Cuernavaca",
                             "Ciudad de Mexico",
                             "Texcoco",
                             "Ciudad de Mexico",
                             "Ciudad de Mexico"),
                    Country = "Mexico",
                    Region = c("Mexico",
                               "Baja California Norte",
                               "Mexico",
                               "Jalisco",
                               "Queretaro",
                               "Baja California Norte",
                               "Mexico",
                               "Morelos",
                               "Mexico",
                               "Estado de Mexico",
                               "Mexico",
                               "Mexico"),
                    Year = c(1999:2010))

## CODE FOR INTERNAL TESTING

## Combine DB with table with the data
#t.dat$ID <- 1:nrow(t.dat)
#get_coords(t.dat)

#t.dat$ID <- as.character(t.dat$ID)
#map.df <- dplyr::left_join(t.dat, db, by = c('ID', 'City'))

#map_by_year(map.df, max(map.df$Year))



## TO-DO
## [ ] When more than 1 entry found, allow the user to choose
## [ ] Allow user add coords manually
## [ ] Low priority: Allow the user download resulted table
