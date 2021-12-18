library(shiny)
library(rhandsontable)
source('setup.R')

ui = shinyUI(fluidPage(
    ## CSS
    tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
         ),

    ## Title
    div(class = 'head',
        titlePanel("Dynamic maps of organizations per country"),
        p('Provided a list of cities, country and year of opening/inaguration, create dynamic maps based on selected year.'),
        h3('Instructions'),
        HTML('<p>
Provide to the table a list of cities and year of opening/inauguration as if each line is one organization. Then click <b>Get coords</b> and once the script has finished (see the log below) click <b>Make map</b>. Once the map is shown you can slide the bar above it to select the year to visualize. You can select an example and <b>Load</b> it to see how it should look like.<br> 
NOTE: For now only one country per map is supported; you can erase everything using the button <b>Remove</b> and create anothermap (i.e. different country). <b>Region</b> also not working. 
</p>')
        ),
    ## Options for examples
    fluidRow(
        shinyjs::useShinyjs(),
        column(3,
               selectInput(inputId = "example",
                           label = 'Load an example',
                           choices = c('Mexico (quick)', 'Other')),
               ),
        column(2,
               actionButton(inputId="load",label="Load", width = "90px"),
               HTML('<br/>'),
               actionButton(inputId="remove",label="Remove", width = '90px')
               ),
        
        ## Map details (text)
        column(7,
               tags$div(class = 'text-between',
                        tags$h3("The Map")
                        )
               )
        ),

    ## Handsontable (Main data)
    fluidRow(
        column(5,
               rHandsontableOutput("country.dat"),
               HTML('<br/><br/>'),
               actionButton(inputId="enter",label="Get coords", width = '110px'),
               actionButton(inputId="mapit",label="Make map", width = '110px')
               ),
        
        ## Map area
        column(7,
               sliderInput(inputId = 'year',
                           label = 'Select year',
                           min = 1950,
                           max = 2021,
                           sep = '',
                           value = 2021),
               plotOutput("map")
               )
    ),

    ## Below main
    fluidRow(
        column(5,
               div(class = 'text-between',
                   h3('Message log')
                   ),
               textOutput("message"),
               tags$head(tags$script(src='script.js'))
               )
    )
))


server = function(input,output){

    ## Initialize empty table
    DF <- data.frame(City = character(5),
                    Country = character(5),
                    Region = character(5),
                    Year = integer(5))
    output$country.dat = renderRHandsontable(rhandsontable(DF, readOnly = F))

    ## Clear all data button 'Remove'
    observeEvent(input$remove, {
        db <- db.empty
        output$country.dat = renderRHandsontable(rhandsontable(DF, readOnly = F))
    })

    ## Load examples
    observeEvent(input$load, {
        if(input$example == 'Mexico (quick)'){
            output$country.dat = renderRHandsontable(rhandsontable(t.dat, readOnly = T))
        }
    })

    ## For now, click only prints table in R
    observeEvent(input$enter, {
        DF = hot_to_r(input$country.dat)
        DF$ID <- 1:nrow(DF)
        withCallingHandlers({
            shinyjs::html('message', '')
            get_coords(DF)
        },
        message = function(m) {
            shinyjs::html(id = 'message', html = paste0(m$message, '<br>'), add = T)
        })
    })

    ## Click to make map
    observeEvent(input$mapit, {
        DF = hot_to_r(input$country.dat)
        DF$ID <- as.character(1:nrow(DF))
        
        #map.df <- dplyr::left_join(DF, db, by = c('ID', 'City'))
        output$map <- renderPlot({
            validate(
                need(nrow(db) != 0, "Get the coordinates first!")
            )
            map.df <- dplyr::left_join(DF, db, by = c('ID', 'City'))
            map_by_year(map.df, year = input$year)
        })
    })
}

shinyApp(ui = ui, server = server)

