#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages <- c('shiny', 'tidyverse', 'plotly', 'aws.s3', 'shinydashboard',
              'leaflet', 'sf', 'shinydashboardPlus', 'htmltools')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

####################
##### TO DO ########
####################

# ADD UPDATED HEX MAP AND RENAME VARIABLES
# PLUS FIX HEX MAP

######################################
######## LOAD IN DATA AND CLEAN ######
######################################

bucket <- 'thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4'

all_variables_joined <- s3read_using(read.csv,
                                     object = '/Tom/GP-contract-unpaid-carers/Outputs/all_variables_joined.csv',
                                     bucket = bucket)

map <- s3read_using(read_sf,
              object = '/Tom/GP-contract-unpaid-carers/Outputs/map_transformed.geojson',
              bucket = bucket)

hex_map <- s3read_using(read_sf,
              object = '/Tom/GP-contract-unpaid-carers/Outputs/hex_map_transformed.geojson',
              bucket = bucket)

all_variables_renamed <- all_variables_joined %>%
  rename(`Difference (number of unpaid carers)`= Difference,
    `Total number of unpaid carers (census)` = CENSUS_NO_OF_CARERS,
         `Estimated number of unpaid carers (GP data)` = EST_CARERS_IN_LA,
    `Number of patients registered to missing practices` = MISSING_PATIENTS, 
         `Proportion of households subject to 1+ dimensions of deprivation` = PERCENT_DEPRIVATION, 
         `Proportion of carers who are female` = prop_female,
         `Proportion of carers over 65` = prop_over65,
         `Proportion of carers under 25` = prop_under25, 
         `Proportion of carers giving over 50 hours of care per week` = prop_over50hours, 
         `Proportion of carers giving under 9 hours of care per week` = prop_lowintensity)

map_variables_renamed <- map %>%
  rename(`Difference (number of unpaid carers)`= Difference,
    `Number of patients registered to missing practices` = MISSING_PATIENTS, 
         `Proportion of households subject to 1+ dimensions of deprivation` = PERCENT_DEPRIVATION, 
         `Proportion of carers who are female` = prop_female,
         `Proportion of carers over 65` = prop_over65,
         `Proportion of carers under 25` = prop_under25, 
         `Proportion of carers giving over 50 hours of care per week` = prop_over50hours, 
         `Proportion of carers giving under 9 hours of care per week` = prop_lowintensity)

LAs_plus <- c('Select All', as.character(sort(unique(map_variables_renamed$LA_NAME))))

LAs_only <- as.character(sort(unique(map_variables_renamed$LA_NAME)))

all_variables_renamed$tooltips <- paste0(all_variables_renamed$LA_NAME,
                                         '\nCoverage', ': ', round(all_variables_renamed$Coverage, 2))

########################################################
#################                        ###############
#################          UI            ###############
#################                        ###############
########################################################


# Define UI for the dashboard
ui <- 
  
  dashboardPage(skin = 'red',

    # Application title
    dashboardHeader(title = "GP contract data"),

    # Initial sidebar
    dashboardSidebar(sidebarMenu(
      menuItem('Maps', tabName = "maps"),
      menuItem('Scatter Plots', tabName = "scatter_plots"),
      menuItem('Histograms', tabName = "histograms")
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = 'maps',
                fluidRow(
                    valueBoxOutput("count1", width = 3),
                    valueBoxOutput("count2", width = 3),
                    valueBoxOutput("count3", width = 3),
                    valueBoxOutput("count4", width = 3)
                  ),
                
                fluidRow(
                  column(3, 
                         selectInput(inputId = 'map_type',
                                     label = 'Map View',
                                     choices = list('Regular view',
                                                    'Hex map')),
                         selectInput(inputId = 'local_authority',
                                     label = 'Local Authority',
                                     choices = LAs_plus)
                         ),
                  column(9, leafletOutput('map1'))
                ),
                
                fluidRow(
                  column(3, h3(strong(textOutput('ICB_text')))),
                  column(9, plotlyOutput('boxplot1'))
                )
                
                ),
        
        
        tabItem(tabName = 'scatter_plots',
                  # Sidebar with a select input for x variable
                  fluidRow(
                    column(3,
                      selectInput(inputId = 'x_variable',
                                  label = "Select X variable: ",
                                  choices = list('Total number of unpaid carers (census)',
                                    'Number of patients registered to missing practices', 
                                                 'Proportion of households subject to 1+ dimensions of deprivation', 
                                                 'Proportion of carers who are female',
                                                 'Proportion of carers over 65',
                                                 'Proportion of carers under 25', 
                                                 'Proportion of carers giving over 50 hours of care per week', 
                                                 'Proportion of carers giving under 9 hours of care per week')),
                      
                      selectInput(inputId = 'y_variable',
                                  label = "Y variable: ",
                                  choices = list('Coverage', 'Difference (number of unpaid carers)'))
                    ),
                    
                    # Show a plot of the selected variables
                    column(9,
                      plotlyOutput("scatterPlot")
                    )
                  )
                
                
                ),
        tabItem(tabName = 'histograms',
                fluidRow(column(3, 
                                selectInput(inputId = 'hist_variable',
                                            label = "Histogram variable: ",
                                            choices = list('Coverage', 'Difference (number of unpaid carers', 'Total number of unpaid carers (census)',
                                                           'Estimated number of unpaid carers (GP data)',
                                                           'Number of patients registered to missing practices', 
                                                           'Proportion of households subject to 1+ dimensions of deprivation', 
                                                           'Proportion of carers who are female',
                                                           'Proportion of carers over 65',
                                                           'Proportion of carers under 25', 
                                                           'Proportion of carers giving over 50 hours of care per week', 
                                                           'Proportion of carers giving under 9 hours of care per week')) 
                                ), 
                         
                         column(9, 
                                plotlyOutput('Histogram'))
                         )
               )
      )
    )

  )



########################################################
#################                        ###############
#################        SERVER          ###############
#################                        ###############
########################################################

# Define server 
server <- function(input, output) {
  
  ## CREATE DYNAMIC DF FOR MAP TAB
  
  filtered_df <- reactive({
    all_variables_renamed %>%
      filter(
        if(input$local_authority == 'Select All'){LA_NAME %in% LAs_plus}else{LA_NAME == input$local_authority}
      )
  })
  
  
  ## MAP TAB: VALUE BOXES
  output$count1 <- renderValueBox({
    number_carers <- filtered_df() %>%
      summarise(sum(`Total number of unpaid carers (census)`))
    
    valueBox(value = tags$p(paste0(number_carers), style = 'font-size: 50%'), subtitle = 'Number of carers (census)', color = 'green')
  
  })
  
  output$count2 <- renderValueBox({
    
    GP_carers <- filtered_df() %>%
      summarise(sum(`Estimated number of unpaid carers (GP data)`))
    
    valueBox(value = tags$p(paste0(round(GP_carers, 0)), style = 'font-size: 50%'), subtitle = 'Number of carers (GP data)', color = 'olive')
  })
  
  output$count3 <- renderValueBox({
    
    coverage <- filtered_df() %>%
      summarise(GP = sum(`Estimated number of unpaid carers (GP data)`), Census = sum(`Total number of unpaid carers (census)`)) %>%
      mutate(coverage = GP/Census) %>%
      select(coverage)
    
    valueBox(value = tags$p(paste0(round(coverage*100, 0), '%'), style = 'font-size: 50%'), subtitle = 'Coverage', color = 'blue')
    
  })
  
  output$count4 <- renderValueBox({
    missing_practices <- filtered_df() %>%
      replace_na(list(MISSING_PRACTICES = 0)) %>%
      summarise(sum(MISSING_PRACTICES))
    
    valueBox(value = tags$p(paste0(missing_practices), style = 'font-size: 50%'), subtitle = 'Missing Practices', color = 'red')
    
  })
  
  
  
  ## MAP TAB: MAPS
 
  
  # Make map-specific reactive datasets and pop-ups 
  filtered_map_df <- reactive({
    map_variables_renamed %>%
      filter(
        if(input$local_authority == 'Select All'){LA_NAME %in% LAs_plus}else{LA_NAME == input$local_authority}
      )
  })
  
  filtered_hex_map_df <- reactive({
    hex_map %>%
      filter(
        if(input$local_authority == 'Select All'){LAD22NM %in% LAs_plus}else{LAD22NM == input$local_authority}
      )
  })
  
  map_palette <- reactive({
    colorNumeric('viridis', filtered_map_df()$Coverage)
  })
  
  hex_map_palette <- reactive({
    colorNumeric('viridis', filtered_hex_map_df()$Coverage)
  })

map_popup <- reactive({
  paste(sep = "<br>",
        "<b>",filtered_map_df()$LA_NAME,
        "ICB: ", filtered_map_df()$maj_ICB,
        "Coverage: ", round(filtered_map_df()$Coverage, 2))
})

hex_map_popup <- reactive({
  paste(sep = "<br>",
        "<b>",filtered_hex_map_df()$LAD22NM,
        #"ICB: ", map_variables_renamed$maj_ICB, "<b>",
        "Coverage: ", round(filtered_hex_map_df()$Coverage, 2))
})
  
    
  output$map1 <- renderLeaflet({
    
    colours_viridis <- colorNumeric(palette = 'viridis', domain = NULL)
  
    if(input$map_type == 'Regular view'){
      
      leaflet() %>%
        addProviderTiles('CartoDB.Positron') %>%
        setView(lng = 3, lat = 54.3781, zoom = 5) %>%
      addLegend(
        pal = colours_viridis,
        values = map_variables_renamed$Coverage,
        title = 'Coverage')
    }
   else if (input$map_type == 'Hex map'){
     
     leaflet() %>%
       addProviderTiles('CartoDB.Positron') %>%
       setView(lng = 3, lat = 54.3781, zoom = 5) %>%
       addLegend(
         pal = colours_viridis,
         values = hex_map$Coverage,
         title = 'Coverage')
   }
    
  })
  
  observe({
    
    colours_viridis <- colorNumeric(palette = 'viridis', domain = NULL)
    
    pal1 <- map_palette()
    pal2 <- hex_map_palette()
    
    if(input$map_type == 'Regular view'){
      
      leafletProxy("map1") %>%
        clearShapes() %>% 
        addPolygons(
          data = map_variables_renamed,
          fillOpacity = 0.1,
          color = 'black',
          weight = 1,
          highlightOptions = highlightOptions(color = 'black')
        ) %>% 
        addPolygons(data = filtered_map_df(),
                    stroke = FALSE,
                    fillOpacity = 0.8,
                    weight = 0.8,
                    label = lapply(map_popup(), htmltools::HTML),
                    popup = map_popup(),
                    color = ~pal1(filtered_map_df()$Coverage))
      
    }
    else if (input$map_type == 'Hex map'){
      
      leafletProxy("map1") %>%
        clearShapes() %>% 
        addPolygons(
          data = hex_map,
          fillOpacity = 0.1,
          color = 'black',
          weight = 1,
          highlightOptions = highlightOptions(color = 'red')
        ) %>% 
        addPolygons(data = filtered_hex_map_df(),
                    stroke = FALSE,
                    fillOpacity = 0.9,
                    weight = 0.9,
                    label = lapply(hex_map_popup(), htmltools::HTML),
                    popup = hex_map_popup(),
                    color = ~pal2(filtered_hex_map_df()$Coverage))
      
    }
    
    
  })
  
  
  output$ICB_text <- renderText({
    if (input$local_authority == 'Select All'){
      
    }else{
      ICB <- filtered_df()$maj_ICB
      paste0('ICB: ', ICB)
    } 
    
  })
  
  output$boxplot1 <- renderPlotly({
    if (input$local_authority == 'Select All'){
      
      
      ggplotly(ggplot(data = all_variables_renamed, aes(y = Coverage, x = factor(0), text=tooltips)) +
                 geom_boxplot() + 
                 geom_point(size = 0.5, color = 'darkblue') +
                 theme_minimal() +
                 coord_flip() +
                 xlab(""),
               tooltip='text')
      
      
    } else{

      ICB <- filtered_df()$maj_ICB
      
      box_df <- all_variables_renamed %>%
        filter(maj_ICB == ICB)
        
      
      ggplotly(ggplot(data = box_df, aes(y = Coverage, x = factor(0), text=tooltips)) +
                      geom_boxplot()+
                      geom_point(size = 2, color = 'darkblue') + 
                      theme_minimal() +
                       coord_flip() +
                 xlab(""),
               tooltip='text')}
    
  })
  
  
  
 ## SCATTER PLOT TAB
  
    output$scatterPlot <- renderPlotly({
      
        # Create dynamic dataframe which adjusts based on the select input
        df <- all_variables_renamed[,c(input$x_variable, input$y_variable, 'LA_NAME')]

        # Create scatterplot based on dynamic dataframe above
        if(input$y_variable == 'Coverage'){
        df$tooltips <- paste0(df$LA_NAME,
                           '\n',  input$y_variable, ': ', round(df[,2], 2),
                           #'\n',  input$x_variable, ': ', 
                           '\nx-value: ', round(df[,1], 2))
        } else if (input$y_variable == 'Difference (number of unpaid carers)'){
          df$tooltips <- paste0(df$LA_NAME,
                                '\n',  input$y_variable, ': ', round(df[,2], 0),
                                #'\n',  input$x_variable, ': ', 
                                '\nx-value: ', round(df[,1], 2))
        }
        
        
        ggplotly(ggplot() +
          geom_point(data = df, aes(x = df[,1], y=df[,2], text = tooltips), color = 'darkblue') +
          theme_minimal()+
            xlab(input$x_variable) +
            ylab(input$y_variable), tooltip = 'text') %>%
          layout(hoverlabel = list(
            bgcolor = 'white'
          ))
    })
    
    
    ## HISTOGRAM TAB
    
    output$Histogram <- renderPlotly({
     
      # Create dynamic dataframe which adjusts based on the select input
      df <- all_variables_renamed[,c(input$hist_variable, 'LA_NAME')]
      
      ggplotly(ggplot() +
                 geom_histogram(data = df, aes(x = df[,1]), fill = 'lightblue', color = 'darkblue') +
                 theme_minimal() +
                 xlab(input$hist_variable) +
                 ylab('Count of LAs'), tooltip = 'count')
    }
    
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
