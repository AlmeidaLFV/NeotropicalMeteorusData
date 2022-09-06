if(!require(shiny)) install.packages("shiny")
if(!require(leaflet)) install.packages("leaflet")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")

#load data from spreadsheet 
data<-read_excel(file.choose()) %>% select('genus','species','year', 'month',
 'latitude','longitude', 'altitude')

# retrieve names of genera and species on data set
gens <- unique(data$genus) %>% sort()
spps <- unique(data$species) %>% sort()

# App interface 
ui <- fluidPage(
  titlePanel("Specimen record explorer"),
  
  #Sidebar set-genus and species selection 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'genus',label = 'Select genus',choices = gens),
      selectInput(inputId = 'spp',label = 'Select species',choices = c("'all'",
                                                                       spps)),
    ),
    # Main panel set       
    mainPanel(
      #set tabs
      tabsetPanel(
        #App TAB 1 - Map and general data
        tabPanel("Map of distribution", 
          leafletOutput("map"),
          textOutput("spname"),
          textOutput("n_records"),
          textOutput("n_species"),
          tags$head(tags$style("#spname,#n_records, #n_species
                               {color: black;font-size: 18px;}"))
        ),
        #App Tab 2 - Monthly distribution
        tabPanel("Monthly distribution", 
          plotOutput(outputId = "seasonal")
        ),
        #App Tab3 - Altitudinal distribution
        tabPanel("Altitudinal distribution",
          plotOutput(outputId = "alt"),
          textOutput("range"),
          tags$head(tags$style("#range{color: black;font-size: 18px;}"))
        )
      )
    )
  )
)

#App server
server <- function(input, output, session) {
    
  # Create species list based on the genus selected   
  observe({
    chosengen <- input$genus
    d_genus <- data[data$genus==chosengen,]
    species_names <- unique(d_genus$species) %>% sort()
    updateSelectInput(session, inputId = "spp",label = "Select species",
                      choices = c("'all'", species_names))
  })
 
  #App TAB 1
  #Plot map with distribution of selected species         
  output$map <- renderLeaflet({
    chosengen <- input$genus
    chosenspp <- input$spp
    d_genus<-data[data$genus==chosengen,]
       
      if(chosenspp=="'all'"){
        leaflet() %>%
        addProviderTiles(providers$OpenTopoMap) %>%
        setView(lng = -55, lat = -5, zoom = 2.5) %>%
        addScaleBar(position = "bottomleft")%>%
        addMarkers(lng = as.numeric(d_genus$longitude), 
                         lat = as.numeric(d_genus$latitude)) 
      }
     
      else {     
        d_species<-data[data$species == chosenspp,]
        leaflet() %>%
        addProviderTiles(providers$OpenTopoMap) %>%
        setView(lng = -55, lat = -5, zoom = 2.5) %>%
        addScaleBar(position = "bottomleft")%>%
        addMarkers(lng = as.numeric(d_species$longitude),
                         lat = as.numeric(d_species$latitude))
      }
  })

  #Print text displaying the data selected 
  output$spname <- renderText({
    paste("Selected:", input$genus, input$spp)
    
  })
  
   #Print text displayng the total number of records on the data selected 
  output$n_records <- renderText({
    chosenspp <- input$spp
    chosengen <- input$genus
    d_genus<-data[data$genus == chosengen,]
    if(chosenspp=="'all'"){ 
      paste(c("Number of records:", nrow(d_genus)))
    }
    else{
      paste(c("Number of records:", nrow(d_genus[d_genus$species == chosenspp,
                                                 ])))
    }
  })
    
  #Print text with the total number of recorded species in the selected genus
  output$n_species <- renderText({
    chosenspp <- input$spp
    chosengen <- input$genus
    d_genus<-data[data$genus == chosengen,]
    
    if(chosenspp=="'all'"){
      paste("Number os species:", length(unique(d_genus$species)))
    }
  })
  
  #App Tab 2 
  #Plot the seasonal variation of the selected species
  output$seasonal <- renderPlot({
    chosenspp <- input$spp
    chosengen <- input$genus
    d_genus<-data[data$genus == chosengen,]
    
    if(chosenspp=="'all'"){
      month<-as.numeric(d_genus$month)
      
      hist(x = month, breaks = c(0:12), ylab= "Number of records", xlab="Month",
           main=" ", cex.lab=1.5)
      axis(1, at = c(1:12))
    }
    else{
      d_species<-d_genus[d_genus$species == chosenspp,]
      month<-as.numeric(d_species$month)
      
      hist(x = month, breaks = c(0:12), ylab= "Number of records", xlab="Month",
           main=" ", cex.lab=1.5)
      axis(1, at = c(1:12))
      axis(2, )
    }
  })
  
   #App Tab 3
   #Plot the altitudinal range of selected species
   output$alt<-renderPlot({
    chosenspp <- input$spp
    chosengen <- input$genus
    d_genus<-data[data$genus == chosengen,]
    
    if(chosenspp=="'all'"){
      alt_data<-as.numeric(d_genus$altitude)
      
      boxplot(alt_data, ylab="Altitude (m)", cex.lab=1.5, frame.plot=F, range=0)
    }
    else{
      alt_data<-as.numeric(d_genus[d_genus$species == chosenspp,]$altitude)
      
      boxplot(alt_data, ylab="Altitude (m)", cex.lab=1.5, frame.plot=F, range=0)
    }
   })  
 
   #Print text with the altitudinal range of selected species
   output$range <- renderText({
     chosenspp <- input$spp
     chosengen <- input$genus
     d_genus<-data[data$genus == chosengen,]
     
     if(chosenspp=="'all'"){
       paste(c("Altitudinal range (m):","-"), 
             range(as.numeric(d_genus$altitude),na.rm = T))
     }
     
     else{          
       paste(c("Altitudinal range (m):","-"), 
             range(as.numeric(d_genus[d_genus$species == chosenspp,]$altitude), 
                   na.rm = T))
     }
   })
}
# run app
shinyApp(ui, server)
