if(!require(shiny)) install.packages("shiny")
if(!require(leaflet)) install.packages("leaflet")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")

#load data: interactive file 
data<-read_excel(file.choose()) %>% select('genus','species','year', 'month',
 'latitude','longitude', 'altitude')


# retrieve names of genus and species on data set
gens <- unique(data$genus) %>% sort()
spps <- unique(data$species) %>% sort()

# App interface 
ui <- fluidPage(
  titlePanel("Specimen record explorer"),
  
  #sidebar-species selection 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'genus',label = 'Select genus',choices = gens),
      selectInput(inputId = 'spp',label = 'Select species',choices = c("'all'",
                                                                       spps)),
    ),
    # main panel set       
    mainPanel(
      #set tabs
      tabsetPanel(
        #TAB 1 - map and general data
        tabPanel("Map of distribution", 
          leafletOutput("map"),
          textOutput("spname"),
          textOutput("n_records"),
          textOutput("n_species"),
          tags$head(tags$style("#spname,#n_records, #n_species
                               {color: black;font-size: 18px;}"))
        ),
        #Tab 2 - Monthly distribution
        tabPanel("Monthly distribution", 
          plotOutput(outputId = "seasonal")
        ),
        #Tab3 - altitudinal distribution
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
    
  # Modify species list based on the genus selected   
  observe({
    chosengen <- input$genus
    d_genus <- data[data$genus==chosengen,]
    species_names <- unique(d_genus$species) %>% sort()
    updateSelectInput(session, inputId = "spp",label = "Select species",
                      choices = c("'all'", species_names))
  
  })
 
  #TAB 1
  #Map output         
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

  #Selected data text
  output$spname <- renderText({
    paste("Selected:", input$genus, input$spp)
    
  })
  
   #number of records 
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
    
  #number of species
  output$n_species <- renderText({
    chosenspp <- input$spp
    chosengen <- input$genus
    d_genus<-data[data$genus == chosengen,]
    
    if(chosenspp=="'all'"){
      paste("Number os species:", length(unique(d_genus$species)))
    }
  })
  
  
  #Tab 2 
  #Seasonal variation output
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
  
   #Tab 3
   #altitudinal plot
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
 
   #altitude range output
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