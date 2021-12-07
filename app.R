library(htmltools)
library(fastmap)
library(shiny)
library(plotly)
library(DT)
library(bslib)
library(shinythemes)

#Indicates how jagged or flat the terrain of a country is on average. Ruggedness is measured in metres of
#elevation difference for grid points 30 arc-seconds (926 metres on the equator or any meridian) apart.

master <- read.csv("GlobalLandTemperaturesByCountry.csv")
small_test <- filter(master, dt>'1981-01-01' & dt<'2013-01-01')
master2 <- read.csv("Production_Crops_Livestock_E_All_Data_(Normalized).csv")
hoverinfo_table <- read.table(file = 'HoverInfo.tsv', sep = '\t', header = TRUE)
alt_crops_table <- read.table(file = 'Alternative Crops.tsv', sep = '\t', header = TRUE)
crop_recommendations_table <- read.table(file = 'Crop Recommendations.tsv', sep = '\t', header = TRUE)
master2 <- filter(master2, Element == 'Production')
master2[is.na(master2)]<-0
master2 <-
    master2 %>%
    group_by(Item) %>%
    mutate(maximum = max(Value))
rugged_data <- read.csv("rugged_data.csv")
ruggedness <- read.csv("terrain-ruggedness-index.csv")

rugged_data$country <-
    iconv( x = rugged_data$country
           , from = "UTF-8"
           , to = "UTF-8"
           , sub = "" )
master2$Area <-
    iconv( x = master2$Area
           , from = "UTF-8"
           , to = "UTF-8"
           , sub = "" )

master2$Item <-
    iconv( x = master2$Item
           , from = "UTF-8"
           , to = "UTF-8"
           , sub = "" )
graph_choice_vector <-c("Global Temperature Over Time (Celsius)", "Global Crop Production Over Time (Tonnes)", "Global Ruggedness Score", "Global Soil Fertility Rate (%)", "Global Desert Rate (%)", "Global Tropical Conditions Rate (%)", "Global Average Distance From Coast (Kilometers)","Detailed Overview","Alternative Crops", "Crop Recommendations")
choice_vector <- unique(master2$Item)


print(choice_vector)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    theme = shinythemes::shinytheme("united"),
    titlePanel("Global Climate and Crop Production Data"),
    
    wellPanel(
        selectInput("graph_choice", "Choose", choices = graph_choice_vector, selected = NULL)
    ),
    
    conditionalPanel(
        condition = "input.graph_choice == 'Global Temperature Over Time (Celsius)'",
        plotlyOutput('temperature_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Crop Production Over Time (Tonnes)'",
        selectInput("choice", "Choose", choices = choice_vector, selected = NULL),
        plotlyOutput('distPlot')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Ruggedness Score'",
        plotlyOutput('ruggedness_score')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Soil Fertility Rate (%)'",
        plotlyOutput('soil_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Desert Rate (%)'",
        plotlyOutput('desert_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Tropical Conditions Rate (%)'",
        plotlyOutput('tropical_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Global Average Distance From Coast (Kilometers)'",
        plotlyOutput('coast_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Detailed Overview'",
        plotlyOutput('detail_graph')
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Alternative Crops'",
        DT::dataTableOutput("alternative_crops_table")
    ),
    conditionalPanel(
        condition = "input.graph_choice == 'Crop Recommendations'",
        plotlyOutput('crop_rec')
    )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    fontStyle = list(
        family = "Helvetica",
        size = 15,
        color = "black"
    )
    label = list(
        bgcolor = "#EEEEEE",
        bordercolor = "transparent",
        font = fontStyle 
    )
    output$distPlot <- renderPlotly({
        farm_graph <- plot_geo(filter(master2,Item == input$choice),locationmode = 'country names', frame = ~Year) %>%
            add_trace(locations = ~Area, 
                      z = ~Value,
                      zmax = filter(master2, Item == input$choice)$maximum,
                      zmin = 0,
                      color = ~Value,
                      colorscale = "temps")
    })
    
    output$temperature_graph <- renderPlotly({
        plot_geo(small_test,locationmode = 'country names', frame = ~dt) %>%
        add_trace(locations = ~Country, 
                  z = ~AverageTemperature,
                  zmin = -50,
                  zmax = 50, 
                  color = ~AverageTemperature,
                  colorscale = "temps")
    })
    output$ruggedness_score <- renderPlotly({ 
        plot_geo(ruggedness, locationmode = 'country names') %>%
            add_trace(locations = ~Entity,
                      z = ~Terrain.Ruggedness,
                      color = ~Terrain.Ruggedness,
                      colorscale = "Earth"
            )
    })
    output$soil_graph <- renderPlotly({
        plot_geo(rugged_data, locationmode = 'country names')%>%
            add_trace(locations = ~country,
                      z = ~soil,
                      color = ~soil,
                      colorscale = "Earth"
            )
        
        
    })
    output$desert_graph <- renderPlotly({
        plot_geo(rugged_data, locationmode = 'country names')%>%
            add_trace(locations = ~country,
                      z = ~desert,
                      color = ~desert,
                      colorscale = "Viridis"
            )
    })
    output$tropical_graph <- renderPlotly({
        plot_geo(rugged_data, locationmode = 'country names')%>%
            add_trace(locations = ~country,
                      z = ~tropical,
                      color = ~tropical,
                      colorscale = "YlGnBu"
            )
    })
    output$coast_graph <- renderPlotly({
        plot_geo(rugged_data, locationmode = 'country names')%>%
            add_trace(locations = ~country,
                      z = ~dist_coast,
                      color = ~dist_coast,
                      colorscale = "YlGnBu"
            )
        
    })
    output$detail_graph <- renderPlotly({
        plot_geo(hoverinfo_table, locationmode = 'country names')%>%
            add_trace(locations = ~Country,
                      text = ~HoverInfo,
                      z = ~z.mode,
                      color = ~z.mode,
                      hoverinfo = 'text',
                      showscale = FALSE)%>%
            style(hoverlabel = label)
    
    
    })
    output$alternative_crops_table = DT::renderDataTable({
        alt_crops_table
    })
    output$crop_rec <- renderPlotly({
        plot_geo(crop_recommendations_table, locationmode = 'country names')%>%
            add_trace(locations = ~Country,
                      text = ~Recommended.Alternative.Crop,
                      z = ~Z.mode,
                      color = ~Z.mode,
                      hoverinfo = 'text',
                      showscale = FALSE)%>%
            style(hoverlabel = label)
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)


