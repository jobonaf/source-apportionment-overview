#.libPaths(new = "/u/arpa/bonafeg/R/x86_64-pc-linux-gnu-library/3.5") ## change this as you need
load("data/OverviewSourceApp_MunicFVG.rda")
source("R/configure.R")
#library(shiny)
library(highcharter)
library(glue)

ui <- fluidPage(

    column(width = 12,
           sliderInput("perc", "percentile:", min = 0, max = 100, value = 50, step = 10),
           selectInput("poll", "inquinante:", choices=unique(SectSA$Pollutant), multiple = F,
                       selected="PM10"),
           selectInput("muni", "Comuni:", choices=unique(SectSA$Municipality), 
                       selected=sample(unique(SectSA$Municipality),10), multiple = T)
    ),
    
    column(width = 6, billboarderOutput("mybb1", height = "500px")),
    column(width = 6, billboarderOutput("mybb2", height = "500px"))
)

server <- function(input, output) {
    
    output$mybb1 <- renderBillboarder({
        
        df <- SpatSA %>% 
            filter(Percentile==input$perc,
                   Pollutant==input$poll,
                   Municipality%in%input$muni) %>%
            mutate(Share=signif(Share,2))
        
        billboarder() %>% 
            bb_barchart(
                data = df,
                mapping = bbaes(x = Municipality, y = Share, group = Origin),
                stacked = T, rotated = T
            )%>% 
            bb_y_axis(label = list(text = glue("{input$poll} (ug/m3)"), position = "middle"))%>% 
            bb_legend(position = "right") %>% 
            bb_y_grid(show = TRUE) %>%
            bb_title(glue("{input$poll}: source apportionment territoriale"))
    })
    
    output$mybb2 <- renderBillboarder({
        
        df <- SectSA %>% 
            filter(Percentile==input$perc,
                   Pollutant==input$poll,
                   Municipality%in%input$muni) %>%
            mutate(Share=signif(Share,2)) %>%
            droplevels()
        
        billboarder() %>% 
            bb_barchart(
                data = df,
                mapping = bbaes(x = Municipality, y = Share, group = Sector),
                stacked = T, rotated = T
            )%>% 
            bb_y_axis(label = list(text = glue("{input$poll} (ug/m3)"), position = "middle")) %>% 
            bb_colors_manual(extraregionale = "lightgrey")%>% 
            bb_legend(position = "right") %>% 
            bb_y_grid(show = TRUE) %>%
            bb_title(glue("{input$poll}: source apportionment settoriale"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
