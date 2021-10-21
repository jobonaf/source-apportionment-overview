load("data/OverviewSourceApp_MunicFVG.rda")
source("util/configure.R") 
library(shiny)
library(billboarder)
library(glue)
library(dplyr)
library(bslib)
library(shinyBS)

ui <- fluidPage(
    fluidRow(
        column(width = 4,
               selectInput("poll", "inquinante:", choices=unique(SectSA$Pollutant), multiple = F,
                           selected="PM10"),
               bsTooltip("poll", "Scegli l\\'inquinante. L\\'analisi si riferisce alla media annua.", placement = "right"),
               sliderInput("perc", "percentile:", min = 0, max = 100, value = 50, step = 10),
               bsTooltip("perc", "Le medie annue considerate si riferiscono al periodo 2014-2019 e all\\'intero territorio comunale. Di default consideriamo la mediana di queste medie. Per tenere conto della variabilità spazio-temporale puoi ripetere l\\'analisi considerando percentili diversi rispetto alla mediana.", 
                         placement = "right")),
        column(width = 4,
               selectInput("muni", "Comuni:", choices=unique(SectSA$Municipality), 
                           selected=sample(unique(SectSA$Municipality),10), multiple = T),
               bsTooltip("muni", "Sono selezionati casualmente 10 Comuni del FVG. Puoi sceglierne altri.", placement = "right")
        )
    ),
    fluidRow(
        column(width = 6, 
               billboarderOutput("mybb1", height = "500px"),
               bsTooltip("mybb1", "Cliccando sulla legenda puoi escludere dal grafico uno o più territori.", placement = "bottom")
        ),
        column(width = 6, 
               billboarderOutput("mybb2", height = "500px"),
               bsTooltip("mybb2", "Cliccando sulla legenda puoi escludere dal grafico uno o più settori.", placement = "bottom"))
    )
)

server <- function(input, output) {
    
    output$mybb1 <- renderBillboarder({
        
        df <- SpatSA %>% 
            filter(Percentile==input$perc,
                   Pollutant==input$poll,
                   Municipality%in%input$muni) %>%
            mutate(Share=round(Share,2))
        
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
            mutate(Share=round(Share,2)) %>%
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
