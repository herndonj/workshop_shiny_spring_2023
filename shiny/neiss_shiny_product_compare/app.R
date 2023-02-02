#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
injuries <- read_tsv("../../data/NEISS/injuries.tsv")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("NEISS Demographic Comparisons by Product"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "code", label = "Product 1",
                        choices = c( "floors or flooring materials" = 1807,	
                                     "stairs or steps" = 1842,
                                     "beds or bedframes, other or not specified" = 4076,
                                     "basketball (activity, apparel or equipment)" = 1205,
                                     "bicycles and accessories" = 5040),
                        selected = 1205),
        
        
            selectInput(inputId = "code2", label = "Product 2",
                    choices = c( "floors or flooring materials" = 1807,	
                                 "stairs or steps" = 1842,
                                 "beds or bedframes, other or not specified" = 4076,
                                 "basketball (activity, apparel or equipment)" = 1205,
                                 "bicycles and accessories" = 5040),
                    selected = 1205),
        ),
    
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("injurePlot1"),
           plotOutput("injurePlot2"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    injuries_demographics <- reactive ({
        injuries |>  filter(prod_code == input$code) |> count(age, sex, wt = weight, sort = TRUE)
    })

    output$injurePlot1 <-renderPlot({injuries_demographics() |> 
            ggplot(aes(x = age, y = n, color = sex)) +
            geom_line(size = 1.5) +
            labs(title = "Estimated Number of Injuries for Product 1", y="") +
            theme_grey(base_size = 16)  
    })
    
    injuries_demographics2 <- reactive ({
        injuries |>  filter(prod_code == input$code2) |> count(age, sex, wt = weight, sort = TRUE)
    })
    
    output$injurePlot2 <-renderPlot({injuries_demographics2() |> 
            ggplot(aes(x = age, y = n, color = sex)) +
            geom_line(size = 1.5) +
            labs(title = "Estimated Number of Injuries for Product 2", y="") +
            theme_grey(base_size = 16)  
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
