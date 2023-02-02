library(shiny)
library(tidyverse)

injuries <- read_tsv("../../data/NEISS/injuries.tsv")

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
            
            hr(),
            
            sliderInput(inputId = "text_size", 
                        label = "Magnify labels",
                        min = 1,
                        max = 25,
                        value = 16,
                        step = 1
                        ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("injurePlot1"),
        )
    ) 
)


server <- function(input, output, session) {
    injuries_demographics <- reactive ({
        injuries |>  filter(prod_code == input$code) |>
            count(age, sex, wt = weight, sort = TRUE)
    })
    
    output$injurePlot1 <-renderPlot({injuries_demographics() |> 
            ggplot(aes(x = age, y = n, color = sex)) +
            geom_line(size=1.5) +
            labs(title = "Estimated Number of Injuries for Product 1", y = "") +
            theme_grey(base_size = input$text_size)  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

