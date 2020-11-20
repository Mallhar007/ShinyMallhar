## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)

coffee_ratings <- readr::read_csv("Worldcoffee.csv")



ui <- dashboardPage(
    dashboardHeader(title = "Coffees of the World"),
    
    dashboardSidebar(
        selectInput("country", "Country", choices = coffee_ratings %>% 
                        select(country_of_origin) %>% 
                        distinct() %>% 
                        arrange(country_of_origin) %>% 
                        drop_na())
    ),
    dashboardBody(
        fluidRow(box(plotOutput("coffee_flavour")), box(plotOutput("coffee_variety"))),
        fluidRow(box(plotOutput("coffee_dif")), box(tableOutput("coffee_table")))
    )
)

server <- function(input, output) { 
    
    output$coffee_flavour <- renderPlot({
        
        coffee_ratings %>% 
            filter(country_of_origin == input$country) %>% 
            select(aroma:cupper_points) %>% 
            gather() %>% 
            group_by(key) %>% 
            summarise(value = mean(value)) %>% 
            ungroup() %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, value)) %>% 
            ggplot(aes(x = key, y = value, color = key)) + 
            geom_point(size = 5) + 
            geom_segment(aes(x = key, xend = key, y = value, yend = 0)) + 
            theme(legend.position = "none") + 
            ylab("") + 
            xlab("") + 
            coord_flip() + 
            labs(title = "Flavour averages")
    })
    
    output$coffee_variety <- renderPlot({
        
        coffee_ratings %>% 
            filter(country_of_origin == input$country) %>% 
            select(variety) %>% 
            drop_na() %>% 
            count(variety) %>% 
            mutate(variety = fct_reorder(variety, n)) %>% 
            ggplot(aes(x = n, y = variety, fill = variety)) + 
            geom_col() + 
            ylab("") + 
            xlab("") + 
            labs(title = "The types of Beans") + 
            theme(legend.position = "none")
    })
    
    
    output$coffee_dif <- renderPlot({
        coffee_ratings %>% 
            select(country_of_origin, aroma:cupper_points) %>% 
            mutate(highlight = if_else(country_of_origin == input$country, "Highlight", "No-Highlight")) %>% 
            select(-country_of_origin) %>% 
            gather(key = "key", value = "value", -highlight) %>% 
            group_by(key) %>% 
            do(t_test = t.test(value~highlight, data = .) %>% tidy()) %>% 
            unnest(t_test) %>% 
            mutate(diffference = case_when(
                conf.low < 0 & conf.high < 0 ~ "Different",
                conf.low > 0 & conf.high > 0 ~ "Different",
                TRUE ~ "Not-Different"
            )) %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, -estimate)) %>% 
            ggplot(aes(x = key, y = estimate, color = diffference)) + 
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
            geom_hline(yintercept = 0, linetype = "dashed") + 
            coord_flip() + 
            theme(legend.position = "none") + 
            xlab("") + 
            ylab("") + 
            labs(title = "Difference of flavours")
    })
    
    
    output$coffee_table <- function(){
        
        coffee_ratings %>% 
            filter(country_of_origin == input$country) %>% 
            select(Points = total_cup_points, Species= species, Country = country_of_origin, Region = region) %>% 
            drop_na()  %>% 
            group_by(Species, Region) %>% 
            arrange(desc(Points)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            mutate(Region = str_trunc(Region, 15, "right")) %>% 
            arrange(desc(Points)) %>% 
            kable() %>% 
            kable_styling() %>% 
            scroll_box(height = "400px", width = "600px")
    }
    
}

shinyApp(ui, server)
