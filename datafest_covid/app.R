library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(readr)

covid_approval_polls_adjusted <- read_csv("/cloud/project/covid_approval_polls_adjusted.csv")

covid_approval_polls_adjusted <- covid_approval_polls_adjusted %>% 
    mutate(startdate = as.Date(startdate, "%m/%d/%Y"))

covid_approval_polls_adjusted <- covid_approval_polls_adjusted %>% 
    mutate(enddate = as.Date(enddate, "%m/%d/%Y"))

pollsters <- covid_approval_polls_adjusted %>% 
    count(pollster) %>% 
    pull(pollster) %>% 
    as.character()

party <- covid_approval_polls_adjusted %>% 
    count(party) %>% 
    pull(party) %>% 
    as.character()


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Trump Approval Ratings during COVID-19"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            dateRangeInput(inputId = "daterange",
                           label   = "Enter poll start range",
                           start   = "2020-02-02",
                           end     = "2020-04-17",
                           min     = "2020-02-02",
                           max     = "2020-04-17"),
            
            radioButtons(inputId = "party",
                         label = "Select party of interest",
                         choices = party),
            
            selectInput(inputId = "pollster",
                        label   = "Select pollster",
                        choice  = pollsters),
            
            submitButton("Update View", icon("refresh")),
            helpText("When you click the button above, you should see",
                     "the output to the right update to reflect the values you",
                     "entered at the top"),
            verbatimTextOutput("value")
            
        ), # sidebarPanel
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "plotpolls"),
           dataTableOutput(outputId = "tablepolls")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$plotpolls <- renderPlot({
        covid_approval_polls_adjusted %>% 
            filter(startdate >= input$daterange[1],
                   startdate <= input$daterange[2]) %>% 
            filter(pollster == input$pollster) %>% 
            filter(party == input$party) %>% 
            ggplot(aes(x = startdate, y = approve_adjusted)) +
            geom_point(color = "darkgreen", size = 2) +
            geom_line(color = "darkgreen", lty = 2) +
            labs(x = "poll's starting date", y = "proportion approve") +
            theme_bw()
            #theme(legend.position = "bottom") +
            #scale_color_identity(name = "Trump approval", 
                                 #breaks = c("green", "red"),
                                 #labels = c("approve", "disapprove"),
                                 #guide = "legend") 
    }) 
    
    output$tablepolls <- DT::renderDataTable({
        covid_approval_polls_adjusted %>% 
            filter(startdate >= input$daterange[1],
                   startdate <= input$daterange[2]) %>% 
            filter(pollster == input$pollster) %>% 
            filter(party == input$party) %>% 
            select("party"|"startdate"|"enddate"|"pollster"|"samplesize"|"approve_adjusted"|"disapprove_adjusted")
    })
    

}

##no polls match criteria if selected attributes yield nothign
##lines showing when certain things happened
##change party to radio buttons
##action button

# Run the application 
shinyApp(ui = ui, server = server)
