library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(readr)
library(ggiraph)

covid_approval_polls_adjusted <- read_csv("/cloud/project/covid_approval_polls_adjusted.csv")

covid_approval_polls_adjusted <- covid_approval_polls_adjusted %>% 
    mutate(startdate = as.Date(startdate, "%m/%d/%Y")) %>% 
    mutate(enddate = as.Date(enddate, "%m/%d/%Y"))

covid_approval_polls_adjusted <- covid_approval_polls_adjusted %>% 
    mutate(
        party = case_when(
            party == "D" ~ "Democrat",
            party == "R" ~ "Republican",
            party == "I" ~ "Independent",
            party == "all" ~ "All parties"
        )
    )

pollsters <- covid_approval_polls_adjusted %>% 
    count(pollster) %>% 
    pull(pollster) %>% 
    as.character()

party <- covid_approval_polls_adjusted %>% 
    count(party) %>% 
    pull(party) %>%
    as.character()


# Define UI for application 
ui <- fluidPage(theme = shinytheme("united"),
                
                # Application title
                titlePanel("Trump Approval Ratings during COVID-19"),
                
                # Sidebar
                sidebarLayout(
                    sidebarPanel(
                        
                        dateRangeInput(inputId = "daterange",
                                       label   = "Enter poll date range",
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
                        helpText("When you click the button above, you should",
                                 "see the output to the right update to",
                                 "reflect the values you entered at the top"),
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
            geom_point(color = "darkgreen", size = 4, alpha = 0.5) +
            geom_line(color = "darkgreen", lty = 2) +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-02-24"))) +
                annotate("text", x = as.Date("2020-02-24"), color = "red", 
                         alpha = 0.7, y = 10, 
                     label = "DOW J experiences\nworst day in\ntwo years") +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-03-06"))) +
                annotate("text", x = as.Date("2020-03-06"), color = "red", 
                         alpha = 0.7, y = 60, 
                     label = str_c("$8.3 billion\n ",
                                   "emergency\nspending package\nsigned")) +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-03-13"))) +
                annotate("text", x = as.Date("2020-03-13"), color = "red", 
                         alpha = 0.7, y = 10, 
                     label = str_c("Trump declares\na ",
                                   "national\nstate of\nemergency")) +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-03-18"))) +
                annotate("text", x = as.Date("2020-03-18"), color = "red", 
                     alpha = 0.7, y = 80, 
                     label = str_c("Families First\nCoronavirus\nResponse ",
                     "Act\nsigned into law")) +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-03-24"))) +
                annotate("text", x = as.Date("2020-03-24"), color = "red", 
                         alpha = 0.7, y = 10, 
                     label = "DOW J surges\nby more\nthan 2,000\npoints") +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-03-27"))) +
                annotate("text", x = as.Date("2020-03-27"), color = "red", 
                         alpha = 0.7, y = 60, 
                     label = str_c("Trump signs\n $2 trillion\n ",
                                   "coronavirus\neconomic\nstimulus bill")) +
            geom_vline(color = "red", alpha = 0.4,
                       xintercept = as.numeric(as.Date("2020-04-11"))) +
                annotate("text", x = as.Date("2020-04-11"), color = "red", 
                         alpha = 0.7, y = 10, 
                     label = str_c("US becomes\nthe ",
                                   "worst-hit country\nin the world")) +
            ylim(0,100) +
            xlim(as.Date("2020-02-02"), as.Date("2020-04-17")) +
            labs(x = "poll's starting date", y = "proportion approve") +
            theme_bw()
        
    }) 
    
    output$tablepolls <- DT::renderDataTable({
        covid_approval_polls_adjusted %>% 
            filter(startdate >= input$daterange[1],
                   startdate <= input$daterange[2]) %>% 
            filter(pollster == input$pollster) %>% 
            filter(party == input$party) %>% 
            select("party"|"startdate"|"enddate"|"pollster"|"samplesize"|
                       "approve_adjusted"|"disapprove_adjusted")
    })
    
    
}

##no polls match criteria if selected attributes yield nothign X
##lines showing when certain things happened X
##change party to radio buttons - checkboxes? X
##action button X
##shiny dashboard? add color X

# Run the application 
shinyApp(ui = ui, server = server)