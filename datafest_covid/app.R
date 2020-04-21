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
ui <- fluidPage(theme = shinytheme("yeti"),

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
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-02-24"))) +
            annotate("text", x = as.Date("2020-02-24"), y = 10, 
                     label = "DOW J experiences worst 
                     day in two years") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-03-06"))) +
                annotate("text", x = as.Date("2020-03-06"), y = 70, 
                         label = "$8.3 billion emergency 
                         spending package signed") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-03-13"))) +
                annotate("text", x = as.Date("2020-03-13"), y = 10, 
                         label = "Trump declares a national 
                         state of emergency") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-03-18"))) +
                annotate("text", x = as.Date("2020-03-18"), y = 70, 
                         label = "Families First Coronavirus 
                         Response Act signed into law") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-03-24"))) +
                annotate("text", x = as.Date("2020-03-24"), y = 10, 
                         label = "DOW J surges by more 
                         than 2,000 points") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-03-27"))) +
                annotate("text", x = as.Date("2020-03-27"), y = 70, 
                         label = "Trump signs a $2 trillion 
                         coronavirus economic stimulus bill") +
            geom_vline(color = "red", 
                       xintercept = as.numeric(as.Date("2020-04-11"))) +
                annotate("text", x = as.Date("2020-04-11"), y = 10, 
                         label = "The United States becomes 
                         the worst-hit country in the world") +
            ylim(0,100) +
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
##change party to radio buttons - checkboxes?
##action button
##make table fancy!! - table color, round to 2
##shiny dashboard? add color

# Run the application 
shinyApp(ui = ui, server = server)
