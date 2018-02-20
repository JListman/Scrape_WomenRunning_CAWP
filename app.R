

library(shiny)
library(plotly)
library(DT)
library(shinythemes)

elections <- readRDS("elections.rds")
statedata <- readRDS("statedata.rds")

mapcolors <- c("#808080", "#B2182B", "#8D2B4B", "#693F6B", "#45528B", "#2166AC")

columns <- c("State", "Candidate", "District", "Office", "Party")

ui <- fluidPage(theme = shinytheme("yeti"),
        titlePanel("Women Running for 2018 Congressional or State Office"),
        tabsetPanel(type = "tabs",
                    tabPanel("Map", align="center", plotlyOutput("statebins", width = "900px", height = "600px")),
                    tabPanel("Searchable Table",
                             fluidPage(
                             column(4,
                                     selectizeInput("Party", "Choose Party",
                                           c("Democrat" = "D",
                                             "Republican" = "R",
                                             "All" = "All"
                                             ),
                                           multiple = TRUE,
                                           selected = "D",
                                           width = '200px'
                                           )),
                             column(4,
                             selectizeInput("State", "Choose State(s)",
                                            c("All"= "All", levels(elections$State)),
                                            multiple = TRUE,
                                            selected = "California",
                                            width = '200px'
                                            )),
                             column(12,
                             DTOutput("data" )))
                    ),
                    tabPanel("About",
                             fluidPage(
                                     column(10,
                                            h3("An App to Search for Women Running for 2018 US Congressional or State Office"),
                                            p("I created this project to visualize a database maintained by the",
                                              a("Center for American Women in Politics",
                                                href = "http://www.cawp.rutgers.edu", target = "_blank"), "at",
                                              a("Rutgers University.", 
                                                href = "https://www.rutgers.edu", target = "_blank"), 
                                              "The Center conducts research on women's participation in politics in America and maintains", 
                                              a("a list", 
                                                href = "http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive",
                                                target = "_blank"),
                                              "of women potentially running for US Congress and State offices. The data are current as of 2/20/18."),
                                            p("You can read more about", a("how", href = "https://medium.com/@jblistman", target = "_blank"),
                                              "or", a("why", href = "https://medium.com/@jblistman/an-app-to-search-for-women-running-for-office-in-2018-346f5a013ec9", target = "_blank"), 
                                              "I created this app on my blog."),
                                            p(a("R", href = "https://www.r-project.org", target = "_blank"),
                                              "code and details of data processing and visualization are available on",
                                              a("GitHub.", icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"),
                                              "Find me, Jenny Listman, on", a("Twitter", icon("twitter"),
                                                      href = "https://twitter.com/jblistman", target = "_blank"), "or",
                                              a("LinkedIn", icon("linkedin"), 
                                                href = "https://www.linkedin.com/in/jenniferlistman/", target = "_blank")),
                                            HTML("<br><br><br>")
                                            )
                                     )
                             )
                    ),
        tags$div(class="footer", checked=NA, tags$p("An interactive app to view data curated by the",
                a("CAWP", href = "http://www.cawp.rutgers.edu", target = "_blank")),
                tags$p(a("View code",icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"))
                 )
        )


server <- function(input, output) {
   
        output$statebins <- renderPlotly({
                y_Axis <- list(
                        title = "", 
                        zeroline = FALSE,
                        showline = FALSE, 
                        showticklabels = FALSE, 
                        showgrid = FALSE
                )
                
                x_Axis <- list(
                        title = "", 
                        zeroline = FALSE,
                        showline = FALSE, 
                        showticklabels = FALSE, 
                        showgrid = FALSE
                )
                
                plot_ly(statedata, x = ~col, y = ~-row) %>%
                        add_markers(
                                color = ~bins,
                                colors = mapcolors,
                                text = ~txt,
                                symbol = I("square"), 
                                size = I(50),
                                hoverinfo = "text",
                                showlegend = FALSE
                        ) %>%
                        add_text(text = ~state, color = I("white"), hoverinfo = "none") %>%
                        add_annotations(
                                x= .5,
                                y= .95,
                                xref = "paper",
                                yref = "paper",
                                text = "Hover Over State For Details",
                                showarrow = F
                        ) %>%
                        layout(title = "", xaxis = x_Axis, yaxis = y_Axis)
                
        })
        
        output$data <- renderDT({
                if ("All" %in% input$State & "All" %in% input$Party)
                        candidates <- elections[, columns]
                else if ("All" %in% input$State & input$Party != "All")
                        elections[elections$Party %in% input$Party, columns]
                else if (input$State != "All" & "All" %in% input$Party)
                        elections[elections$State %in% input$State, columns]
                else
                        elections[elections$Party %in% input$Party & elections$State %in% input$State, columns]
                
        },
        escape = FALSE)
        
}

shinyApp(ui = ui, server = server)

