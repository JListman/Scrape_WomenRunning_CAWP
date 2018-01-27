

library(shiny)
library(plotly)
library(DT)

if (!exists("statedata")) statedata = readRDS("data/statedata.rds")

columns <- c("State", "Candidate", "District", "Office", "Party")

ui <- fluidPage(
        titlePanel("Find Women Running for Federal or State Office"),
        tabsetPanel(type = "tabs",
                    tabPanel("Map", plotlyOutput("statebins")),
                    tabPanel("Searchable Table",
                             selectizeInput("Party", "Choose Party",
                                           c("Democrat" = "D",
                                             "Republican" = "R",
                                             "All" = "All"
                                             ),
                                           multiple = TRUE,
                                           selected = "D",
                                           width = '250px'
                                           ),
                             selectizeInput("State", "Choose State(s)",
                                            c("All"= "All", levels(elections$State)),
                                            multiple = TRUE,
                                            selected = "California",
                                            width = '250px'
                                            ),
                             DTOutput("data" )),
                    tabPanel("About",
                             fluidPage(
                                     column(12,
                                            h3("American Women Running for State or Federal Office"),
                                            p("This project was created with a database maintained by the",
                                              a("Center for American Women in Politics",
                                                href = "http://www.cawp.rutgers.edu", target = "_blank"), "at",
                                              a("Rutgers University.", 
                                                href = "https://www.rutgers.edu", target = "_blank"), 
                                              "The Center conducts research on women's participation in politics in America and maintains", 
                                              a("a list", 
                                                href = "http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive",
                                                target = "_blank"),
                                              "of women potentially running for US Congress and State offices. The data are current as of 1/25/18."),
                                            p("You can read more about this project on my", 
                                              a("blog.", href = "https://medium.com/@jblistman", target = "_blank")),
                                            p(a("R", href = "https://www.r-project.org", target = "_blank"),
                                              "code and details of data processing and visualization are available on",
                                              a("GitHub.", href = "https://github.com/jblistman", target = "_blank"),
                                              "Find me", a("Jenny Listman",
                                                      href = "https://twitter.com/jblistman", target = "_blank"),
                                              "on", a("Twitter", icon("twitter"),
                                                      href = "https://twitter.com/jblistman", target = "_blank"), "or",
                                              a("LinkedIn", icon("linkedin"), 
                                                href = "https://www.linkedin.com/in/jenniferlistman/", target = "_blank"))
                                            )
                                     )
                             )
                    ),
        tags$div(class="footer", checked=NA,tags$p("An interactive app with data from",
                a("The Center for American Women in Politics", href = "http://www.cawp.rutgers.edu")),
                tags$p(a("Code Available on GitHub", href = "http://github.org"))
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
                                size = I(35),
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
                        elections[elections$Party %in% input$Party, columns ]
                else if (input$State != "All" & "All" %in% input$Party)
                        elections[elections$State %in% input$State, columns]
                else
                        elections[elections$Party %in% input$Party & elections$State %in% input$State, columns]
                
        })
        
}

shinyApp(ui = ui, server = server)

