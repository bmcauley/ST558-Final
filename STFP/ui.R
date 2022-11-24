# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)

var_choice <- names(gFLT)

shinyUI(fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

  navbarPage("ST 558 Final Project",
             
             tabPanel("About", fluid = TRUE,
                      HTML("<center>
                            <h2>Welcome to the Genshin Character Modeling App!</h2>
                            <img src = 'https://pbs.twimg.com/media/Et6mA7TVgAA1qrl?format=jpg&name=large',
                            width = 1200px>
                            <h6><em>Author & Developer: Bennett McAuley <s>(Lvl 58 / 60 and a little obsessed)</s></em></h6>
                            <hr />
                           </center>"),
                      
                      fluidRow(
                        column(6,
                               h4("About the Application"),
                               
                               p("The purpose of this app is to demonstrate some of the capabilities R shiny provides for interactive data analysis.
                                 The basic premise is that each page of the interface serves as a step in the project process, from performing EDA to making predictions."),
                               
                               p("All pages contained within this app can be accessed via the nagivation menu at the top of the window.
                                 Each enables the following features:"),
                               tags$ul(
                                 tags$li(strong("Data Exploration"),
                                         "- Create numerical and graphical summaries using the data.
                                         You can change the type of plot and summary shown in the main portion of the page.
                                         You can also change the variables and filter rows to change the data used in the outputs."), 
                                 
                                 tags$li(strong("Modeling"), "- Contains three subpages with information about the models implemented (", code("Info"),
                                         "); functionality for building and fitting your own models (", code("Build"),");
                                         and options to select variables and obtain a prediction for the response (", code("Predict"),")."), 
                                
                                  tags$li(strong("Data"), "- Explore the data itself. You can scroll through the data; subset and filter rows and columns;
                                          and save a copy of the data to a CSV file in any configuration you'd like.")
                               )
                               ),
                        
                        column(6,
                               h4("About the Data Source"),

                               p("The data contains information and battle stats for all current (as of October 2022) playable characters from",
                                 em("Genshin Impact."),
                                 " There are 63 characters (observations) and 71 attributes (variables). The data is hosted on", a("kaggle by user sophiahealy", href = "https://www.kaggle.com/datasets/sophiahealy/genshin-impact-character-data"),
                                 ",who compiled the data from the",  a("Genshin Wiki", href = "https://genshin-impact.fandom.com/wiki/Genshin_Impact_Wiki"), "site."),
                               
                               p("Genshin Impact is a free-to-play, action role-playing game developed and published in September 2020 by HoYoverse, a multinational enterprise based in Shanghai.
                                 The game features an anime-style open-world environment and battle system involving elemental magic and character-switching.
                                 The base game is expanded regularly through patches using the ''Games-as-a-Service'' (GaaS) model."),
                                
                               p("Genshin takes place in the fantasy world of Teyvat, home to seven nations,
                                 each of which is tied to a different element and ruled by a different god. The story follows the Traveler,
                                 who has traveled across countless worlds with their twin sibling before becoming separated in Teyvat.
                                 The Traveler adventures in search of their lost sibling with their companion Paimon, and becomes involved in the affairs of Teyvat's nations.")
                               
                                 )
                        )
                      ),
             
            tabPanel("Data Exploration"),

             tabPanel("Modeling",
                        tabsetPanel(
                          tabPanel("Info"),
                          tabPanel("Build"),
                          tabPanel("Predict")
                        )
             ),
             tabPanel("Data",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("<h4>See the Data</h4>
                          <p>Use the <em>Columns</em> box below to select variables from the dataset
                            to appear in the table on the right. Click within the box to access the dropdown
                            containing all available variables. Actions:</p>
                            
                            <ul>
                              <li><b>Click</b> - Select/highlight a variable
                              <li><b><code>Ctrl</code> (hold) + Click</b> - Select more than one variable
                              <li><b><code>Shift</code> (hold) + Click</b> - Select all variables between 1st and 2nd clicked
                              <li><b><code>Backspace</code></b> - Delete the variable behind the cursor OR the selected variable(s)
                              <li><b><code>Delete</code></b> - Delete the selected variable(s)
                            </ul>"),
                          selectizeInput("vars", "Columns:",
                                                   choices = var_choice,
                                                  selected = var_choice[c(1:10,17:19)],
                                         multiple = TRUE),
                          
                          
                          downloadButton("genshinData", "Save")),
                        mainPanel(dataTableOutput("fullTable"))
                      )
             ),
            
             id = "input$About")
  
  )
)