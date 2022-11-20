# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)

shinyUI(fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

  navbarPage("STFP",
             
             tabPanel("About", fluid = TRUE,
                      fluidRow(
                        column(6,
                               h4(p("About the Project")),
                               h5(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                                    Morbi eget pellentesque turpis, nec egestas erat.
                                    Suspendisse porta odio ipsum, sed tempus turpis consectetur sit amet. 
                                    Pellentesque rhoncus elit at neque mollis sagittis.
                                    Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.
                                    Aenean vitae facilisis nisi. Etiam ornare nulla et nunc lacinia mattis.
                                    Nam dignissim arcu lacinia, mollis mauris sit amet, pretium elit.
                                    Nunc ut est eros. Aliquam vel tincidunt est.
                                    Donec condimentum arcu quis eros gravida, ultricies posuere turpis scelerisque.
                                    In hac habitasse platea dictumst. Nunc iaculis libero sed convallis aliquam."))
                               )
                        )
                      ),
             
            tabPanel("Data Exploration"),

             navbarMenu("Modeling",
                        tabPanel("Model Info"),
                        tabPanel("Model Fitting"),
                        tabPanel("Prediction")),
             tabPanel("Data"),
             id = "input$About")
  
  )
)