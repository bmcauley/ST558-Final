# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)

var_choice <- names(attrition)
numVar <- names(select_if(attrition, is.numeric))
factVar <- names(select_if(attrition, is.factor))

shinyUI(fluidPage(
  
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

  navbarPage("ST 558 Final Project",
             
             tabPanel("About", fluid = TRUE,
                      HTML("<center>
                              <h3>Welcome to the Employee Attrition Modeling App!</h2>
                              <img src=\"https://connect-assets.prosple.com/cdn/ff/CZPOCa3Dzl41-iEsysSbReDOcWdRMEbjbv3kOkXPcrk/1606438047/public/2020-11/banner-sas-1786x642-2020.png\",
                              width = 800px/>
                              <h6>Author & Developer: Bennett McAuley</h6>
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

                               p("The data consists of information gathered and sampled by IBM about their employee satisfaction, income, seniority, and other demographics.
                                There are 1470 employees (observations) and--within the confines of this interface--30 attributes (variables). The response variable
                                in the data is ", code("Attrition"), "--a binary variable with values `Yes` (an employee has left IBM) and `No` (an employee is still working at IBM).
                                The version of the data used here is hosted on", a("kaggle", href = "https://www.kaggle.com/datasets/patelprashant/employee-attrition"),
                                "For more information about the background and purpose of the data, see ", a("IBM's Developer Blog", href = 'https://developer.ibm.com/patterns/data-science-life-cycle-in-action-to-solve-employee-attrition-problem/'),"."                      
                                ),
                               
                               p("As an HR professional", HTML("<s>and secretely disgruntled nerd that couldn't use his initial dataset of choice due to lack of volume</s>,"),
                                 "I have significant investment in what makes employees want to stay at a company and what serves at the \"nail in the coffin\".
                                 Considering IBM is one of SAS's competitors, this is also an opportunity to explore solutions that could make yesterday's
                                 recent IBM mass-layoff victims into invaluable assets for SAS's tomorrow.")
                               )
                        )
                      ),
             
            tabPanel("Data Exploration",
                     
                     titlePanel("EDA"),
                     
                     sidebarLayout(
                       
                       sidebarPanel(
                         
                         HTML("<em><h6>
                              Use the options below to generate custom graphical summaries of the data.
                              The variables available per plot type are constrained by <code>ggplot</code>
                              aesthetic requirements:
                              </h6></em>
                              
                            <ul>
                              <li>Histogram - Single variable (continuous)
                              <li>Bar - single variable (discrete/categorical)
                              <li>Scatter - Two variables (both continous)
                              <li>Boxplot - Two variables (one continous, one discrete)
                            </ul>
                              "),
                         radioButtons("plotType",
                                      "Plot Type:",
                                      c(Histogram = "density",
                                        Bar = "bar",
                                        Scatter = "point",
                                        Boxplot = "boxplot")
                         ),
                         
                         conditionalPanel(
                           condition = "input.plotType == 'density'",
                           selectInput(
                             "plotVar",
                             "X Variable:",
                             numVar
                           ),
                           
                           radioButtons("fillOpt",
                                         "Fill with another variable? (creates density plot)",
                                         c("No", "Yes")),
                           
                           conditionalPanel(condition = "input.fillOpt == 'Yes'",
                                            selectInput(
                                              "fillVar",
                                              "Fill By:",
                                              factVar
                                            ))
                          
                         ),
                         
                         conditionalPanel(
                           condition = "input.plotType == 'bar'",
                           selectInput(
                             "barPlotVar",
                             "X Variable:",
                             factVar
                           ),
                           
                           checkboxInput("barfillOpt",
                                        "Include grouping variable"),
                           
                           conditionalPanel(condition = "input.barfillOpt == 1",
                                            selectInput(
                                              "barfillVar",
                                              "Group By:",
                                              factVar
                                              )
                                            ),
                           checkboxInput("barfacetOpt",
                                         "Include faceting variable"),
                           
                           conditionalPanel(condition = "input.barfacetOpt == 1",
                                            selectInput("barfacetVar",
                                                        "Facet By:",
                                                        factVar))
                         ),
                         
                         conditionalPanel(
                           condition = "input.plotType == 'point'",
                           selectizeInput(
                             "scatterX",
                             'X Variable:',
                             choices = numVar,
                             multiple = FALSE),
                           
                           selectizeInput(
                             "scatterY",
                             "Y Variable:",
                             choices = numVar,
                             multiple = FALSE
                           )
                           ),
                         
                         h6(em("Use the options below to generate custom numerical summaries of the data.")),
                         
                         radioButtons("tblType",
                                      "Summary Type:",
                                      c('Correlation Matrix' = "corr",
                                        'Five-Number Summary' = "fivenum",
                                        'Center/Spread' = "spr")),
                         
                         conditionalPanel(condition = "input.tblType == 'corr'",
                                          selectizeInput("corrvars", "Variables:",
                                                         choices = numVar,
                                                         selected = numVar[1:5],
                                                         multiple = TRUE)
                                          ),
                         
                         conditionalPanel(condition ="input.tblType == 'fivenum' || input.tblType == 'spr'",
                                          selectInput("sumVar",
                                                      "Variable:",
                                                      choices = numVar
                                          ),
                                          
                                          radioButtons("tableGroup",
                                                       "Summarize by grouping variable?",
                                                       c("No", "Yes")),
                                          
                                          conditionalPanel(condition = "input.tableGroup == 'Yes'",
                                                           selectInput(
                                                             "tblGroupVar",
                                                             "Group By:",
                                                             factVar
                                                           )
                                          )
                                          )

                        
                       ),
                       
                       mainPanel(
                         plotOutput("ggplot"),
                         dataTableOutput("summTable")
                       )
                     )
                    
                     ),

             tabPanel("Modeling",
                        tabsetPanel(
                          tabPanel("Info", fluid = TRUE,
                                   
                                   h6(em("Before jumping into configuring your own models, read the information
                                     below to learn about the available methods implemented. Since the response variable is
                                      categorical, we are exclusively making use of algorithms suitable for classification.")
                                      ),
                                   
                                   HTML("<center><h4>Logistic Regression</h4></center>
                                         <p>
                                          Broadly speaking, logistic regression is a model method that falls under the
                                          category of generalized linear models (GLMs). GLMs allow for responses from 
                                          non-normal distributions and can accomodate continuous and categorical predictors.
                                         </p>
                                                  
                                         <p>
                                           For basic logistic regression, the model response has one of two outcomes--
                                           \"success\" (<code>1</code>, <code>True</code>, <code>Yes</code>, <code>Heads</code>, etc.)
                                           or \"failure\" (<code>0</code>, <code>False</code>, <code>No</code>, <code>Tails</code>, etc.).
                                           The response follows a binomial distribution, so using a regular linear model is not the best choice
                                           for making predictions. Instead, logistic regression predicts the <em>probability</em> of an outcome
                                           using the following expression:
                                         </p>"
                                        ),
                                   
                                   withMathJax("$$p(x)=\\frac{e^{\\beta_0+\\beta_1x}}{1+e^{\\beta_0+\\beta+1x}}$$ where \\(x\\) is a given predictor (or 
                                                linear combination of predictors) and \\(p(x)\\)is the probability of success. The model never goes below 0 or above 1,
                                                but there is no closed form solution. In other words, the parameters (\\(\\beta\\)'s) are estimated using
                                                maximum likelihood. For predictions, if \\(p(x) \\rightarrow 1\\)--above a defined threshold--then the response
                                                is classified as a success. Otherwise, it's considered a failure."
                                               ),
                                   
                                   fluidRow(
                                     column(6, 
                                      HTML("<center><br />
                                      <u><b>Pros</b></u>
                                            <p>
                                              -Simple to implement and easy to interpret <br />
                                              -Very robust and makes no assumptions about distributions <br />
                                              -Can easily extend to multiple classes
                                            </p>
                                           </center>"
                                          )
                                      ),
                                      
                                     column(6,
                                      HTML("<center><br />
                                      <u><b>Cons</b></u>
                                              <p>
                                              -Requires little to no multicollinearity between predictors to perform optimally <br />
                                              -Contrary to its name, only beneficial/appropriate for classifying <br />
                                              -Can generally be outperformed by more complex models <br />
                                              </p>
                                           </center>"
                                           )
                                      )
                                     ),
                                   
                                   hr(),

                                    HTML("<center><h4>Classification Tree</h4></center>"),
                                            
                                    p('Tree based methods for modeling follow a basic procedure: splitting
                                      predictor values into different regions (branches), and assigning corresponding
                                      observations that match the critiera of the splits to those branches. For 
                                      classification, the goal is to assign observations to groups. For a given
                                      region (node), the class that appears most often among the observations within it
                                      is laballed accordingly.'
                                      ),
                                            
                                    withMathJax("How does the algorithm determine splits? For classification--and more specifically,
                                                binary respones--it calculates the Gini index. The Gini index is characterized
                                                by the following expression: $$Gini:2p(1-p)$$
                                                        
                                                where \\(p\\) is the probability of correct classification. For all possible
                                                variable splits, the algorithm works to minimize the value of the index. If \\(p\\) is
                                                close to 0 or 1, it will be very small, and that implies that the chosen variable to split on
                                                makes the best (cleanest) classification of observations at some optimal value."
                                                ),
                                   
                                    p('Trees will recursively chose variables to split on (using a Gini calculation) and
                                      assign observations to a subsequent node down one of the branches of the split, based on their own values
                                      of the variable.'
                                      ),
                                   fluidRow(
                                     column(6, 
                                            HTML("<center>
                                      <u><b>Pros</b></u>
                                            <p>
                                              -Simple to implement and output easy to interpret <br />
                                              -No statisical assumptions; predictors don't need to be standardized <br />
                                              -Built in variable selection
                                            </p>
                                           </center>"
                                            )
                                     ),
                                     
                                     column(6,
                                            HTML("<center>
                                      <u><b>Cons</b></u>
                                              <p>
                                              -Small changes in data can vastly change structure <br />
                                              -Has no optimal algorithm, very \"nearsighted\" and greedy when building <br />
                                              -Pruning usually needed to address overfitting <br />
                                              </p>
                                           </center>"
                                            )
                                     )
                                   ),
                                   
                                   hr(),
                
                                  HTML("<center><h4>Random Forest</h4></center>
                                        <p>
                                          Random forest is an ensemble tree method; it is a model comprised
                                          of <em>multiple</em> (tree) models. It works on the concept of bagging (short for Bootstrap Aggregation), which is a technique
                                          wherein the algorithm collects a random sample from the dataset, builds a model
                                          on that sample, and provides a prediction to be combined with all of the other
                                          predictions from each tree.
                                        </p>
                                                 
                                        <p>
                                          Symbolically, bagging follows this procedure for classification:
                                        </p>
                                                
                                        <ol>
                                          <li>Create boostrap sample of size \\(n\\) from \\(N\\) observations
                                          <li>Train tree on the sample, and call class prediction \\(\\hat{y}^{*}(x) \\) for given set of \\(x\\) values 
                                          <li>Repeat \\(B = 1000\\) times to obtain \\(\\hat{y}^{*j}(x), j=1,...,B \\)
                                          <li>Use <em>majority vote</em> as the final classification prediction for the whole ensemble; the class that
                                              is returned most often is the \"winner\"
                                        </ol>
                                                 
                                        <p>
                                          Where random forest differs from typical bagging, however, 
                                          is that it also randomly selects the variables to include in each tree. This controls bias since
                                          single and bagged trees will choose the same variable(s) to split on if they are deemed optimal,
                                          no matter how many times they are fit (assuming the data does not change).
                                        </p>"),
                                      
                                  fluidRow(
                                    column(6, 
                                           HTML("<center>
                                      <u><b>Pros</b></u>
                                            <p>
                                              -Generally performs better than single trees <br />
                                              -Decreases variance by averaging results across trees <br />
                                              -Bootstrapping and random feature selection makes it more robust against overfitting
                                            </p>
                                           </center>"
                                           )
                                    ),
                                    
                                    column(6,
                                           HTML("<center>
                                      <u><b>Cons</b></u>
                                              <p>
                                              -Can be very slow and computationally intensive <br />
                                              -Loses intepretability in favor of accuracy <br />
                                              -Still generally bested by boosted trees <br />
                                              </p>
                                           </center>"
                                           )
                                    )
                                  )

                                  ),
                          tabPanel("Build",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectizeInput("vars", "Columns:",
                                                      choices = var_choice,
                                                      selected = var_choice[c(1:10)],
                                                      multiple = TRUE)
                                     ),
                                     
                                     mainPanel()
                                     
                                   )
                          )
                                   ,
                          tabPanel("Predict")
                                   )
                          
                        ),
            
             tabPanel("Data",
                      titlePanel("See the Data"),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          HTML(" <em><h6>
                                    Use the <b>Columns</b> box below to select variables from the dataset
                                    to appear in the table on the right.</h6>
                                    
                                    <h6>
                                    Use the Search bar and/or the top selection row to subset the data to any value (or values) of each variable.
                                  </h6>
                                  
                                  
                                  <h6>  
                                    Click within the filter boxes to access dropdowns or sliders (depending on the data type)
                                    containing the variable names and options for values.
                                  </h6>  
                                  
                                  <h6>You can save the data in any configured state.</h6>
                                  </em>
                                    
                                    <u>Actions</u>
                            
                                  <ul>
                                    <li><b>Click</b> - Select/highlight a variable
                                    <li><b><code>Ctrl / Cmd</code> (hold) + Click</b> - Select more than one variable
                                    <li><b><code>Shift</code> (hold) + Click</b> - Select all variables between 1st and 2nd clicked
                                    <li><b><code>Backspace</code></b> - Delete the variable behind the cursor OR the selected variable(s)
                                    <li><b><code>Delete</code></b> - Delete the selected variable(s)
                                  </ul>
                               "),
                          
                          selectizeInput("vars", "Columns:",
                                                   choices = var_choice,
                                                  selected = var_choice[c(1:10)],
                                         multiple = TRUE),
                          
                          
                          downloadButton("employData", "Save")),
                        
                        mainPanel(dataTableOutput("fullTable"))
                      )
             )#,
            
             #id = "input$About"
            )
  
  )
)