#USER INTERFACE DEFINITION FOR THE STFP APPLICATION

#----------START OF THE INTERFACE----------#

shinyUI(fluidPage(
  
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  navbarPage(
    "ST 558 Final Project",
    
#-----ABOUT PAGE-----#
    
    tabPanel("About",fluid = TRUE,
      HTML(
        "<center>
            <h3>Welcome to the Employee Attrition Modeling App!</h3>
        
            <img src=\"https://connect-assets.prosple.com/cdn/ff/CZPOCa3Dzl41-iEsysSbReDOcWdRMEbjbv3kOkXPcrk/1606438047/public/2020-11/banner-sas-1786x642-2020.png\",
            width = 800px/>
            
            <h6>Author & Developer: Bennett McAuley</h6>
        
            <hr />
         </center>"
      ),
      
      fluidRow(column(6,
                      h4("About the Application"),
                      
                      p(
                        "The purpose of this app is to demonstrate some of the capabilities R shiny provides for interactive data analysis.
                        The basic premise is that each page of the interface serves as a step in the project process, from performing EDA to making predictions."
                        ),
                      
                      p(
                        "All pages contained within this app can be accessed via the nagivation menu at the top of the window.
                        Each enables the following features:"
                        ),
                      
                      tags$ul(
                        tags$li(
                          strong("Data Exploration"),
                          "- Create numerical and graphical summaries using the data.
                          You can change the type of plot and summary shown in the main portion of the page.
                          You can also change the variables and filter rows to change the data used in the outputs."
                          ),
                        
                        tags$li(
                          strong("Modeling"),
                          "- Contains three subpages with information about the models implemented (",
                          code("Info"),
                          "); functionality for building and fitting your own models (",
                          code("Build"),");
                          and options to select variable values and obtain a prediction for the response (",
                          code("Predict"),")."
                          ),
                        
                        tags$li(
                          strong("Data"),
                          "- Explore the data itself. You can scroll through the data; subset and filter rows and columns;
                          and save a copy of the data to a CSV file in any configuration you'd like."
                          )
                        )
                      ),
               
               column(6,
                      h4("About the Data Source"),
        
        p(
          "The data consists of information gathered and sampled by IBM about their employee satisfaction, income, seniority, and other demographics.
          There are 1470 employees (observations) and--within the confines of this interface--30 attributes (variables). The response variable
          in the data is ",
          code("Attrition"),
          "--a binary variable with values `Yes` (an employee has left IBM) and `No` (an employee is still working at IBM).
          The version of the data used here is hosted on",
          a("kaggle", href = "https://www.kaggle.com/datasets/patelprashant/employee-attrition"),
          "For more information about the background and purpose of the data, see ",
          a("IBM's Developer Blog", href = 'https://developer.ibm.com/patterns/data-science-life-cycle-in-action-to-solve-employee-attrition-problem/'),
          "."
          ),
        
        p(
          "As an HR professional",
          HTML(
            "<s>and secretely disgruntled nerd that couldn't use his initial dataset of choice due to lack of volume</s>,"
          ),
          "I have significant investment in what makes employees want to stay at a company and what serves at the \"nail in the coffin\".
          Considering IBM is one of SAS's competitors, this is also an opportunity to explore solutions that could make yesterday's
          recent IBM mass-layoff victims into invaluable assets for SAS's tomorrow. #saslife"
          )
        ))
      ),
    
#-----DATA EXPLORATION PAGE-----#

    tabPanel("Data Exploration",
             
             titlePanel("EDA"),

# VARIABLE INPUTS AND GROUPING/FILTERING OPTIONS
             
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
                      </ul>"
                      ),
                 
          radioButtons(
            "plotType",
            "Plot Type:",
            c(Histogram = "density", Bar = "bar", Scatter = "point", Boxplot = "box")
            ),
          
          conditionalPanel(
            condition = "input.plotType == 'density'",
            selectInput("densityX","X Variable:", numVar)),
          
          conditionalPanel(
            condition = "input.plotType == 'bar'",
            selectInput("barX", "X Variable:", factVar)),
          
          conditionalPanel(
            condition = "input.plotType == 'point'",
            selectInput("pointX", "X Variable:", numVar, numVar[1]),
            selectInput("pointY", "Y Variable:", numVar, numVar[2])),
          
          conditionalPanel(
            condition = "input.plotType == 'box'",
            selectInput("boxX", "X Variable:", factVar, factVar[1]),
            selectInput("boxY", "Y Variable:", numVar, numVar[1])),
          
          checkboxGroupInput(
            "plotOpt", "Plot Options:",
            c('Group Fill' = 'fill','Facet Plot' = 'facet')
          ),
          
          conditionalPanel(
            condition = "input.plotOpt.includes('fill')",
            selectInput("groupVar", "Group By:", factVar, factVar[2])
          ),
          
          conditionalPanel(
            condition = "input.plotOpt.includes('facet')",
            selectInput("facetVar", "Facet By:", factVar, factVar[3])
          ),
          
          h6(em("Use the options below to generate custom numerical summaries of the data.")),
          
          radioButtons("tblType","Summary Type:",
            c('Correlation Matrix' = "corr", 'Five-Number Summary' = "fivenum",'Center/Spread' = "spr")
          ),
          
          conditionalPanel(
            condition = "input.tblType == 'corr'",
            selectizeInput("corrvars","Variables (Max. 6):", choices = numVar,
                           selected = numVar[1:5], multiple = TRUE, options = list(maxItems = 6))
          ),
          
          conditionalPanel(
            condition = "input.tblType == 'fivenum' || input.tblType == 'spr'",
            selectizeInput("sumVar", "Variable(s)", choices = numVar, multiple = TRUE,
                           selected = numVar[1:5]),
            
            radioButtons("tableGroup", "Summarize by grouping variable(s)? (Max. 2)",
                         c("No", "Yes")),
            
            conditionalPanel(
              condition = "input.tableGroup == 'Yes'",
              selectizeInput("tblGroupVar", "Group By:", choices = factVar, multiple = TRUE,
                             selected = factVar[1:2], options = list(maxItems = 2))
            )
          )
          
        ),
        
# OUTPUT FOR PLOTS AND SUMMARY TABLES

        mainPanel(plotOutput("ggplot"),
                  dataTableOutput("summTable"))
      )
      
    ),

#-----MODELING PAGE-----#
    
    tabPanel("Modeling",
             
             tabsetPanel(
               
#INFO SUBPAGE ABOUT MODELS AVAILABLE
               
               tabPanel("Info", fluid = TRUE,
                 h6(em("Before jumping into configuring your own models, read the information
                       below to learn about the available methods implemented. Since the response variable is
                       categorical, we are exclusively making use of algorithms suitable for classification.")
                 ),
                 
                 HTML(
                   "<center><h4>Logistic Regression</h4></center>
                   
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
                 
                 withMathJax(
                   "$$p(x)=\\frac{e^{\\beta_0+\\beta_1x}}{1+e^{\\beta_0+\\beta+1x}}$$ where \\(x\\) is a given predictor (or
                    linear combination of predictors) and \\(p(x)\\)is the probability of success. The model never goes below 0 or above 1,
                    but there is no closed form solution. In other words, the parameters (\\(\\beta\\)'s) are estimated using
                    maximum likelihood. For predictions, if \\(p(x) \\rightarrow 1\\)--above a defined threshold--then the response
                    is classified as a success. Otherwise, it's considered a failure."
                 ),
                 
                 fluidRow(column(6,
                                 HTML(
                                   "<center><br />
                                   
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
                                 HTML(
                                   "<center><br />
                                   
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
                 
                 p(
                   'Tree based methods for modeling follow a basic procedure: splitting
                   predictor values into different regions (branches), and assigning corresponding
                  observations that match the critiera of the splits to those branches. For
                  classification, the goal is to assign observations to groups. For a given
                  region (node), the class that appears most often among the observations within it
                  is laballed accordingly.'
                  ),
                 
                 withMathJax(
                   "How does the algorithm determine splits? For classification--and more specifically,
                   binary respones--it calculates the Gini index. The Gini index is characterized
                   by the following expression: $$Gini:2p(1-p)$$

                   where \\(p\\) is the probability of correct classification. For all possible
                   variable splits, the algorithm works to minimize the value of the index. If \\(p\\) is
                  close to 0 or 1, it will be very small, and that implies that the chosen variable to split on
                  makes the best (cleanest) classification of observations at some optimal value."
                 ),
                 
                 p(
                   'Trees will recursively chose variables to split on (using a Gini calculation) and
                    assign observations to a subsequent node down one of the branches of the split, based on their own values
                    of the variable.'
                   ),
                 
                 fluidRow(column(6,
                   HTML(
                     "<center>
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
                   HTML(
                     "<center>
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
                 
                 HTML(
                   "<center><h4>Random Forest</h4></center>
                   
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
                    </p>"
                 ),
                 
                 fluidRow(column(6,
                   HTML(
                     "<center>
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
                   HTML(
                     "<center>
                        <u><b>Cons</b></u>
                          
                        <p>
                          -Can be very slow and computationally intensive <br />
                          -Loses intepretability in favor of accuracy <br />
                          -Still generally bested by boosted trees <br />
                        </p>
                     
                      </center>"
                   )
                 ))
                 
               ),
               
# MODEL BUILDING SUBPAGE

               tabPanel( "Build",
                         
                 titlePanel("Build Models"),
                 
                 fluidRow(column(3,
                                 
                     wellPanel(
                       
                       h6("Use the slider below to set the proportion by which
                        you want to split the training and test data, respectively."),
                       
                       sliderInput("prop",
                         label = h6("Split Proportion"), min = 0, max = 1, value = 0.7,
                         step = 0.05
                       ),
                       
                       conditionalPanel(condition = "input.prop == 0",
                                        h6(em("So...build a model on nothing, then?"))
                                        ),
                       
                       conditionalPanel(condition = "input.prop == 1",
                                        h6(em("So...what is the model going to test on, exactly?"))
                                        ),
                       
                       conditionalPanel(condition = "input.prop < 0.5 && input.prop != 0",
                                        h6(em("You may want to go higher."))
                                        ),
                       
                       conditionalPanel(condition = "input.prop > 0.8 && input.prop != 1",
                                        h6(em("You may want to go lower."))
                                        ),
                       
                       HTML(
                         "<h6>Specify the respective settings for each model method in the columns to the right.</h6>

                          <h6>Click the <em><b>Build</b></em> button to obtain the approprtiate summaries and fit statistics.
                          The generated models can be applied on the <em>Predict</em> tab.</h6>"
                         ),
                       
                       
                       actionButton("modelBuild", "Build", icon = icon("gears"),
                                    style = "color: #000; background-color: #fff; border-color: #000"
                                    )
                     )
                   ),
 
# MODEL OPTIONS FOR LOGISTIC REGRESSION

                   column(3,
                     
                          wellPanel(
                            h5("Logistic Regression"),
                            selectizeInput("logVars", "Predictor Variables:", var_choice[-2],
                                           selected = var_choice[1:10], multiple = TRUE
                       ),
                       
                       checkboxGroupInput("logSettings", label = NULL,
                         c("Standardize Variables" = 'std', "Cross-Validation" = 'cv')
                         ),
                       
                       conditionalPanel(condition = "input.logSettings.includes('cv')",
                                        selectInput("logCV", "Number of folds:",
                                                    c(3, 5, 7, 10)))
                     )
                   ),
                   
# MODEL OPTIONS FOR CLASSIFICATION TREE

                   column(3,
                     
                          wellPanel(
                            h5("Classification Tree"),
                            selectizeInput(
                              "treeVars", "Predictor Variables:", var_choice[-2],
                              selected = var_choice[1:10], multiple = TRUE
                              ),
                            
                            checkboxInput("treeSettings",
                                          "Cross-Validation", value = FALSE),
                            
                            conditionalPanel(condition = "input.treeSettings == 1",
                                             selectInput("treeCV", "Number of folds",
                                                         c(3, 5, 7, 10))),
                            
                            numericInput("cp", "Value for cp parameter:", value = 0.01,
                                         min = 0, max = 1, step = 0.01
                                         ),
                       
                       HTML(
                         "<em>I'm not the most robust algorithm; if my <code>cp</code>
                       is too high--depending on the variables--I might not make any splits.
                       Please lower it if I only make 0's!</em>"
                       )
                     )
                   ),

# MODEL OPTIONS FOR RANDOM FOREST

                   column(3,
                          
                     wellPanel(
                       
                       h5("Random Forest"),
                       
                       selectizeInput("rfVars", "Predictor Variables:", var_choice[-2],
                                      selected = var_choice[1:10], multiple = TRUE
                       ),
                       
                       checkboxInput("rfSettings", "Cross-Validation"),
                       
                       conditionalPanel(condition = "input.rfSettings == 1", 
                                        selectInput("rfCV", "Number of folds",
                                                    c(3, 5, 7, 10))),
                       
                       numericInput(
                         "mtry", "Value for mtry parameter:", value = 1, min = 1,
                         max = 20, step = 1
                         )
                       )
                     )
                 ),

# OUTPUT FOR MODEL FIT SUMMARIES
                 
                 fluidRow(column(8, h4("Model Summaries"))),
                 
                 fluidRow(
                   column(6,
                          h6("LogReg"),
                          withSpinner(verbatimTextOutput("logModel")), type = 7),
                   
                   column(3,
                          h6("ClassTree"),
                          withSpinner(verbatimTextOutput("treeModel")), type = 7),
                   
                   column(3,
                          h6("RF"),
                          withSpinner(verbatimTextOutput("rfModel")), type = 7)
                 )
               ),

# PREDICTION SUBPAGE
               tabPanel("Predict",
                        
                 titlePanel("Make Predictions"),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     h6("Select a model to make preditions with."),
                     
                     selectInput("modelType", "Model Method:", choices = c(
                         "Logistic Regression" = 'logreg',
                         "Classification Tree" = 'ctree',
                         "Random Forest" = 'rf')
                         ),
                     
                     HTML(
                       "<h6>Use the fields below to set your desired values of the predictor variables
                        included in the model generated on <em>Build</em> tab. The values will populate/update
                       in the output to the right.</h6>

                       <h6>Click the <em><b>Predict</b></em> button at the bottom of the panel to obtain a prediction.
                       You can change the values and obtain another prediction by clicking it again.</h6>"
                       ),
                     
                     conditionalPanel(condition = "input.modelType == 'logreg'",
                                      uiOutput("logUI")),
                     
                     conditionalPanel(condition = "input.modelType == 'ctree'",
                                      uiOutput("treeUI")),
                     
                     conditionalPanel(condition = "input.modelType == 'rf'",
                                      uiOutput("rfUI")),
                     
                     actionButton("predict", "Predict", icon = icon("bars-progress"),
                                  style = "color: #000; background-color: #fff; border-color: #000"
                                  )
                   ),
                   
                   mainPanel(
                     conditionalPanel(condition = "input.modelType == 'logreg'",
                                      
                                      span(verbatimTextOutput("logEmploy"), style = "font-size: 20px"),
                                      
                                      span(textOutput("logResult"),
                                           style = "color:blue; background-color: black; font-family: Verdana; font-size: 20px")
                                      ),
                     conditionalPanel(condition = "input.modelType == 'ctree'",
                                      
                                      span(verbatimTextOutput("treeEmploy"), style = "font-size: 20px"),
                                      
                                      span(textOutput("treeResult"),
                                           style = "color:blue; background-color: black; font-family: Verdana; font-size: 20px")
                                      ),
                     
                     conditionalPanel(condition = "input.modelType == 'rf'",
                                      
                                      span(verbatimTextOutput("rfEmploy"), style = "font-size: 20px"),
                                      
                                      span(textOutput("rfResult"),
                                           style = "color:blue; background-color: black; font-family: Verdana; font-size: 20px")
                                      )
                     
                     )
                   )
                 )
             )),
    
#-----DATA EXPLORATION PAGE-----x

    tabPanel("Data",
             
      titlePanel("See the Data"),
      
      sidebarLayout(
        
        sidebarPanel(
          HTML(
            " <em>
              <h6>Use the <b>Columns</b> box below to select variables from the dataset
              to appear in the table on the right.</h6>

               <h6>Use the <b>Filter</b> box to enter a query to subset the rows by. For multiple
               conditions, use <b>&</b> (And) or <b>|</b> (Or) between expressions.</h6>

               <h6>Click the <b>Update</b> button to populate the table with your specified options. Click
               the <b>Save</b> button to download the data table in its current state.</h6>
              </em>

              <u>Actions</u>

              <ul>
                <li><b>Click</b> - Select/highlight a variable
                <li><b><code>Ctrl / Cmd</code> (hold) + Click</b> - Select more than one variable
                <li><b><code>Shift</code> (hold) + Click</b> - Select all variables between 1st and 2nd clicked
                <li><b><code>Backspace</code></b> - Delete the variable behind the cursor OR the selected variable(s)
                <li><b><code>Delete</code></b> - Delete the selected variable(s)
              </ul>"
            ),
          
          selectizeInput("tblVars", "Columns:", choices = var_choice, selected = var_choice[c(1:10)],
                         multiple = TRUE),
          
          textInput("tblRows","Filter",
                    placeholder = 'Enter a subsetting condition (e.g. Age > 40)'),
          
          actionButton(
            "updateTbl", "Update", icon = icon("rotate-right"),
            style = "color: #000; background-color: #fff; border-color: #000"
          ),
          
          downloadButton("employData", "Save", 
                         style = "color: #000; background-color: #fff; border-color: #000")
          ),
        
        mainPanel(dataTableOutput("fullTable"))
      )
    )
  )
  
))