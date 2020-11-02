library(shiny)
library(shinyjs)
library(recipes)
library(gridExtra)
library(ggplot2)


insurance <- read.csv('dataset/insurance.csv', header=TRUE)
trainingRowIndices <- sample(1:nrow(insurance), 0.8*nrow(insurance))
train <- insurance[trainingRowIndices, ]
test <- insurance[-trainingRowIndices, ]



ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Regression Model: Predict Insurance charges"),
  
  sidebarLayout(
    sidebarPanel(
      
      h2("Dataset Controls"),
      
      # Select features for training
      checkboxGroupInput("features", "Select Features for Training:",
                         names(insurance[, !names(insurance) %in% c("charges")]),
                         selected = names(insurance[, !names(insurance) %in% c("charges")])),
      
      # Target Variable
      disabled(textInput("targetVariable", "Target Variable", "Charges")),
      
      # Internal or External Data
      radioButtons("split", "Should Testing Data be Split from Dataset?:",
                   c("Yes" = TRUE,
                     "No" = FALSE)),
      
      # File Input for Testing Dataset
      fileInput("testDataset", "Choose Test Dataset",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Percentage Split
      disabled(textInput("percentageSplit", "Percentage of Data used for Training", "80%")),
    
      # Separator line
      hr(),
      tags$head(
        tags$style(HTML("hr {border-top: 2px solid #000000;}"))
      ),
      
      # For Scatter Plot
      h2("Plot Controls"),
      
      checkboxInput("scatterAge", "Show Age Plot", value = T),
      checkboxInput("scatterBmi", "Show BMI Plot", value = T),
      checkboxInput("scatterChildren", "Show Children Plot", value = T),
      sliderInput("wt1","Size of Age Plot",min=1,max=10,value=1),
      sliderInput("wt2","Size of BMI Plot",min=1,max=10,value=1),
      sliderInput("wt3","Size of Children Plot",min=1,max=10,value=1)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("About",htmlOutput("about")),
                  tabPanel("Raw Dataframe", DT::dataTableOutput("dataTable")),
                  tabPanel("Summary of Raw Dataset", verbatimTextOutput("rawDataSummary")),
                  tabPanel("Scatter Plot", column(3, plotOutput(outputId="scatterPlot", width="800px",height="400px"))), # Plot scatterplot
                  tabPanel("Histogram", column(3, plotOutput(outputId="histPlot", width="800px",height="400px"))), # Plot histogram
                  tabPanel("Trained Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Results", # results
                           tabsetPanel(type = "tabs",
                                       tabPanel("Predictions",tableOutput("predictions")),
                                       tabPanel("Accuracy Plot",plotOutput("accuracy")),
                                       tabPanel("Accuracy Score",verbatimTextOutput("accuracy_score"))
                           )
                  )
                  
      )     # tabsetpanel ends
    )       # mainpanel ends
  )         # sidebarlayout ends
)           # fluidlayout ends


server <- function(input, output, session) {
  
  # Split raw dataset for testing data?
  ds <- reactiveValues()
  
  observe({
    ds$split <- input$split
    ds$trainingRowIndices <- sample(1:nrow(insurance), 0.8*nrow(insurance))
    ds$train <- ifelse(ds$split == TRUE, insurance[ds$trainingRowIndices, ], insurance)
    ds$test <- ifelse(ds$split == TRUE, insurance[-ds$trainingRowIndices, ], insurance) # ERRONEOUS INPUT. CHANGE INSURANCE TO INPUT TEST DATASET !
  })
  
  # Data table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(insurance[, input$features, drop = FALSE])
  })
  
  # Raw data summary
  output$rawDataSummary <- renderPrint({
    summary(insurance[,input$features])
  })
  
  # Scatter plots
  scrplt1 <- reactive({
    if (!input$scatterAge) return(NULL)
      qplot(insurance$age, insurance$charges, color="red", main="Charges v. Age")
  })
  
  scrplt2 <- reactive({
    if (!input$scatterBmi) return(NULL)
    qplot(insurance$bmi, insurance$charges, color="blue", main="Charges v. BMI")
  })
  
  scrplt3 <- reactive({
    if (!input$scatterChildren) return(NULL)
    qplot(insurance$children, insurance$charges, color="green", main="Charges v. Children")
  })
  
  output$scatterPlot = renderPlot({
    ptlist <- list(scrplt1(),scrplt2(),scrplt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
  
  # Histogram plots
  histplt1 <- reactive({
    if (!input$scatterAge) return(NULL)
      qplot(insurance$age, fill=I("red"), main="Charges v. Age")
  })

  histplt2 <- reactive({
    if (!input$scatterBmi) return(NULL)
    qplot(insurance$bmi, fill=I("blue"), top="Charges v. BMI")
  })

  histplt3 <- reactive({
    if (!input$scatterChildren) return(NULL)
    qplot(insurance$children, fill=I("green"), top="Charges v. Children")
  })

  output$histPlot = renderPlot({
    ptlist <- list(histplt1(),histplt2(),histplt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)

    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))

  })

  # Model Summary
  lm_reg <- reactive(
    lm(as.formula(paste("charges"," ~ ",paste(input$features,collapse="+"))),data=train)
    #lm(as.formula(paste("charges", " ~ ", paste(list("age", "bmi", "children"),collapse="+"))),data=train)
  )
  
  output$summary = renderPrint({summary(lm_reg())})
  
  # Prediction Results
  pred_results <- reactive(
    distPred <- predict(lm_reg(), test)
  )
  
  # Accuracy
  accuracy <- reactive(
    cor(test[,"charges"],pred_results())
  )
  
  # Print accuracy score
  output$accuracy_score = renderPrint({accuracy()})
  
  # Print predictions
  output$predictions <- renderTable({
    pred_results()
  })
  
  # Plot accuracy
  output$accuracy <- renderPlot({
    plot(test[,"charges"],pred_results())
  })
  
 
  output$about <- renderUI({
    HTML('<h3> App background:</h3> This app is used for estimating the health insurance charges by considering the health and the overall background of an individual. The app uses linear regression to estimate the results. User can input the data, training features and the test data.
    The app displays data visualization plots and the regression results to the user. User can select different features and check the effect of feature selection on regression.<br><br>
    
     <h3>Data collected:</h3> I am using the insurance.csv dataset collected from Kaggle. The dataset contains 1338 observations (rows) and 7 features (columns). The dataset contains 4 numerical features (age, bmi, children and expenses) and 3 categorical features (sex, smoker and region)
    All the features together describe the health and the overall background of the individual.
    age: age of primary beneficiary <br> sex: insurance contractor gender <br> bmi: Body mass index <br> children: Number of children covered by health insurance <br>smoker: Smoking<br> region: the residential area.<br>charges: Individual medical costs billed by health insurance<br><br>
         
    <h3>Motivation:</h3> The purpose is to find the relationship between different features such as health, family background, lifestyle etc with the cost of insurance. The company can feed the test data and get the estimates of the charges for each individual. The data visualization plots in the app will also give a good background of clients to the insurance company.<br><br>
   
    <h3>Machine learning methodology:Multi variable linear regression</h3>
    <ul>
    <li>Steps: Split data in train and test </li>
    <li>Fit model on training data i.e.Find optimal coefficients by minimizing cost function.</li>
    <li>Predict results using test data</li>
    </ul>
    
    <h3>Multi variable linear regression:</h3> Linear regression is used to predict the value of an target variable Y based on one or more input predictor variables X. In linear regression, we establish the linear relationship between the predictor variable(s) and the response variable. Once the model is established, we can estimate the value of  Y, when only the predictors (Xs) values are known.<br><br>
    
    <h3>Mathematical Background:</h3> Linear regression equation:Y=β1+β2X+ϵ where, β1 is the intercept and β2 is the slope.It work on the principle of ordinary least square (OLS)/ Mean square errror (MSE).The goal is to minimize sum of square difference between observed dependent variable in the given data set and those predicted by linear regression fuction.
    For multivariable regression: Y = β1+β2X1+β3X2+β4X3+ϵ where x1, x2, x3 are all independent features.
    Cost Function to minimize: MSE=1/N ∑i=1n (yi−yp)2<br><br>
    
    <h3>Data analysis and results are shown in respective tabs</h3>')
  })
   
}

shinyApp(ui = ui, server = server)