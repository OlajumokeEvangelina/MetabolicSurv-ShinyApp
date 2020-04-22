#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# The required package
# need<-c("superpc", "glmnet", "matrixStats", "survminer", "survival", 
#         "rms", "dplyr", "pls", "Rdpack", "methods", "stats", "gplots", "ggplot2","gdata","foreign","readxl","httr","RCurl")
# 
# # Checking if those packages are available, if No install and load, if yes just load alone
# for(i in 1:length(need)){
#   if(require(need[i], character.only = TRUE)==FALSE){ 
#     install.packages(need[i]);library(need[i], character.only = TRUE)
#     } else { 
#     library(need[i],character.only = TRUE)}
# }
# 
# 
# if(require(pcaMethods)==FALSE){
#   need<-c('Rcpp',
#           'Matrix', 'cluster', 'foreign', 'lattice', 'mgcv',"RCurl","MetabolicSurv")
#   for(i in 1:length(need)){
#     if(require(need[i], character.only = TRUE)==FALSE){
#       install.packages(need[i],dependencies=TRUE)
#       library(need[i], character.only = TRUE)
#     } else { 
#       library(need[i],character.only = TRUE)
#     }
#   }
#   #dependancies
#   source("http://bioconductor.org/biocLite.R")
#   BiocManager::install("pcaMethods")
# }


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(shinythemes)


  header <- dashboardHeader(title = "MetabolicSurv")
  
  #Sidebar content of the dashboard
  sidebar <- dashboardSidebar(
 sidebarMenu(
   menuItem("Upload/Simulate Dataset", tabName = "dataset1", icon = icon("folder"),
 menuSubItem("Upload Dataset", tabName = "dataset", icon = icon("folder")),
 menuSubItem("Simulate Dataset", tabName = "simdataset", icon = icon("folder"))),
 menuItem("Metabolite Specific CoxPH", tabName = "MSpecificCoxPh", icon = icon("folder")),
 menuItem("Lasso/Elasticnet CoxPH", tabName = "Lasoelacox", icon = icon("folder"))
 ))

  ### END OF SIDEBAR 

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
  ,valueBoxOutput("value5")
  ,valueBoxOutput("value6")
)

frow11 <- fluidRow(
  valueBoxOutput("value11")
  ,valueBoxOutput("value21")
  ,valueBoxOutput("value31")
  ,valueBoxOutput("value41")
  ,valueBoxOutput("value51")
  ,valueBoxOutput("value61")
)

### END OF FROW ONE

frow2 <- fluidRow(  
  box(
    title = "Choose the variables to display summary for"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,sliderInput(inputId = "sliders",label = "",min = 1,max = 2,value = c(1,5), step=1,animate = animationOptions    (interval=1,loop = FALSE))
    ,width = 5
    , actionButton("sumsum","Run!")
  )
  ,box(
    title = "Data Summary"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,verbatimTextOutput("summa")
    ,width = 7
  ),
  box(
    title = "Dataset Preview"
    ,status = "primary"
    ,width = 12
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
  )
  )


frow22 <- fluidRow(  
  box(
    title = "Choose the variables to display summary for"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,sliderInput(inputId = "slidersi",label = "",min = 1,max = 2,value = c(1,5), step=1,animate = animationOptions    (interval=1,loop = FALSE))
    ,width = 5
    , actionButton("sumsim","Run!")
  )
  ,box(
    title = "Data Summary"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,verbatimTextOutput("summi")
    ,width = 7))


#frow3 <- fluidRow(htmlOutput("ArgSelect"))

### END OF FROW2

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dataset",
            tabBox(
              title = "", id = "tabset1", height = "750px",width = "350px",
              tabPanel("Uploading", fluidRow(box(title = "Choose the type of data format",
                                                 status = "primary"
                                                 ,solidHeader = TRUE 
                                                 ,collapsible = TRUE,selectInput(inputId = 'readFunction',
                                        label = 'Data type',
                                        choices = c(
                                          # Base R:
                                          "read.table",
                                          "read.csv",
                                          "read.csv2",
                                          "read.delim",
                                          "read.delim2",
                                          
                                          # foreign functions:
                                          "read.spss",
                                          "read.arff",
                                          "read.dbf",
                                          "read.dta",
                                          "read.epiiinfo",
                                          "read.mtp",
                                          "read.octave",
                                          "read.ssd",
                                          "read.systat",
                                          "read.xport",
                                          
                                          # Advanced functions:
                                          "scan",
                                          "readLines",
                                          
                                          # readxl functions:
                                          "read.xls",
                                          "read_excel"))),
                                        box(
                                            title = ""
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE 
                                            ,fileInput(inputId = 'file1', label = 'File upload')
                                          )),
                       
                       
                       fluidRow(
                         box(
                           title = "The function arguments"
                           ,status = "primary"
                           ,solidHeader = TRUE 
                           ,collapsible = TRUE 
                           ,htmlOutput("ArgSelect")
                           ,width = 4
                         ),
                         box(
                           title = "The argument values"
                           ,status = "primary"
                           ,solidHeader = TRUE 
                           ,collapsible = TRUE 
                           ,htmlOutput("ArgText")
                           ,width = 4
                         ),
                         box(
                           title = "The Dataset name"
                           ,status = "primary"
                           ,solidHeader = TRUE 
                           ,collapsible = TRUE 
                           ,textOutput("dataName")
                           ,width = 4
                         )),
                       
                       fluidRow(
                       box(
                         title = "The Column names"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = TRUE 
                         ,htmlOutput("varselect")
                         ,width = 12
                       ))
                       #textInput("name","Dataset name:","Data"),
                       
    
                       #fileInput(inputId = 'file1', label = 'File upload'),
                       
                       #uiOutput(outputId = "sliders")
              ),
              tabPanel("Data Summary", tabPanel("Dataset", frow1,frow2))
            )),
    # end of tabitem 2
    
    
    tabItem(tabName = "simdataset",
            tabBox(title = "", id = "tabset2", height = "750px",width = "350px",
            tabPanel("Simulation",fluidRow(box(
              title = "Generate Artificial Metabolic Survival Data",solidHeader = TRUE,
              status = "primary",
              p("Simulate metabolic profile of any number of patients and also their survival information and prognostic factor that will be used for data Analysis. The proportion of simulation must be between 0 and 1, also the dataset can also be downloaded to your computer."),
              hr(),
              numericInput("num1", label = "Number of Patients", value = 20),
              numericInput("num2", label = "Number of Metabolites", value = 100),
              numericInput("num3", label = "Proportion of simulation", value = 0.6,min=0.1, max=1,step = 0.1),
              actionButton("msdata","Run!"),
              p("Click the button to generate the dataset")
              )
            ,
            box(
              title = "Dataset Preview"
              ,status = "primary"
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,div(style = 'overflow-x: scroll', DT::dataTableOutput('table2')),
              downloadButton("downloadData", "Download")
            ))),
              tabPanel("Data Summary", frow11,frow22)))
    
    
    
    ,tabItem(tabName = "MSpecificCoxPh",h1("METABOLITE BY METABOLITE COX PROPORTIONAL ANALYSIS", style ="color : darkblue"),
              tabBox(title = "", id = "tabset3", height = "750px",width = "350px",
                     tabPanel("Arguments", fluidRow(
                       box(
                         title = "Description"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = FALSE 
                         ,p("This function fits Cox proportional hazard model for each metabolite in the dataset and then perform the classification based on the speciied quantile. Risk score is then obtained using a single metabolite.")
                         ,width = 12
                       )),fluidRow(box(
                         title = "Select the variables for analysis",solidHeader = TRUE
                         ,height = "600"
                         ,status = "primary", uiOutput("Survival"), uiOutput("Censor"),numericInput("num5", label = "Quantile", value = 0.5,min=0.25, max=1,step = 0.25),uiOutput("Prognostic"), 
                         sliderInput(inputId = "met2",label = "Metabolomic Matrix range",min = 1,max = 2,value = c(1,5), step=1,animate = animationOptions(interval=1,loop = FALSE)),uiOutput("Reduce"),checkboxInput("checkbox", label = "check if Reduce by SPCA is TRUE", value = FALSE),conditionalPanel("input.checkbox == true",numericInput("num4", label = "Number of metabolites selected from SPCA.", value = 10)),width = 4, actionButton("mspec","Run!")),box(
                           title = "Dataset Preview2"
                           ,status = "primary"
                           ,solidHeader = TRUE 
                           ,collapsible = TRUE 
                           ,width = 7
                           ,height = "600"
                           ,div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", DT::dataTableOutput('table3'))
                         ))),
                     tabPanel("Results", tabPanel("MSpecificCoxPh", fluidRow(  
                      box(
                         title = "Cox proportional analysis for each metabolites and prognostics"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = TRUE
                         ,numericInput("num6", label = "Which metabolite to display result for?", value = 10)
                         ,verbatimTextOutput("ex")
                         ,width = 7
                         ,height = "450"
                         ,actionButton("mm","Run!")
                       )
                      ,box(
                        title = "Hazard ratio information for each analysis"
                        ,status = "primary"
                        ,width = 5
                        ,height = "450"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,column(width = 12,
                                DT::dataTableOutput("table4"),style = "height:380px; overflow-y: scroll;overflow-x: scroll;"
                        )
                      )
                    ),fluidRow(
                      box(
                        title = "Risk group for selected metabolite"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE
                        ,verbatimTextOutput("group")
                        ,width = 12
                      )
                    )))
                     )),
    
    
    tabItem(tabName = "Lasoelacox",h1("LASSO, ELASTICNET AND RIDGE REGRESSION SURVIVAL ANALYSIS", style ="color : darkblue"),
            tabBox(title = "", id = "tabset4", height = "750px",width = "350px",
                   tabPanel("Arguements", fluidRow(
                     box(
                       title = "Description"
                       ,status = "primary"
                       ,solidHeader = TRUE
                       ,collapsible = FALSE
                       ,p("This is a wrapper function for glmnet and it fits models using either Lasso, Elastic net and Ridge regressions. This is done in the presence or absene of prognostic factors. The prognostic factor when avaialable will always be forced to be in the model so no penalty for it. Optimum lambda will be used to select the non-zero shrinkage coefficients, the nonzero selceted metabolites will thus be used in the survival analysis and in calculation of the risk scores.")
                       ,width = 12
                     )),fluidRow(width = 12,
                                 
                                 box(
                                   title = "Select the variables for analysis",solidHeader = TRUE
                                   , width=12
                                   ,status = "primary", column(width=3,uiOutput("Survivallasoelascox"))
                                                               , column(width=3,uiOutput("Censorlasoelascox"))
                    ,column(width=3,numericInput("num5", label = "Quantile", value = 0.5,min=0.25, max=1,step = 0.25))
                    ,column(width=3,uiOutput("Prognosticlasoelascox"))
                    ,column(width=2,numericInput("fold", label = "Fold", value = 4,min=1, max=10,step = 1))
                    ,column(width=3,uiOutput("Reducelasoelascox"))
                    ,column(width=2,selectInput("Plot",label = "Display plots",choices = list("TRUE", "FALSE"),selected = "FALSE"))
                    ,column(width=2,numericInput("alpha", label = "Mixing Parameter", value = 1,min=0, max=1,step = 0.1))
                    
                    ,column(width=3,numericInput("lambda", label = "Number of lambda values", value = 100,min=1, max=1000,step = 10))
                    ,column(width=3,selectInput("sd",label = "Standardize metabolomic matrix",choices = list("TRUE", "FALSE"),selected = "FALSE"))
                    ,column(width=4,sliderInput(inputId = "met3",label = "Choose metabolomic Matrix range",min = 1,max = 2,value = c(1,5), step=1,animate = animationOptions(interval=1,loop = FALSE))), br()
,column(3, offset=4,actionButton("lasoel","Run!"))
     )
     )),tabPanel("Results", tabPanel("Lasoelacox", fluidRow(  
       box(
         title = "Cox proportional analysis for selected metabolites and prognostics"
         ,status = "primary"
         ,solidHeader = TRUE 
         ,collapsible = TRUE
         ,verbatimTextOutput("ex2")
         ,width = 9
       )
     ))))

)

))

  #completing the ui part with dashboardPage

ui <- dashboardPage(header, sidebar, body, skin='blue')



  
  
























# create the server functions for the dashboard  
  server <- function(input, output, session) { 
    
    ArgNames <- reactive({
      Names <- names(formals(input$readFunction)[-1])
      Names <- Names[Names!="..."]
      return(Names)
    })
    
    # Argument selector:
    output$ArgSelect <- renderUI({
      if (length(ArgNames())==0) return(NULL)
      
      selectInput("arg","Argument:",ArgNames())
    })
    
    
    ## Arg text field:
    output$ArgText <- renderUI({
      fun__arg <- paste0(input$readFunction,"__",input$arg)
      
      if (is.null(input$arg)) return(NULL)
      
      Defaults <- formals(input$readFunction)
      
      if (is.null(input[[fun__arg]]))
      {
        textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
      } else {
        textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
      }
    })
    
    
    
    ### Data import:
    Dataset <- reactive({
      if (is.null(input$file1)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
      
      argList <- list()
      for (i in seq_along(args))
      {
        argList[[i]] <- eval(parse(text=input[[args[i]]]))
      }
      names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
      
      argList <- argList[names(argList) %in% ArgNames()]
      
      Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file1$datapath),argList)))
      return(Dataset)
    })
    
    output$dataName <- renderText({paste0(input$file1$name)})
    
    # Select variables:
    output$varselect <- renderUI({
      
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      
      # Variable selection:    
      selectInput("vars", "Variable names",
                  names(Dataset()), names(Dataset()), multiple =TRUE)            
    })
    
    
    observeEvent(Dataset(), {
      # if (is.null(input$file1)){
      #   df <- Dataset()
      # }
      df <- Dataset()
      maxi = ncol(df)
      updateSliderInput(session, "sliders", max=maxi)
    })
    
    suma <- eventReactive(input$sumsum,{
      # if (is.null(input$file1)){
      #   df <- Dataset()
      # }
      df <- Dataset()
      yes <- data.frame(df)
      summary(yes[,input$sliders[1]:input$sliders[2]])
    })
    
    output$summa <- renderPrint({
      suma()
    }) 
    
    
    
    # output$sliders <- renderUI({
    #   sliderInput(inputId = "ncov",
    #               label = "Number of covariates:",
    #               min = 1,
    #               max = as.numeric(ncol(Dataset())),
    #               value = c(1,as.numeric(ncol(Dataset()))), step=1,
    #               animate = animationOptions(interval=1,loop = FALSE))
    # })
    
    # Show table
   output$table <- DT::renderDataTable({
      
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
      
    return(DT::datatable(Dataset()[,1:as.numeric(ncol(Dataset()))],extensions = "Buttons",options = list(dom = 'Bfrtip', ordering = FALSE, searching=TRUE, scrollY="300px",scrollCollapse=TRUE,paging=FALSE,scrollX=TRUE, buttons = list("pdf","csv","excel","print"))))
    })
     
    
    # # Show summary:
    # output$print <- renderPrint({
    #   
    #   if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    #   
    #   return(summary(Dataset()[,input$ncov,drop=FALSE]))
    # })
    
    
    
    output$value1 <- renderValueBox({
   
      infoBox("Number", subtitle = "OF SUBJECTS", dim(Dataset())[1], icon = icon("credit-card"), color="blue")
      
    })
   
    
     output$value2 <- renderValueBox({
      
      infoBox("Number", subtitle = "OF VARIABLES", dim(Dataset())[2], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value3 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Dataset(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF FACTOR VARIABLES", k$Freq[k$Var1=="factor"], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value4 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Dataset(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF CHARACTER VARIABLES", k$Freq[k$Var1=="character"], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value5 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Dataset(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF NUMERIC VARIABLES", k$Freq[k$Var1=="numeric"], icon = icon("credit-card"), color="blue")
      
    })
    
    
    output$value6 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Dataset(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF INTERGER VARIABLES", k$Freq[k$Var1=="integer"], icon = icon("credit-card"), color="blue")
      
    })

    #################################################   SERVER FOR SIMULATE DATA
    
    datainter<- eventReactive(input$msdata, {
          val<- MetabolicSurv::MSData(nPatients=as.numeric(input$num1),nMet=as.numeric(input$num2),Prop=as.numeric(input$num3))
    })
    
    Pr <- reactive({
      val <- datainter()
      good <- data.frame(Indicator=val$Censor, Survtime = val$Survival, val$Prognostic, val$Mdata)
      good
    })
    
    
    output$table2 <- DT::renderDataTable({ DT::datatable({
       Pr()
        }) 
    })
    
    
    
    output$value11 <- renderValueBox({
      infoBox("Number", subtitle = "OF SUBJECTS", dim(Pr())[1], icon = icon("credit-card"), color="blue")
    }) 
    
    output$value21 <- renderValueBox({
      
      infoBox("Number", subtitle = "OF VARIABLES", dim(Pr())[2], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value31 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Pr(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF FACTOR VARIABLES", k$Freq[k$Var1=="factor"], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value41 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Pr(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF CHARACTER VARIABLES", k$Freq[k$Var1=="character"], icon = icon("credit-card"), color="blue")
      
    })
    
    output$value51 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Pr(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF NUMERIC VARIABLES", k$Freq[k$Var1=="numeric"], icon = icon("credit-card"), color="blue")
      
    })
    
    
    output$value61 <- renderValueBox({
      
      dat = data.frame(x= unname(sapply(Pr(),class)))
      
      k = as.data.frame(table(dat$x))
      
      infoBox("Number", subtitle = "OF INTERGER VARIABLES", k$Freq[k$Var1=="integer"], icon = icon("credit-card"), color="blue")
      
    })
    
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(Pr(), file, row.names = FALSE)
      }
    )
    
    observeEvent(Pr(), {
      df <- Pr()
      maxi = ncol(df)
      updateSliderInput(session, "slidersi", max=maxi)
    })
    
    
    
    sumi <- eventReactive(input$sumsim,{
      df <- Pr()
      yes <- data.frame(df)
      summary(yes[,input$slidersi[1]:input$slidersi[2]])
    })
    
    output$summi <- renderPrint({
      sumi()
    }) 
    
    
########################################################## server for metabolite by metabolite cox ph
    
    output$Survival <- renderUI({
      if (input$msdata){
      df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Survival","Select the Survival Variable",items)
    })
    
    output$Censor <- renderUI({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Censor","Select the Censor Variable",items)
    })
    
    output$Reduce <- renderUI({
      selectInput("Reduce",label = "Reduce by SPCA",choices = list("TRUE", "FALSE"),selected = "FALSE")
    })
    
    
    output$Prognostic <-renderUI({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Prognostic", "Select the Prognostics Variables",items, multiple = TRUE)
    })
    
    item <- reactive({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      names(df)
    })
    
   
    observeEvent(input$msdata, {
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      max = ncol(df)
      updateSliderInput(session, "met2", max=max)
    })
    
    ## update the sliders  for the metabolite dataframe
    observeEvent(Dataset(), {
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      max = ncol(df)
      updateSliderInput(session, "met2", max=max)
    })
    
    
    Pr2 <- eventReactive(input$mspec,{
      if (is.null(input$mspec)) return()
      if (input$mspec==0) return() 
      
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      good2 <- data.frame(df)
      a <- data.frame(good2[,input$met2[1]:input$met2[2]])
      b <- t(a)
      c <- good2[,input$Prognostic]
      d <- good2[,input$Survival]
      e <- good2[,input$Censor]
      return(list(a=a,b=b,c=c,d=d,e=e))
    })
    
    
    
    output$table3 <- DT::renderDataTable({ 
      if (is.null(input$mspec)) return()
      if (input$mspec==0) return()
      DT::datatable(round(Pr2()$a,5),options = list(pageLength = 10, lengthChange = FALSE,paging=FALSE))
    })
    
    riri <- reactive({
      if (is.null(input$mm)) return()
      if (input$mm==0) return()
      
      isolate({
        if (is.null(Pr2()$b)) return()
        MetabolicSurv::MSpecificCoxPh(Survival=Pr2()$d,Censor=Pr2()$e,Reduce=input$Reduce,Select=input$num4,Quantile=input$num5,Prognostic=Pr2()$c,Mdata=Pr2()$b)
     }) 
    }) 
    
    
    output$ex <- renderPrint({
      if (is.null(input$mm)) return()
      if (input$mm==0) return()
      ant <- riri()
      ant@Result[[input$num6]]
      })

    output$table4 <- DT::renderDataTable({ 
      if (is.null(input$mm)) return()
      if (input$mm==0) return()
      ant <- riri()
      DT::datatable(round(ant@HRRG,5),options = list(pageLength = 10, lengthChange = FALSE,paging=FALSE))
    })
    
    output$group <- renderPrint({
      if (is.null(input$mm)) return()
      if (input$mm==0) return()
      ant <- riri()
      as.character(ant@Group[input$num6,])
    })
   
########################################################## server for lasso elasticnet and ridge regrssion survival analysis
    
    output$Survivallasoelascox <- renderUI({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Survival","Select the Survival Variable",items)
    })
    
    output$Censorlasoelascox <- renderUI({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Censor","Select the Censor Variable",items)
    })
    
    
    output$Prognosticlasoelascox <-renderUI({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      items=names(df)
      names(items)=items
      selectInput("Prognostic", "Select the Prognostic Variables",items, multiple = TRUE)
    })
    
    item <- reactive({
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      names(df)
    })
    
    
    observeEvent(input$msdata, {
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      maxi = ncol(df)
      updateSliderInput(session, "met3", max=maxi)
    })
    
    ## update the sliders  for the metabolite dataframe
    observeEvent(Dataset(), {
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      maxi = ncol(df)
      updateSliderInput(session, "met3", max=maxi)
    })
    
    
    Pr3 <- eventReactive(input$lasoel,{
      if (is.null(input$lasoel)) return()
      if (input$lasoel==0) return() 
      
      if (input$msdata){
        df <- Pr()
      } else{
        df <- Dataset()
      }
      good3 <- data.frame(df)
      aa <- data.frame(good3[,input$met3[1]:input$met3[2]])
      bb <- t(aa)
      cc <- good3[,input$Prognosticlasoelascox]
      dd <- good3[,input$Survivallasoelascox]
      ee <- good3[,input$Censorlasoelascox]
      return(list(aa=aa,bb=bb,cc=cc,dd=dd,ee=ee))
    })
    
    
    laso <- reactive({
      if (is.null(input$lasoel)) return()
      if (input$lasoel==0) return()
      
      isolate({
        if (is.null(Pr2()$b)) return()
MetabolicSurv::Lasoelacox(Survival=Pr2()$d, Censor=Pr2()$e,
  Mdata=Pr2()$b, Prognostic = Pr2()$c, Quantile = input$num5, Metlist = NULL, 
  Plots = input$Plot, Standardize = input$sd, Alpha = input$alpha)
      }) 
    }) 
    
    
    output$ex2 <- renderPrint({
      if (is.null(input$lasoel)) return()
      if (input$lasoel==0) return()
      #lasoo <- laso()
      #lasso$SurvFit
      list(Pr3()$dd,Pr3()$ee)
    })
    
    
  }
shinyApp(ui, server)