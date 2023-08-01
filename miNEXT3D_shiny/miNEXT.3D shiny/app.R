library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(ggplot2)
library(ape)
library(shinycssloaders)
library(reshape2)
library(iNEXT.3D)
library(miNEXT.3D)
library(devtools)
library(DT)
library(phytools)


ui <- navbarPage("miNEXT3D",
                 
                 # TD ----------------------------------------------------------------------
                 
                 tabPanel("Mixture Taxonomic Diversity", icon = icon("bug"),
                          fluidPage(fluidRow(
                            h1("Mixture Taxonomic Diversity"),
                            column(2,
                                   img(src = "final.PNG", height = 125, width = 125),
                                   
                                   actionButton("go_TD", "Run!", style="font-size: 30px", icon("rocket")),
                                   p(h3("Data setting")),
                                   wellPanel(
                                     radioButtons("what_data", "Choose one:", 
                                                  c("Demo data"="demo", "Upload data"="upload" )),
                                     uiOutput("selectdata"),
                                     uiOutput("select_Ori_Assemblage"),
                                     uiOutput("select_Trans_Assemblage")
                                     # uiOutput("selectAssemblage"),
                                     # p("Using ctrl / command key to select Two datasets, the dataset with more species will be the original assemblage.")

                                   ),
                                   p(h3("General setting")),
                                   wellPanel(
                                     # radioButtons("q", "Diversity order q:", 
                                     #              c("q = 0" = 0, "q = 1" = 1, "q = 2" = 2), selected = 0),
                                     
                                     radioButtons("knots_choose", "Choose one:", 
                                                  c("Specify # of knots" = 0, "Specify sample sizes" = 1), selected = 0),
                                     
                                     uiOutput("knots_set"),
                                     numericInput("b", "Number of bootstraps (Time consuming, enter '0' to skip bootstrap)",0),
                                     numericInput("conf", "Level of confident interval", 0.95)
                                     
                                   )
                            ),
                            
                            column(10,
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Data summary", icon = icon("table"),
                                                br(),
                                                ## Data view
                                                fluidRow(
                                                  h3("View data"),
                                                  dataTableOutput('view_data_TD', width = "80%") %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                                                  h5("Download the data as .csv file"),
                                                  downloadButton('download_data_TD','Download')
                                                ),
                                                br(),
                                                ## Data information
                                                fluidRow(
                                                  h3("Data information"),
                                                  dataTableOutput("data_summary_TD") %>% withSpinner(color="#0dc5c1"),
                                                  h5("Download the data information as .csv file"),
                                                  downloadButton('download_Datainf_TD','Download'),
                                                  h4("Notes:"),
                                                  tags$li("n = number of observed individuals in the reference sample (sample size)."),
                                                  tags$li("S.obs = number of observed species in the reference sample."),
                                                  tags$li("SC = estimator of the sample coverage of the reference sample."),
                                                  tags$li("f1-f10 = the first ten species abundance frequency counts in the reference sample.")
                                                )
                                                
                                       ),
                                       tabPanel("Mixture Rarefaction and Extrapolation", icon = icon("book"),
                                                selectInput("Assemblage_choose","Select Assemblage:",
                                                            choices = c("Mixture" = 1,
                                                                        "Original assemblage" = 2,
                                                                        "Transform assemblage" = 3), selected = 1),
                                                uiOutput("minext_TDout"),
                                                uiOutput("minext_TDdownout"),
                                                h4("Notes:"),
                                                tags$li("Order.q = Diversity order."),
                                                tags$li("m1 = Number of individuals in the original reference sample."),
                                                tags$li("m2 = Number of individuals in the transform reference sample."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("qmiNEXT_TD = Estimated mixed taxonomic diversity."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the diversity of order q at the specified level (with a default value of 0.95)."),
                                                tags$li("linetype = 'Rarefaction' means the mixture of two rarefaction curves.
                                                        'Extrapolation' means the mixture of one rarefaction curve and one extrapolation curve."),
                                                tags$li("Assemblage = The select assemblage name.'Mixture' means the mixture of two assemblage.")

                                       ),
                                       tabPanel("Decomposition Analysis", icon = icon("book-open"),
                                                dataTableOutput("decom_TD")%>% withSpinner(color="#0dc5c1"),
                                                downloadButton("decom_down_TD", "Download"),
                                                h4("Notes: The decomposition can be evaluated only for the mixture of two rarefaction curves."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("Type = 'q0_un1' means the unique species in the original reference sample.
                                                        'q0_un2' means the unique species in the transform reference sample.
                                                        'q0_sh' means the shared species in two reference sample."),
                                                tags$li("Value = Estimated composition."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the composition at the specified level (with a default value of 0.95).")
                                       ),
                                       tabPanel("Figure plots", icon = icon("images"),
                                                fluidRow(
                                                  h2("(1) Mixture rarefaction and extrapolation sampling curve"),
                                                  plotOutput("type1_TD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot1_TD", "Download"),
                                                  h2("(2) Composition of q = 0"),
                                                  plotOutput("type2_TD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot2_TD", "Download")
                                                )
                                                
                                       )
                                     )
                                     
                                   )
                            )
                            
                            
                          ))),
                 # PD ----------------------------------------------------------------------
                 
                 tabPanel("Mixture Phylogenetic Diversity", icon = icon("tree"),
                          fluidPage(fluidRow(
                            h1("Mixture Phylogenetic Diversity"),
                            
                            column(2,
                                   img(src = "final.PNG", height = 125, width = 125),
                                   
                                   actionButton("go_PD", "Run!", style="font-size: 30px", icon("rocket")),
                                   p(h3("Data setting")),
                                   wellPanel(
                                     radioButtons("what_data_PD", "Choose one:", 
                                                  c("Demo data"="demo", "Upload data"="upload")),
                                     # radioButtons("type_PD", "Choose diversity type:", 
                                     #              c("PD" = "PD", "meanPD" = "meanPD"), selected = "PD"),
                                     uiOutput("selectdata_PD"),
                                     uiOutput("select_Ori_Assemblage_PD"),
                                     uiOutput("select_Trans_Assemblage_PD"),
                                     # uiOutput("selectAssemblage_PD"),
                                     # p("Using ctrl / command key to select multiple datasets you want"),
                                     uiOutput("select_tree")
                                     
                                   ),
                                   p(h3("General setting")),
                                   wellPanel(
                                     # radioButtons("q_PD", "Diversity order q:", 
                                     #              c("q = 0" = 0, "q = 1" = 1, "q = 2" = 2), selected = 0),
                                     
                                     radioButtons("knots_choose_PD", "Choose one:", 
                                                  c("Specify # of knots" = 0, "Specify sample sizes" = 1), selected = 0),
                                     
                                     uiOutput("knots_set_PD"),
                                     numericInput("b_PD", "Number of bootstraps (Time consuming, enter '0' to skip bootstrap)",0),
                                     numericInput("conf_PD", "Level of confident interval", 0.95)
                                     
                                   )
                            ),
                            
                            column(10,
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Data summary", icon = icon("table"),
                                                br(),
                                                ## Data view
                                                fluidRow(
                                                  h3("View data"),
                                                  dataTableOutput('view_data_PD', width = "80%") %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                                                  h5("Download the data as .csv file"),
                                                  downloadButton('download_data_PD','Download')
                                                ),
                                                br(),
                                                ## Data information
                                                fluidRow(
                                                  h3("Data information"),
                                                  dataTableOutput("data_summary_PD") %>% withSpinner(color="#0dc5c1"),
                                                  h5("Download the data information as .csv file"),
                                                  downloadButton('download_Datainf_PD','Download'),
                                                  h4("Notes:"),
                                                  tags$li("n = number of observed individuals in the reference sample (sample size)."),
                                                  tags$li("S.obs = number of observed species in the reference sample."),
                                                  tags$li("PD.obs = sum of observed branch lengths in the reference sample."),
                                                  tags$li("f1*, f2* = the first two nodes frequency counts in the reference sample."),
                                                  tags$li("g1, g2 = sum of the first two nodes branch lengths in the reference sample."),
                                                  tags$li("Reftime = the age of the root of the tree.")
                                                )
                                                
                                       ),
                                       
                                       tabPanel("Mixture Rarefaction and Extrapolation",icon = icon("book"),
                                                selectInput("Assemblage_choose_PD","Select Assemblage:",
                                                            choices = c("Mixture" = 1,
                                                                        "Original assemblage" = 2,
                                                                        "Transform assemblage" = 3), selected = 1),
                                                uiOutput("minext_PDout"),
                                                uiOutput("minext_PDdownout"),
                                                h4("Notes:"),
                                                tags$li("Order.q = Diversity order."),
                                                tags$li("m1 = Number of individuals in the original reference sample."),
                                                tags$li("m2 = Number of individuals in the transform reference sample."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("qmiNEXT_PD = Estimated mixed phylogenetic diversity."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the diversity of order q at the specified level (with a default value of 0.95)."),
                                                tags$li("linetype = 'Rarefaction' means the mixture of two rarefaction curves.
                                                        'Extrapolation' means the mixture of one rarefaction curve and one extrapolation curve."),
                                                tags$li("Assemblage = The select assemblage name.'Mixture' means the mixture of two assemblage.")
                                       ),
                                       tabPanel("Decomposition Analysis",icon = icon("book-open"),
                                                dataTableOutput("decom_PD")%>% withSpinner(color="#0dc5c1"),
                                                downloadButton("decom_down_PD", "Download"),
                                                h4("Notes: The decomposition can be evaluated only for the mixture of two rarefaction curves."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("Type = 'q0_un1' means the unique lineages in the original reference sample.
                                                        'q0_un2' means the unique lineages in the transform reference sample.
                                                        'q0_sh' means the shared lineages in two reference sample."),
                                                tags$li("Value = Estimated composition."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the composition at the specified level (with a default value of 0.95).")
                                       ),
                                       tabPanel("Figure plots", icon = icon("images"),
                                                fluidRow(
                                                  h2("(1) Mixture rarefaction and extrapolation sampling curve"),
                                                  plotOutput("type1_PD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot1_PD", "Download"),
                                                  h2("(2) Composition of q = 0"),
                                                  plotOutput("type2_PD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot2_PD", "Download")
                                                )
                                                
                                       )
                                     )
                                     
                                   )
                            )
                            
                            
                          ))),
                 
                 # FD ----------------------------------------------------------------------
                 
                 tabPanel("Mixture Functional Diversity", icon = icon("lungs"),
                          fluidPage(fluidRow(
                            h1("Mixture Functional Diversity"),
                            column(2,
                                   img(src = "final.PNG", height = 125, width = 125),
                                   
                                   actionButton("go_FD", "Run!", style="font-size: 30px", icon("rocket")),
                                   p(h3("Data setting")),
                                   wellPanel(
                                     radioButtons("what_data_FD", "Choose one:", 
                                                  c("Demo data"="demo", "Upload data"="upload")),
                                     uiOutput("selectdata_FD"),
                                     uiOutput("select_Ori_Assemblage_FD"),
                                     uiOutput("select_Trans_Assemblage_FD"),
                                     # uiOutput("selectAssemblage_FD"),
                                     # p("Using ctrl / command key to select multiple datasets you want"),
                                     uiOutput("select_dis")
                                     
                                   ),
                                   p(h3("General setting")),
                                   wellPanel(
                                     # radioButtons("q_FD", "Diversity order q:", 
                                     #              c("q = 0" = 0, "q = 1" = 1, "q = 2" = 2), selected = 0),
                                     radioButtons("choose_tau", "Choose threshold type:",
                                                  choices = c("single threshold" = "tau_values", "AUC" = "AUC"), selected = "tau_values"),
                                     uiOutput("tau_set_FD"),
                                     radioButtons("knots_choose_FD", "Choose one:", 
                                                  c("Specify # of knots" = 0, "Specify sample sizes" = 1), selected = 0),
                                     
                                     uiOutput("knots_set_FD"),
                                     numericInput("b_FD", "Number of bootstraps (Time consuming, enter '0' to skip bootstrap)",0),
                                     numericInput("conf_FD", "Level of confident interval", 0.95)
                                   )
                            ),
                            column(10,
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Data summary", icon = icon("table"),
                                                br(),
                                                ## Data view
                                                fluidRow(
                                                  h3("View data"),
                                                  dataTableOutput('view_data_FD', width = "80%") %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                                                  h5("Download the data as .csv file"),
                                                  downloadButton('download_data_FD','Download')
                                                ),
                                                br(),
                                                ## Data information
                                                fluidRow(
                                                  h3("Data information"),
                                                  dataTableOutput("data_summary_FD") %>% withSpinner(color="#0dc5c1"),
                                                  h5("Download the data information as .csv file"),
                                                  downloadButton('download_Datainf_FD','Download'),
                                                  h4("Notes:"),
                                                  tags$li("n = number of observed individuals in the reference sample (sample size)."),
                                                  tags$li("S.obs = number of observed species in the reference sample."),
                                                  tags$li("SC = estimator of the sample coverage of the reference sample."),
                                                  tags$li("a1'-a10' = the first ten functional group abundance frequency counts in the reference sample."),
                                                  tags$li("Tau = the threshold level (with a default value quadratic entropy)."),
                                                  tags$li("dmin, dmean, dmax = the threshold level.
                                                  'dmin' means the minimum distance (except 0) in the distance matrix.
                                                  'dmean' means the quadratic entropy.
                                                  'dmax' means the maximum distance in the distance matrix.")
                                                )
                                                
                                       ),
                                       tabPanel("Mixture Rarefaction and Extrapolation",icon = icon("book"),
                                                selectInput("Assemblage_choose_FD","Select Assemblage:",
                                                            choices = c("Mixture" = 1,
                                                                        "Original assemblage" = 2,
                                                                        "Transform assemblage" = 3), selected = 1),
                                                uiOutput("minext_FDout"),
                                                uiOutput("minext_FDdownout"),
                                                h4("Notes:"),
                                                tags$li("Order.q = Diversity order."),
                                                tags$li("m1 = Number of individuals in the original reference sample."),
                                                tags$li("m2 = Number of individuals in the transform reference sample."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("qmiNEXT_FD/qmiNEXT_FD_singletau = Estimated mixed functional diversity."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the diversity of order q at the specified level (with a default value of 0.95)."),
                                                tags$li("linetype = 'Rarefaction' means the mixture of two rarefaction curves.
                                                        'Extrapolation' means the mixture of one rarefaction curve and one extrapolation curve."),
                                                tags$li("Assemblage = The select assemblage name.'Mixture' means the mixture of two assemblage.")
                                       ),
                                       tabPanel("Decomposition Analysis",icon = icon("book-open"),
                                                dataTableOutput("decom_FD")%>% withSpinner(color="#0dc5c1"),
                                                downloadButton("decom_down_FD", "Download"),
                                                h4("Notes: The decomposition can be evaluated only for the mixture of two rarefaction curves."),
                                                tags$li("prop.v = The proportion of the original assemblage, that is m1/n1."),
                                                tags$li("Type = 'q0_un1' means the unique functional groups in the original reference sample.
                                                        'q0_un2' means the unique functional groups in the transform reference sample.
                                                        'q0_sh' means the shared functional groups in two reference sample."),
                                                tags$li("Value = Estimated composition."),
                                                tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for 
                                                        the composition at the specified level (with a default value of 0.95).")
                                       ),
                                       tabPanel("Figure plots",icon = icon("images"),
                                                fluidRow(
                                                  h2("(1) Mixture rarefaction and extrapolation sampling curve"),
                                                  plotOutput("type1_FD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot1_FD", "Download"),
                                                  h2("(2) Composition of q = 0"),
                                                  plotOutput("type2_FD")%>% withSpinner(color="#0dc5c1"),
                                                  downloadButton("plot2_FD", "Download")
                                                  
                                                )
                                                
                                       )
                                     )
                                   ))
                          ))
                          
                          
                          
                 )
                 
                 
                 
                 
                 
                 
                 
                 
)

# Define server logic ----
server <- function(input, output) {
  
  # TD ----------------------------------------------------------------------
  
  output$selectdata <- renderUI({ 
    if(input$what_data == "demo"){
      radioButtons("data", "Choose demo data:", c("dunes data"="1"), selected = "1")
    }else{
      fileInput("data", "Upload data (one csv file):", accept = "text")
    }
  }) 

  output$select_Ori_Assemblage <- renderUI({
    if(input$what_data == "demo"){
      if(input$data == "1"){
        data = miNEXT.3D::dunes$data
        # data = data.frame(read.csv("abu/dunes_data.csv",row.names = 1, check.names = F))
        selectInput("Ori_Assemblage", "Select Original Assemblage:", choices=names(data), selected=names(data)[1], multiple = F, selectize=FALSE)
      }
    }else{
      if(is.null(input$data)){
        NULL
      }else{
        infile <- input$data
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Ori_Assemblage", "Select Original Assemblage:", choices=names(data), selected=names(data)[1], multiple = F, selectize=FALSE)
      }
    }
  })
  
  output$select_Trans_Assemblage <- renderUI({
    if(input$what_data == "demo"){
      if(input$data == "1"){
        data = miNEXT.3D::dunes$data
        # data = data.frame(read.csv("abu/dunes_data.csv",row.names = 1, check.names = F))
        selectInput("Trans_Assemblage", "Select Transform Assemblage:", choices=names(data), selected=names(data)[2], multiple = F, selectize=FALSE)
      }
    }else{
      if(is.null(input$data)){
        NULL
      }else{
        infile <- input$data
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Trans_Assemblage", "Select Transform Assemblage:", choices=names(data), selected=names(data)[2], multiple = F, selectize=FALSE)
      }
    }
  })
  
  output$knots_set <- renderUI({
    if(input$knots_choose == 0){
      numericInput("endpoint", "knots setting", 11, min = 5)
    }else{
      textAreaInput("endpoint", "Specify sample size:", rows = 2, value = " ")
    }
  })
  
  # run ---------------------------------------------------------------------
  what_data <- eventReactive(input$go_TD,{input$what_data})
  ass <- eventReactive(input$go_TD, {c(input$Ori_Assemblage,input$Trans_Assemblage)})
  data <- eventReactive(input$go_TD, {
    if(input$what_data == "demo"){
      if(input$data == "1"){
        data = miNEXT.3D::dunes$data
      }
    }else{
      infile <- input$data
      data = read.csv(infile$datapath, row.names = 1)
    }
    return(data)
  })
  
  go_data <- eventReactive(input$go_TD,{
    ass <- ass()
    data <- data()
    what_data <- what_data()
    go_data = data[c(ass)]
    names(go_data) <- ass
    return(go_data)
  })
  
  # q <- eventReactive(input$go_TD, {input$q})
  end2 <- eventReactive(input$go_TD, {input$knots_choose})
  end <- eventReactive(input$go_TD, {
    end2 <- end2()
    if(end2 == 0){
      out <- input$endpoint
    }else{
      out <- strsplit(input$endpoint, "\\s+")[[1]]
      out <- as.numeric(out)
      out <- out[!is.na(out)]
    }
  })
  b <- eventReactive(input$go_TD, {input$b})
  conf <- eventReactive(input$go_TD, {input$conf})
  
  ## Data summary 
  TD_infor <-eventReactive(input$go_TD,{
    
    out <- DataInfo3D(data = go_data(),
                     diversity = "TD",
                     datatype = "abundance")
    
  })
  
  output$data_summary_TD <- renderDataTable({
    TD_infor() %>% data.frame()
  })
  
  output$view_data_TD <- renderDataTable({
    data = go_data()
    data %>%
      DT::datatable(options = list(bFilter=T, scrollX = TRUE), rownames = T)
  })
  
  output$download_data_TD <- downloadHandler(
    filename = function() {
      paste('Raw data', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- go_data()
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_Datainf_TD <- downloadHandler(
    filename = function() {
      paste('DataInformation_TD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- data.frame(TD_infor())
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  ## Mixture
  TD <-eventReactive(input$go_TD,{
    if(end2() == 0){
      out <- miNEXT3D(data = go_data(),
                      diversity = 'TD',
                      q = c(0,1,2),
                      knots = end(),
                      nboot = b(),
                      conf = conf())
      
    }else{
      out <- miNEXT3D(data = go_data(),
                      diversity = 'TD',
                      q = c(0,1,2),
                      size = end(),
                      nboot = b(),
                      conf = conf())
    }
  })
  
  plot_TD <- eventReactive(input$go_TD,{
    ggmiNEXT3D(TD())
  }) 
  
  output$minext_TDout <- renderUI({
    if(input$Assemblage_choose == 1){
      dataTableOutput("Mixture_TD") %>% withSpinner(color="#0dc5c1")
    }else if(input$Assemblage_choose == 2){
      dataTableOutput("Ori_TD") %>% withSpinner(color="#0dc5c1")
    }else{
      dataTableOutput("Trans_TD") %>% withSpinner(color="#0dc5c1")
    }
  })
  
  output$minext_TDdownout <- renderUI({
    if(input$Assemblage_choose == 1){
      downloadButton("Mixture_TD_down", "Download")
    }else if(input$Assemblage_choose == 2){
      downloadButton("Ori_TD_down", "Download")
    }else{
      downloadButton("Trans_TD_down", "Download")
    }
  })
  
  output$Mixture_TD <- renderDataTable({
    TD()$Mixture[,-8]
  })
  
  output$Ori_TD <- renderDataTable({
    TD()$Original[,-7]
  })
  
  output$Trans_TD <- renderDataTable({
    TD()$Transform[,-7]
  })
  
  output$Mixture_TD_down <- downloadHandler(
    filename = function() {
      paste('mixture_TD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- TD()$Mixture
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Ori_TD_down <- downloadHandler(
    filename = function() {
      paste('Original_TD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- TD()$Original
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Trans_TD_down <- downloadHandler(
    filename = function() {
      paste('Transform_TD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- TD()$Transform
      write.csv(out, file, row.names = FALSE)
    }
  )
  ## Decom
  output$decom_TD <- renderDataTable({
    TD()$q0_ana
  })
  
  output$decom_down_TD <- downloadHandler(
    filename = function() {
      paste('TD_composition', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- TD()$q0_ana
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  ## Plot
  output$type1_TD <- renderPlot({plot_TD()[[1]]})
  output$plot1_TD <- downloadHandler(
    filename = function() {
      paste('miNEXT_TD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_TD()[[1]], width=10, height=12,dpi=500)
    }
  )
  
  output$type2_TD <- renderPlot({plot_TD()[[2]] + theme(plot.margin=unit(c(0.5,6,0.5,0),'cm'))})
  output$plot2_TD <- downloadHandler(
    filename = function() {
      paste('Composition_TD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_TD()[[2]], width=10, height=7,dpi=500)
    }
  )
  
  # PD ----------------------------------------------------------------------
  
  output$selectdata_PD <- renderUI({
    if(input$what_data_PD == "demo"){
      radioButtons("data_PD", "Choose demo data:", c("dunes data"= "1"), selected = "1")
    }else{
      fileInput("data_PD", "Upload data (one csv file)", accept = "text")
    }
  })
  
  output$select_Ori_Assemblage_PD <- renderUI({
    what_data <- input$what_data_PD
    
    if(what_data == "demo"){
      if(input$data_PD == "1"){
        data <- miNEXT.3D::dunes$data
        selectInput("Ori_Assemblage_PD", "Select Original Assemblage:", 
                    choices = names(data), selected = names(data)[1], multiple=F, selectize=FALSE)
      }
    }else{
      if(is.null(input$data_PD$datapath)){
        NULL
      }else{
        infile <- input$data_PD
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Ori_Assemblage_PD", "Select Original Assemblage:", 
                    choices = names(data), selected = names(data)[1], multiple=F, selectize=FALSE)
        
      }
    }
    
  })
  
  output$select_Trans_Assemblage_PD <- renderUI({
    what_data <- input$what_data_PD
    if(what_data == "demo"){
      if(input$data_PD == "1"){
        data <- miNEXT.3D::dunes$data
        selectInput("Trans_Assemblage_PD", "Select Transform Assemblage:", 
                    choices = names(data), selected = names(data)[2], multiple=F, selectize=FALSE)
      }
    }else{
      if(is.null(input$data_PD$datapath)){
        NULL
      }else{
        infile <- input$data_PD
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Trans_Assemblage_PD", "Select Transform Assemblage:", 
                    choices = names(data), selected = names(data)[2], multiple=F, selectize=FALSE)
        
      }
    }
    
  })
  
  
  output$select_tree <- renderUI({
    what_data <- input$what_data_PD
    
    if(what_data == "demo"){
      if(input$data_PD == 1){
        radioButtons("tree", "Demo phylo tree:",
                     choices = c("dunes tree" = 1), selected = 1)
      }
    }else{
      fileInput("tree", "Upload phylo tree (one txt file):", accept = "text")
    }
    # if(input$data_PD == 1){
    #   radioButtons("tree", "Demo phylo tree:",
    #                choices = c("Beetles' phylo tree" = 1), selected = 1)
    # }else if(input$data_PD == 2){
    #   radioButtons("tree", "Demo phylo tree:",
    #                choices = c("Hinkley's fish phylo tree" = 2), selected = 2)
    # }else{
    #   fileInput("tree", "Upload phylo tree:", accept = "text/csv")
    #   
    # }
    
  })
  
  
  output$knots_set_PD <- renderUI({
    if(input$knots_choose_PD == 0){
      numericInput("endpoint_PD", "knots setting", 11, min = 5)
    }else{
      textAreaInput("endpoint_PD", "Specify sample size:", rows = 2, value = " ")
    }
  })
  
  
  # run ---------------------------------------------------------------------
  what_data_PD <- eventReactive(input$go_PD,{input$what_data_PD})
  ass_PD <- eventReactive(input$go_PD, {c(input$Ori_Assemblage_PD,input$Trans_Assemblage_PD)})
  
  data_PD <- eventReactive(input$go_PD, {
    if(input$what_data_PD == "demo"){
      if(input$data_PD == "1"){
        data = miNEXT.3D::dunes$data
      }
    }else{
      infile <- input$data_PD
      data = read.csv(infile$datapath, row.names = 1)
    }
    return(data)
  })
  
  go_data_PD <- eventReactive(input$go_PD,{
    ass <- ass_PD()
    data <- data_PD()
    what_data <- what_data_PD()
    go_data = data[c(ass)]
    names(go_data) <- ass
    return(go_data)
  })
  
  # q_PD <- eventReactive(input$go_PD, {input$q_PD})
  end2_PD <- eventReactive(input$go_PD, {input$knots_choose_PD})
  end_PD <- eventReactive(input$go_PD, {
    end2 <- end2_PD()
    if(end2 == 0){
      out <- input$endpoint_PD
    }else{
      out <- strsplit(input$endpoint_PD, "\\s+")[[1]]
      out <- as.numeric(out)
      out <- out[!is.na(out)]
    }
  })
  b_PD <- eventReactive(input$go_PD, {input$b_PD})
  conf_PD <- eventReactive(input$go_PD, {input$conf_PD})
  
  tree <- eventReactive(input$go_PD, {
    what_data <- what_data_PD()
    if(what_data == "demo"){
      if(input$data_PD == "1"){
        tree <- miNEXT.3D::dunes$tree
      }
    }else{
      infile <- input$tree
      tree = read.tree(infile$datapath)
    }
    return(tree)
  })
  # typePD <- eventReactive(input$go_PD, {input$type_PD})
  
  ## Data summary 
  PD_infor <- eventReactive(input$go_PD,{
    
    out <- DataInfo3D(data = go_data_PD(),
                      diversity = 'PD',
                      datatype = "abundance",
                      PDtree = tree(),
                      PDreftime = NULL
    )
  })
  
  
  output$data_summary_PD <- renderDataTable({
    PD_infor() %>% data.frame()
  })
  
  output$view_data_PD <- renderDataTable({
    data = go_data_PD()
    data %>%
      DT::datatable(options = list(bFilter=T, scrollX = TRUE), rownames = T)
  })
  
  output$download_data_PD <- downloadHandler(
    filename = function() {
      paste('Raw data', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- go_data_PD()
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_Datainf_PD <- downloadHandler(
    filename = function() {
      paste('DataInformation_PD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- data.frame(PD_infor())
      write.csv(out, file, row.names = FALSE)
    }
  )
  ## Mixture
  PD <- eventReactive(input$go_PD,{
    if(end2_PD() == 0){
      out <- miNEXT3D(data = go_data_PD(),
                      diversity = 'PD',
                      q = c(0,1,2),
                      knots = end_PD(),
                      nboot = b_PD(),
                      conf = conf_PD(),
                      PDtree = tree()) 
      
    }else{
      out <- miNEXT3D(data = go_data_PD(),
                      diversity = 'PD',
                      q = c(0,1,2),
                      size = end_PD(),
                      nboot = b_PD(),
                      conf = conf_PD(),
                      PDtree = tree())
    }
  })
  
  plot_PD <- eventReactive(input$go_PD,{
    ggmiNEXT3D(PD())
  })
  
  output$minext_PDout <- renderUI({
    if(input$Assemblage_choose_PD == 1){
      dataTableOutput("Mixture_PD") %>% withSpinner(color="#0dc5c1")
    }else if(input$Assemblage_choose_PD == 2){
      dataTableOutput("Ori_PD") %>% withSpinner(color="#0dc5c1")
    }else{
      dataTableOutput("Trans_PD") %>% withSpinner(color="#0dc5c1")
    }
  })
  
  output$minext_PDdownout <- renderUI({
    if(input$Assemblage_choose_PD == 1){
      downloadButton("Mixture_PD_down", "Download")
    }else if(input$Assemblage_choose_PD == 2){
      downloadButton("Ori_PD_down", "Download")
    }else{
      downloadButton("Trans_PD_down", "Download")
    }
  })
  
  output$Mixture_PD <- renderDataTable({
    PD()$Mixture[,-8]
  })
  
  output$Ori_PD <- renderDataTable({
    PD()$Original[,-7]
  })
  
  output$Trans_PD <- renderDataTable({
    PD()$Transform[,-7]
  })
  
  output$Mixture_PD_down <- downloadHandler(
    filename = function() {
      paste('mixture_PD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- PD()$Mixture
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Ori_PD_down <- downloadHandler(
    filename = function() {
      paste('Original_PD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- PD()$Original
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Trans_PD_down <- downloadHandler(
    filename = function() {
      paste('Transform_PD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- PD()$Transform
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  ## Decom
  output$decom_PD <- renderDataTable({
    PD()$q0_ana
  })
  
  output$decom_down_PD <- downloadHandler(
    filename = function() {
      paste('PD_composition', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- PD()$q0_ana
      writecsv(out, file, row.names = FALSE)
    }
  )
  
  ## Plot
  output$type1_PD <- renderPlot({plot_PD()[[1]]})
  output$plot1_PD <- downloadHandler(
    filename = function() {
      paste('miNEXT_PD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_PD()[[1]], width=10, height=12,dpi=500)
    }
  )
  
  output$type2_PD <- renderPlot({plot_PD()[[2]] + theme(plot.margin=unit(c(0.5,6,0.5,0),'cm'))})
  output$plot2_PD <- downloadHandler(
    filename = function() {
      paste('Composition_PD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_PD()[[2]], width=10, height=7,dpi=500)
    }
  )
  
  
  # FD ----------------------------------------------------------------------
  
  output$selectdata_FD <- renderUI({
    if(input$what_data_FD == "demo"){
      radioButtons("data_FD", "Choose demo data:",
                   choices = c("dunes data"="1"), selected = "1")
    }else{
      fileInput("data_FD", "Upload data (one csv file)", accept = "text")
    }
  })
  
  output$select_Ori_Assemblage_FD <- renderUI({
    what_data <- input$what_data_FD
    
    if(what_data == "demo"){
      if(input$data_FD == "1"){
        data <- miNEXT.3D::dunes$data
        selectInput("Ori_Assemblage_FD", "Select Original Assemblage:", 
                    choices = names(data), selected = names(data)[1], multiple=F, selectize=FALSE)
      }
    }else{
      if(is.null(input$data_FD$datapath)){
        NULL
      }else{
        infile <- input$data_FD
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Ori_Assemblage_FD", "Select Original Assemblage:", 
                    choices = names(data), selected = names(data)[1], multiple=F, selectize=FALSE)
        
      }
    }
    
  })
  
  output$select_Trans_Assemblage_FD <- renderUI({
    what_data <- input$what_data_FD
    
    if(what_data == "demo"){
      if(input$data_FD == "1"){
        data <- miNEXT.3D::dunes$data
        selectInput("Trans_Assemblage_FD", "Select Transform Assemblage:", 
                    choices = names(data), selected = names(data)[2], multiple=F, selectize=FALSE)
      }
    }else{
      
      if(is.null(input$data_FD$datapath)){
        NULL
      }else{
        infile <- input$data_FD
        data = read.csv(infile$datapath, row.names = 1)
        selectInput("Trans_Assemblage_FD", "Select Transform Assemblage:", 
                    choices = names(data), selected = names(data)[2], multiple=F, selectize=FALSE)
        
      }
    }
    
  })
  
  
  output$select_dis <- renderUI({
    what_data <- input$what_data_FD
    
    if(what_data == "demo"){
      if(input$data_FD == 1){
        radioButtons("dis", "Demo distance matrix:",
                     choices = c("dunes distance matrix" = 1), selected = 1)
      }
    }else{
      fileInput("dis", "Upload distance matrix (one csv file):", accept = "text")
    }
  })
  
  output$tau_set_FD <- renderUI({
    if(input$what_data_FD == "demo"){
      if(input$data_FD == "1"){
        data <- miNEXT.3D::dunes$data
        dis <- miNEXT.3D::dunes$dist
      }
    }else{
      infile <- input$data_FD
      data = read.csv(infile$datapath, row.names = 1)
      dis = read.csv(infile$datapath, row.names = 1)
    }
    
    ass = c(input$Ori_Assemblage_FD,input$Trans_Assemblage_FD)
    go_data = data[c(ass)]
    tmp = rowSums(go_data)
    tmp <- matrix(tmp/sum(tmp), ncol = 1)
    tmp1 <- sum((tmp %*% t(tmp)) * dis)
    
    if(input$choose_tau == "tau_values"){
      numericInput("tau_setting", "threshold setting", tmp1, min = 0, max = 1)
    }
  })
  
  output$knots_set_FD <- renderUI({
    if(input$knots_choose_FD == 0){
      numericInput("endpoint_FD", "knots setting", 11, min = 5)
    }else{
      textAreaInput("endpoint_FD", "Specify sample size:", rows = 2, value = " ")
    }
  })
  
  # run ---------------------------------------------------------------------
  what_data_FD <- eventReactive(input$go_FD,{input$what_data_FD})
  ass_FD <- eventReactive(input$go_FD, {c(input$Ori_Assemblage_FD,input$Trans_Assemblage_FD)})
  
  data_FD <- eventReactive(input$go_FD, {
    if(input$what_data_FD == "demo"){
      if(input$data_FD == "1"){
        data = miNEXT.3D::dunes$data
      }
    }else{
      infile <- input$data_FD
      data = read.csv(infile$datapath, row.names = 1)
    }
    return(data)
  })
  
  go_data_FD <- eventReactive(input$go_FD,{
    ass <- ass_FD()
    data <- data_FD()
    what_data <- what_data_FD()
    go_data = data[c(ass)]
    names(go_data) <- ass
    return(go_data)
  })
  
  # q_FD <- eventReactive(input$go_FD, {input$q_FD})
  end2_FD <- eventReactive(input$go_FD, {input$knots_choose_FD})
  end_FD <- eventReactive(input$go_FD, {
    end2 <- end2_FD()
    if(end2 == 0){
      out <- input$endpoint_FD
    }else{
      out <- strsplit(input$endpoint_FD, "\\s+")[[1]]
      out <- as.numeric(out)
      out <- out[!is.na(out)]
    }
  })
  b_FD <- eventReactive(input$go_FD, {input$b_FD})
  conf_FD <- eventReactive(input$go_FD, {input$conf_FD})
  distance <- eventReactive(input$go_FD, {input$dis})
  
  dis_matrix <- eventReactive(input$go_FD, {
    what_data <- what_data_FD()
    dist <- distance()
    if(what_data == "demo"){
      if(input$data_FD == 1){
        dis <- miNEXT.3D::dunes$dist
      }
    }else{
      infile <- dist
      dis = read.csv(infile$datapath, row.names = 1)
    }
    return(dis)
  })
  
  FDtype <- eventReactive(input$go_FD,({input$choose_tau}))
  tau_FD <- eventReactive(input$go_FD, {
    tau2 <- FDtype()
    if(tau2 == 'tau_values'){
      out <- input$tau_setting
    }else{
      out <- NULL
    }
  })
  
  ## Data summary 
  FD_infor <- eventReactive(input$go_FD,{
    out <- DataInfo3D(data = go_data_FD(),
                      diversity = 'FD',
                      datatype = "abundance",
                      FDdistM = dis_matrix(),
                      FDtype = FDtype(),
                      FDtau = tau_FD())
  })
  
  output$data_summary_FD <- renderDataTable({
    FD_infor() %>% data.frame()
  })
  
  output$view_data_FD <- renderDataTable({
    data = go_data_FD()
    data %>%
      DT::datatable(options = list(bFilter=T, scrollX = TRUE), rownames = T)
  })
  
  output$download_data_FD <- downloadHandler(
    filename = function() {
      paste('Raw data', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- go_data_FD()
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_Datainf_FD <- downloadHandler(
    filename = function() {
      paste('DataInformation_FD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- data.frame(FD_infor())
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  ## Mixture
  FD <- eventReactive(input$go_FD,{
    
    if(end2_FD() == 0){
      out <- miNEXT3D(data = go_data_FD(),
                      diversity = 'FD',
                      q = c(0,1,2),
                      knots = end_FD(),
                      nboot = b_FD(),
                      conf = conf_FD(),
                      FDdistM = dis_matrix(),
                      FDtype = FDtype(),
                      FDtau = tau_FD()) 
    }else{
      out <- miNEXT3D(data = go_data_FD(),
                      diversity = 'FD',
                      q = c(0,1,2),
                      size = end_FD(),
                      nboot = b_FD(),
                      conf = conf_FD(),
                      FDdistM = dis_matrix(),
                      FDtype = FDtype(),
                      FDtau = tau_FD())
    }
  })
  
  plot_FD <-eventReactive(input$go_FD,{
    ggmiNEXT3D(FD())
  })
  
  output$minext_FDout <- renderUI({
    if(input$Assemblage_choose_FD == 1){
      dataTableOutput("Mixture_FD") %>% withSpinner(color="#0dc5c1")
    }else if(input$Assemblage_choose_FD == 2){
      dataTableOutput("Ori_FD") %>% withSpinner(color="#0dc5c1")
    }else{
      dataTableOutput("Trans_FD") %>% withSpinner(color="#0dc5c1")
    }
  })
  
  output$minext_FDdownout <- renderUI({
    if(input$Assemblage_choose_FD == 1){
      downloadButton("Mixture_FD_down", "Download")
    }else if(input$Assemblage_choose_FD == 2){
      downloadButton("Ori_FD_down", "Download")
    }else{
      downloadButton("Trans_FD_down", "Download")
    }
  })
  
  output$Mixture_FD <- renderDataTable({
    FD()$Mixture[,-8]
  })
  
  output$Ori_FD <- renderDataTable({
    FD()$Original[,-7]
  })
  
  output$Trans_FD <- renderDataTable({
    FD()$Transform[,-7]
  })
  
  output$Mixture_FD_down <- downloadHandler(
    filename = function() {
      paste('mixture_FD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- FD()$Mixture
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Ori_FD_down <- downloadHandler(
    filename = function() {
      paste('Original_FD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- FD()$Original
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$Trans_FD_down <- downloadHandler(
    filename = function() {
      paste('Transform_FD', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- FD()$Transform
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  ## Decom
  output$decom_FD <- renderDataTable({
    FD()$q0_ana
  })
  
  output$decom_down_FD <- downloadHandler(
    filename = function() {
      paste('FD_composition', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      out <- FD()$q0_ana
      writecsv(out, file, row.names = FALSE)
    }
  )
  
  ## Plot
  output$type1_FD <- renderPlot({plot_FD()[[1]]})
  output$plot1_FD <- downloadHandler(
    filename = function() {
      paste('miNEXT_FD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_FD()[[1]], width=10, height=12,dpi=500)
    }
  )
  
  output$type2_FD <- renderPlot({plot_FD()[[2]] + theme(plot.margin=unit(c(0.5,6,0.5,0),'cm'))})
  output$plot2_FD <- downloadHandler(
    filename = function() {
      paste('Composition_FD', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, plot_FD()[[2]], width=10, height=7,dpi=500)
    }
  )
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)





