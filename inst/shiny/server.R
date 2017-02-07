
shinyServer(function(input, output, session) {
  
  # By default, Shiny limits file uploads to 5MB per file. You can modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R would increase the limit to 30MB.
  options(shiny.maxRequestSize=30*1024^2) 
  
  #######################################
  ####      Onglet :   Import CSV   #####
  
  dataInput <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath, sep=input$sep, dec=input$dec, header=TRUE, comment.char="#")
  })
  

  output$filedf <- renderTable({
    if(is.null(dataInput())){return()}
    input$file
  })
  
  output$table <- renderDataTable({
    if(is.null(dataInput())){return()}
    datatable(dataInput(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  output$sum <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("About file", tableOutput("filedf")), tabPanel("Data", DT::dataTableOutput("table"))) 
  })
  
  
  #######################################
  ####        Onglet : Events        ####
  ####    Selection d une chaine      ##
  
  names <- reactive({
    names = colnames(dataInput())
    return(names)
  })
  
  observe({
    updateSelectInput(session, inputId='variables', 'Select a MCMC chain', choices = names() )
  })
  
  selectChain <- reactive({ 
    dataInput()[[ input$variables ]]
    })
  
  output$MarginalPlot <- renderPlot({
    MarginalPlot(selectChain(), level = input$level, title = input$titlePlot, colors=input$color )
  })

  MarginalStatisticsText <- reactive({ 
    MarginalStatistics(selectChain(), level = input$level) 
  })
  
  output$MarginalStatisticsUI <- renderUI({ 
  tags$div(
    tags$p("Mean = ", MarginalStatisticsText()[1,1]),
    tags$p("MAP = ", MarginalStatisticsText()[2,1]),
    tags$p("sd = ", MarginalStatisticsText()[3,1]), 
    tags$p("Q1 = ", MarginalStatisticsText()[4,1]),
    tags$p("Median = ", MarginalStatisticsText()[5,1]),
    tags$p("Q2 = ", MarginalStatisticsText()[6,1]), 
    tags$p("For a level of confidence at ", MarginalStatisticsText()[7,1]*100, "%"),
    tags$p("Credible Interval = [", MarginalStatisticsText()[8,1], "," , MarginalStatisticsText()[9,1], "]"),
    tags$p("HPD region = [", MarginalStatisticsText()[10,1], "," , MarginalStatisticsText()[11,1], "]") 
    ) 
    })
  
  output$result2 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
    tabsetPanel(tabPanel("Marginal plot", plotOutput("MarginalPlot")), tabPanel("Marginal statistics", uiOutput("MarginalStatisticsUI")))
    })
  
  
  #######################################
  ####        Onglet : Events         ###
  ##    Selection plusieurs chaines    ##
  
  # Initialize reactive values
  values <- reactiveValues()
  
  output$ChainsSelection <- renderUI({
    themes <- names()
    values$names <- themes
    checkboxGroupInput('multiChainsCI', 'Select numbers:', themes)
  })
  
  
  # Add observer on select-all button
  observeEvent(input$selectAll, {
    values$names <- names()
    updateCheckboxGroupInput(session, 'multiChainsCI', selected =  values$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll, {
    values$names <- c() 
    updateCheckboxGroupInput(session, 'multiChainsCI', selected =  "none")
  })
    
  # data selectionnees 
  selectData <- reactive({ 
    dataInput()[, input$multiChainsCI, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetCI <- renderDataTable({
    if(is.null(selectData())){return( )}
    datatable(selectData(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  # calcul des IC
  MultiCredibleIntervalText <- reactive({   
    if(is.null( input$multiChainsCI )) { return()}
    position = seq(1, length(input$multiChainsCI))
    MultiCredibleInterval(selectData(), position, level = input$level22) 
  })
  
  # affichage des resultats des IC
    output$resultTableMCI <- renderTable({
      if(is.null(MultiCredibleIntervalText())) {return()}
      else {
        dim = dim(MultiCredibleIntervalText())
        names_CI = rownames(MultiCredibleIntervalText())
        CIInf = NULL
        CISup = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_CI[i])
          CIInf= c(CIInf, MultiCredibleIntervalText()[i,2])
          CISup = c(CISup, MultiCredibleIntervalText()[i,3])
        }
        data.frame("names"=name,  "Credible Interval Inf"=CIInf, "Credible Interval Sup" = CISup)
      }
     })  
    
    ### calcul des HPD
    MultiHPDText <- reactive({   
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      MultiHPD(selectData(), position, level = input$level22) 
    })
    
    # affichage des resultats des IC
    output$resultTableMHPD <- renderTable({
      if(is.null(MultiHPDText())) {return()}
      else {
        dim = dim(MultiHPDText())
        names_HPD = rownames(MultiHPDText())
        HPDInf = NULL
        HPDSup = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_HPD[i])
          HPDInf= c(HPDInf, MultiHPDText()[i,2])
          HPDSup = c(HPDSup, MultiHPDText()[i,3])
        }
        data.frame("names"=name, "HPD Inf"=HPDInf, "HPD Sup" = HPDSup)
      }
    })     
    
    
    output$MultiDatesPlot <- renderPlot({
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      
      MultiDatesPlot(selectData(), position, intervals =input$intervals, level = input$level, title = input$titleIntervalsplot)
    })#, height = 600, width = 800)
    
    
    output$TempoPlot <- renderPlot({
        if(is.null( input$multiChainsCI )) { return()}
        position = seq(1, length(input$multiChainsCI))
        TempoPlot(selectData(), position, level = input$level, title = input$titleTempoplot, Gauss=input$GaussCI, count=input$count)
    })#, height = 600, width = 800)
    
    output$TempoPlotUI <- renderUI({
      if(is.null( input$multiChainsCI )) {h5(" Nothing to display ")}
      else{
        plotOutput("TempoPlot", width="80%")
      }
    })
    
    
    output$TempoActivityPlot <- renderPlot({
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      TempoActivityPlot(selectData(), position, level = input$level, count=input$count)
    })#, height = 600, width = 800)

    output$TempoActivityPlotUI <- renderUI({
      if(is.null( input$multiChainsCI )) {h5(" Nothing to display ")}
      else{
        plotOutput("TempoActivityPlot", width="80%")
      }
    })
    
  output$result22 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetCI")), tabPanel("Credible intervals", uiOutput("resultTableMCI")), tabPanel("HPD regions", uiOutput("resultTableMHPD")), tabPanel("Intervals Plot", plotOutput("MultiDatesPlot")), tabPanel("Tempo Plot", plotOutput("TempoPlot"), br(), plotOutput("TempoActivityPlot")))
    
  })
  
  
  ######################################
  ####         Onglet : Phases      ####
  ####    Selection d une phase      ###
  
  observe({
    updateSelectInput(session, inputId='variablesalpha', 'Select the minimum of the group (alpha)', choices = names())
    updateSelectInput(session, inputId='variablesbeta', 'Select the maximum of the group (beta)', choices = names())
  })

  selectChain2 <- reactive({ 
    dataInput()[,c(input$variablesalpha, input$variablesbeta), drop = FALSE]
  })
  
  TestPhaseSelected <- reactive({ 
    if( sum(ifelse(dataInput()[,1] < dataInput()[,2], 1, 0)) == length(dataInput()[,1])) {return(1)}
  })
  
  output$selectedTable2 <- renderDataTable({
    if(is.null(selectChain2())) { return( h5("prout")) }
    else 
    datatable(selectChain2(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })

  PhaseStatisticsText <- reactive({ 
    PhaseStatistics(dataInput()[,input$variablesalpha, drop=TRUE], dataInput()[,input$variablesbeta, drop=TRUE], level = input$level2) 
  })
  
  output$PhaseStatisticsUI <- renderTable({
      res = PhaseStatisticsText()
      if(is.null(res)) {return()}
      else {
        dim = dim(res)
        names_PS = rownames(res)
        Minimum = NULL
        Maximum = NULL
        Duration = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_PS[i])
          Minimum= c(Minimum, res[i,2])
          Maximum = c(Maximum, res[i,3])
          Duration = c(Duration, res[i,3])
        }
        data.frame("names"=name, "Minimum"=Minimum, "Maximum" = Maximum, "Duration" = Duration)
      }
  })   
  
  PhaseTimeRangeText <- reactive({ 
    PhaseTimeRange(dataInput()[,input$variablesalpha, drop=TRUE],dataInput()[,input$variablesbeta, drop=TRUE], level = input$level2) 
  })
  
  output$PhaseTimeRangeUI <- renderUI({ 
    res = PhaseTimeRangeText()
    tags$div(
      tags$p("For a level of confidence at ", res[1]*100, "%"),
      tags$p("Time Range = [", res[2], "," ,res[3], "]")
    ) 
  })
 
  
  output$PhasePlotFunction <- renderPlot({
    PhasePlot(dataInput()[,input$variablesalpha, drop=TRUE], dataInput()[,input$variablesbeta, drop=TRUE], level = input$level2, title = input$titlePlot2, colors=input$color2 )
  })
  output$PhaseDurationPlotFunction <- renderUI({
    MarginalPlot(dataInput()[,input$variablesbeta, drop=TRUE] - dataInput()[,input$variablesalpha, drop=TRUE], level = input$level2, title = "Duration of the phase", colors=input$color2)
   })
  
  output$PhasePlotUI <- renderUI({
    if(is.null(dataInput()))
      {h5(" Nothing to display ")}
    else{
      plotOutput("PhasePlotFunction")
    }
  })
  
  output$PhaseDurationPlotUI <- renderUI({
    if(is.null(dataInput()))
    {h5(" Nothing to display ")}
    else{
        uiOutput("PhaseDurationPlotFunction")
    }
  })
  
  output$result3 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("Data", DT::dataTableOutput("selectedTable2")), tabPanel("Marginal Plots", fluidRow( uiOutput("PhasePlotUI"), uiOutput("PhaseDurationPlotUI") ) ), tabPanel("Time range", uiOutput("PhaseTimeRangeUI")), tabPanel("Marginal Statistics", uiOutput("PhaseStatisticsUI")))
  })  
  
  
  
  #####################################
  ####  Onglet : Several phases   ####
  
  # Initialize reactive values
  phases <- reactiveValues()
  
  output$PhasesSelection32 <- renderUI({
    themes <- names()
    succession$names <- themes
    checkboxGroupInput('multiPhasesSelection32', 'Select the beginning and the end of each phase:', themes)
  })
  
  # Add observer on select-all button
  observeEvent(input$selectAll32, {
    succession$names <- names()
    updateCheckboxGroupInput(session, 'multiPhasesSelection32', selected =  succession$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll32, {
    succession$names <- c() 
    updateCheckboxGroupInput(session, 'multiPhasesSelection32', selected =  "none")
  })
  
  # data selectionnees 
  selectData32 <- reactive({ 
    dataInput()[, input$multiPhasesSelection32, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetPhases32 <- renderDataTable({
    if(is.null(selectData32())){return()}
    datatable(selectData32(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  Position_beginning32 <- reactive({
    dim = dim(selectData32())[2]
    pos = seq(1, dim, by = 2)
    return(pos)
  })

  MultiPhaseTimeRangeFunction <- reactive({
    MultiPhaseTimeRange(selectData32(), position_minimum = Position_beginning32(), level = input$levelMultiPhases)
  })

  output$MultiPhaseTimeRangeUI <- renderTable({
    res = MultiPhaseTimeRangeFunction()
    if(is.null(res)) {h5(" Nothing to display ")}
    else {
      dim = dim(res)
      names_MTR = rownames(res)
      PTInf = NULL
      PTSup = NULL
      names = NULL
      for (i in 1:dim[1]){
        names = c(names, names_MTR[i])
        PTInf= c(PTInf, res[i,2])
        PTSup = c(PTSup, res[i,3])
      }
      data.frame("names"=names,"Time Range Inf"=PTInf, "Time Range Sup" = PTSup)
    }
  })

  output$MultiPhasePlotFunction <- renderPlot({
   MultiPhasePlot(selectData32(), position_minimum = Position_beginning32(), title = input$titleMultiPhases, level = input$levelMultiPhases )
  })

  output$MultiPhasePlotUI <- renderUI({
    if(is.null(selectData32()))
      {h5(" Nothing to display ")}
    else
      plotOutput("MultiPhasePlotFunction")
  })
  
  output$result32 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetPhases32")), tabPanel("Time range", uiOutput("MultiPhaseTimeRangeUI")), tabPanel("Phases plot", "Marginal posterior densities of the beginning and the end of the selected phases and their time range interval (segment above the curves) at the desired level." ,uiOutput("MultiPhasePlotUI")))
  })
  
  
  
  ##########################################
  ####  Onglet : Succession de phases   ####
   
  # Initialize reactive values
  succession <- reactiveValues()
  
  output$PhasesSelection <- renderUI({
    themes <- names()
    succession$names <- themes
    checkboxGroupInput('multiPhasesSelection', 'Select the beginning and the end of each phase:', themes)
  })
  
  # Add observer on select-all button
  observeEvent(input$selectAll4, {
    succession$names <- names()
    updateCheckboxGroupInput(session, 'multiPhasesSelection', selected =  succession$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll4, {
    succession$names <- c() 
    updateCheckboxGroupInput(session, 'multiPhasesSelection', selected =  "none")
  })
  
  # data selectionnees 
  selectData4 <- reactive({ 
    dataInput()[, input$multiPhasesSelection, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetPhases <- renderDataTable({
    if(is.null(selectData4())){return()}
    datatable(selectData4(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })

  
    ## Ordering 
     Position_beginning <- reactive({
      ordre <- order(selectData4()[1,])
      pos = seq(1,length(ordre), by = 2)
      return(ordre[pos])
    })
     
     output$AffichagePositions <- renderUI({
           tags$div(
            tags$p("Positions of the beginnings 1", as.character(Position_beginning()[1]), ""),
            tags$p("Positions of the beginnings 2", as.character(Position_beginning()[2])),
            tags$p("Positions of the beginnings 3", as.character(Position_beginning()[3]), ""),
            tags$p("Positions of the beginnings 4", as.character(Position_beginning()[4]))
            )
     })
    
    ## Succession plot
    output$MultiSuccessionFunction <- renderPlot({
      MultiSuccessionPlot( selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession, title = input$titleSuccessionPlot )
    }, height = 600, width = 800 )

    output$MultiSuccessionUI <- renderUI({
      if( length(Position_beginning() ) < 2)
        h5(" Nothing to display ")
      else
        plotOutput("MultiSuccessionFunction", width="100%")
    })
    

    ## Succession Transitions
    MultiPhasesTransitionFunction <- reactive({
      MultiPhasesTransition(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })

    output$MultiPhasesTransitionResults <- renderTable({
      if( length(Position_beginning() ) < 2){ return()}
      else {
        res = MultiPhasesTransitionFunction()
        dim = dim(res)
        names_MTR = rownames(res)
        TRInf = NULL
        TRSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MTR[i])
          TRInf= c(TRInf, res[i,2])
          TRSup = c(TRSup, res[i,3])
        }
        data.frame("names"=names,"Transition range Inf"=TRInf, "Transition range Sup" = TRSup)
      }
    })  

    ## Succession Gaps

    MultiPhasesGapFunction <- reactive({
      MultiPhasesGap(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })

    output$MultiPhasesGapResults <- renderTable({
      if( length(Position_beginning() ) < 2) { return()}
      else {
        res = MultiPhasesGapFunction()
        dim = dim(res)
        names_MPG = rownames(res)
        GapInf = NULL
        GapSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MPG[i])
          GapInf= c(GapInf, res[i,2])
          GapSup = c(GapSup, res[i,3])
          }
       data.frame("names"=names,"Gap range Inf"= GapInf, "Gap range Sup" = GapSup) 
      }
    })
    
    ## Succession Time range
    
    MultiPhaseTimeRangeFunction4 <- reactive({
      MultiPhaseTimeRange(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })
    
    output$MultiPhaseTimeRange4UI <- renderTable({
      if( length(Position_beginning() ) < 2){ return()}
      else {
        res = MultiPhaseTimeRangeFunction4()
        dim = dim(res)
        names_MTR = rownames(res)
        PTInf = NULL
        PTSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MTR[i])
          PTInf= c(PTInf, res[i,2])
          PTSup = c(PTSup, res[i,3])
        }
        data.frame("names"=names,"Time Range Inf"=PTInf, "Time Range Sup" = PTSup)
      }
    })   
    
   ## Output 
   output$Inputs4 <- renderUI({
     if(is.null(dataInput()))
       h5("No data imported")
     else 
       tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetPhases")), tabPanel("Time ranges", uiOutput("MultiPhaseTimeRange4UI")), tabPanel("Transition ranges", uiOutput("MultiPhasesTransitionResults")), tabPanel("Gap ranges", uiOutput("MultiPhasesGapResults")), tabPanel("Succession plot", fluidRow("Curves represent the marginal posterior densities of the beginning and end of each phase. Segments correspond to time range of the phase of the same color,  two-coloured segments correspond to transition interval or to the gap range. A cross instead of a two-coloured segment means that there is no gap range at the desired level of confidence."),fluidRow(uiOutput("MultiSuccessionUI"))  )) 
   })
   
   
  ##########################################
  
})