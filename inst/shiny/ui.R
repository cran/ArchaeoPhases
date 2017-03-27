library(shiny)
library(shinythemes)
library(ArchaeoPhases)
library(DT)
library(dplyr)
library(hdrcde)
library(coda)

renderInputs0 <- function() {
    fluidRow(
      h4("Post-Processing of the Markov Chain Simulated by ChronoModel, by BCal or by Oxcal"),
      br(),
      h4("This is a Shiny application for ChronoModel, BCal or Oxcal users who want to use ArchaeoPhases package without having to know R software."),
      h4("The process is very simple !"),
      h4("First model your chronology with ChronoModel, BCal or Oxcal or any other software for constructing archaeological chronologies.  "),
      h4("Then extract the simulated Markov Chains and save it into a CSV file. "),
      h4("And finally, import the CSV file using this web application and analyse your chronological dates or groups of dates. "),
      br(),
      tags$div(class="header", checked=NA,
                  tags$p("To visite ChronoModel website"), tags$a(href="http://www.chronomodel.fr/", "Click Here!"), 
               br(),
                  tags$p("To visite Oxcal website"), tags$a(href="https://c14.arch.ox.ac.uk/oxcalhelp/hlp_contents.html", "Click Here!"),
               br(),
               tags$p("To visite BCal website"), tags$a(href="http://bcal.shef.ac.uk/.html", "Click Here!")
       ),
      br(),
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("Ready to take the ArchaeoPhases tutorial? If so"),
               tags$a(href="http://www.math.sciences.univ-nantes.fr/~philippe/Stat_&_Archaeology_files/ArchaeoPhasesTuto1.0.pdf", "Click Here!")
      ),       
      br(),
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("This application was developed by Anne Philippe and Marie-Anne Vibet members of the Laboratoire de mathématiques Jean Leray, Université de Nantes, France."),
               tags$p("Maintainer : Anne Philippe <anne.philippe@univ-nantes.fr>"),
               tags$p("Last release : March 2017")
      )
      
  )}

################################
### Onglet "Import CSV" ########

renderInputs11 <- function() {

  wellPanel(
    fluidRow(
      fileInput(inputId = "file11", label ="Choose file", multiple = FALSE),
      h5(helpText("Select separators below to read the CSV file")),
      column(6,radioButtons(inputId ='sep11', label="Cell separator", choices=c(Comma=',', Semicolon=';', Tab='\t', Space=''), selected=',')),
      column(6,radioButtons(inputId='dec11', label="Decimal separator", choices=c(Comma=',', Dot='.'), selected='.') ), 
      br(),
      br(),
      textInput(inputId='iterationColumn1', label="Number of the iteration column", "NULL" ),
      textInput(inputId='referenceYear1', label="Year of reference for non BC/AD format", "NULL" ),
      textInput(inputId='rowToWithdraw1', label="Row to be withdrawn", "NULL" )
    )
    
  )}

renderInputs12 <- function() {
  wellPanel(
    fluidRow(
      fileInput(inputId = "file12", label ="Choose file", multiple = FALSE),
      h5(helpText("Select separators below to read the CSV file")),
      column(6,radioButtons(inputId ='sep12', label="Cell separator", choices=c(Comma=',', Semicolon=';', Tab='\t', Space=''), selected=',')),
      column(6,radioButtons(inputId='dec12', label="Decimal separator", choices=c(Comma=',', Dot='.'), selected='.') ), 
      br(),
      textInput(inputId='iterationColumn2', label="Number of the iteration column", "NULL" ),
      textInput(inputId='referenceYear2', label="Year of reference for non BC/AD format", "NULL" ),
      textInput(inputId='rowToWithdraw2', label="Row to be withdrawn", "NULL" )
    )
    
  )}


renderInputs13 <- function() {
  wellPanel(
    fluidRow(
      h3("Create Group of dates"),
      h5(helpText("Dates selection")),
      actionButton(inputId = "selectAllG", 
                   label = "Select all"), 
      actionButton(inputId = "clearAllG", 
                   label = "Clear selection"), 
      uiOutput("ChainsSelectionG"),
      br(),
      textInput(inputId='name', label="Name of the group", "Group1" ),
      textInput(inputId='exportFile', label="The name of the file to be exported", "NULL" ),
      actionButton(inputId = "goButton", label = "Create group"),
      actionButton(inputId = "addButton", label = "Add group"),
      actionButton(inputId = "clearButton", label = "Clear groups")
    )
    
  )}

################################
### Onglet Convergence  ########

renderInputsCV <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of groups in succession"),
      h5(helpText("Dates selection")),
      actionButton(inputId = "selectAllCV", 
                   label = "Select all"), 
      actionButton(inputId = "clearAllCV", 
                   label = "Clear selection"), 
      uiOutput("ChainsSelectionCV"),
      br(),
      h4(helpText("Number of chains")),
      numericInput(inputId ='NbChains', label="Number", value=2, min=1, max=100)
    )
    
  )}


################################
### Onglet Dates  ########

renderInputs2 <- function() {
  wellPanel(
    fluidRow(
      h3("Description of individual dates"),
      selectInput("variables", "Select chain names", character(0)),
      h5(helpText("Statistical options")),
      numericInput(inputId ='level', label="Confidence level", value=0.95,min=0, max=1),
      br(),
      h5(helpText("Graphic options")),
      textInput(inputId='titlePlot', label="Plot title", "Characteristics of a date" ),
      column(6,radioButtons(inputId='color', label="Colors", choices=c(Yes='TRUE', No='FALSE'), selected='TRUE') )
      )
    
  )}


renderInputs22 <- function() {
  wellPanel(
    fluidRow(
      h3("Group of dates"),
      h5(helpText("Dates selection")),
      actionButton(inputId = "selectAll", 
                   label = "Select all"), 
      actionButton(inputId = "clearAll", 
                   label = "Clear selection"), 
      uiOutput("ChainsSelection"), 
      br(),
      h5(helpText("Statistical options")),
      numericInput(inputId ='level22', label="Confidence level", value=0.95,min=0, max=1),
      #br(),
      h5(helpText("Intervals plot options")),
      textInput(inputId='titleIntervalsplot', label="Intervals plot title", "Intervals plot" ),
      radioButtons(inputId='intervals', label="Intervals", choices=c("Credible Intervals"='CI', "Highest Posterior Density"='HPD'), selected='CI'),
      column(6,radioButtons(inputId='exportFile22IT', label="Export graph", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE')),
      column(6,radioButtons(inputId='exportFormatIT', label="File format", choices=c(PNG='PNG', SVG='SVG'), selected='PNG')),
      br(),
      h5(helpText("Tempo plot options")),
      textInput(inputId='titleTempoplot', label="Tempo plot title", "Tempo plot" ),
      column(6,radioButtons(inputId='GaussCI', label="Gaussian approx", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE')), 
      column(6,radioButtons(inputId='count', label="Counting process", choices=c(Number='TRUE', Probability='FALSE'), selected='TRUE')),
      textInput(inputId='xlabel', label="x-label", "Calendar year" ),
      textInput(inputId='ylabel', label="y-label", "Cumulative events" ),
      column(6,radioButtons(inputId='colors', label="Use of colors", choices=c(Yes='TRUE', No='FALSE'), selected='TRUE')),
      column(6,radioButtons(inputId='exportFile22', label="Export graph", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE'))
    )
    
  )}


  ##################################
#######   Group of dates   ##########

renderInputs3 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of a group"),
      h5(helpText("Make sure that File2 is uploaded.")),
      selectInput("variablesMin", "Select chain names", choices = character(0), multiple = FALSE, selected = NULL),
      selectInput("variablesMax", "Select chain names", choices = character(0), multiple = FALSE, selected = NULL),
      br(),
      h5(helpText("Statistical options")),
      numericInput(inputId ='level2', label="Confidence level", value=0.95, min=0, max=1),
      br(),
      h5(helpText("Graphical options")),
      textInput(inputId='titlePlot2', label="Title", "Characterisation of a group" ),
      column(6,radioButtons(inputId='color2', label="Colors", choices=c(Yes='TRUE', No='FALSE'), selected='TRUE') )
    )
  )}

renderInputs32 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of groups (at least two)"),
      br(),
      h4(helpText("Warning : If groups are nested in each other, the CSV file should be reorganised. ")),
      br(),
      actionButton(inputId = "selectAll32", 
                   label = "Select all"),
      actionButton(inputId = "clearAll32", 
                   label = "Clear selection"), 
      uiOutput("PhasesSelection32"),
      br(),
      h5(helpText("Statistical options")),
      numericInput(inputId ='levelMultiPhases', label="Confidence level", value=0.95, min=0, max=1),
      br(),
      h5(helpText("Graphical options")),
      textInput(inputId='titleMultiPhases', label="Plot title", "Characterisation of several groups" ),
      column(6,radioButtons(inputId='exportFile32', label="Export graph", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE')),
      column(6,radioButtons(inputId='exportFormat32', label="File format", choices=c(PNG='PNG', SVG='SVG'), selected='PNG'))
    )
  )}



    ##################################
#######   Succession of groups   ##########


renderInputs4 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of groups in succession"),
      br(),
      h4(helpText("Warning : temporal order constraints should have been introduced in the modelling. ")),
      br(),
      actionButton(inputId = "selectAll4", 
                   label = "Select all"),
      actionButton(inputId = "clearAll4", 
                   label = "Clear selection"), 
      uiOutput("PhasesSelection"),
      br(),
      h5(helpText("Statistical options")),
      numericInput(inputId ='levelSuccession', label="Confidence level", value=0.95, min=0, max=1),
      br(),
      h5(helpText("Graphical options")),
      textInput(inputId='titleSuccessionPlot', label="Plot title", "Characterisation of a succession of groups" ),
      column(6,radioButtons(inputId='exportFile4', label="Export graph", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE')),
      column(6,radioButtons(inputId='exportFormat4', label="File format", choices=c(PNG='PNG', SVG='SVG'), selected='PNG'))
    )
    
  )}





shinyUI(fluidPage(
  headerPanel('ArchaeoPhases'),
  titlePanel('   Analysis of archaeological phases'),
  
  navbarPage(" ", theme = shinytheme("cerulean"),
             tabPanel("Home", 
                      renderInputs0()
             ),
             tabPanel("Import CSV", titlePanel("Import your CSV files"), 
                      fluidRow(
                        h2("File1 : Dates"),
                        h4("First, import here the CSV file contaning the MCMC of all dates"),
                        h4("For modelling done with ChronoModel, this file is called 'events.csv'"),
                        h4("For any other software, import the CSV file extracted from it. ")
                      ),
                      fluidRow(
                        column(5, renderInputs11()),
                        column(6, uiOutput("AfficheTableLue11"))
                      )  ,
                      fluidRow(
                        h2("File2 : Groups of dates"),
                        h4("Import here the CSV file contaning the MCMC of the minimum and maximum of all groups of dates if any."),
                        h4("For modelling done with ChronoModel, this file is called 'phases.csv'"),
                        h4("For any other software, you may need to create it (see below). ")
                      ),
                      fluidRow(
                        column(5, renderInputs12()),
                        column(6, uiOutput("AfficheTableLue12"))
                      )  ,
                      fluidRow(
                        h4("Creating the file containing the minimim and maximum of all groups of dates.")
                       ),
                      fluidRow(
                        column(5, renderInputs13()),
                        column(6, uiOutput("result13"))

                      )
                      ),
             tabPanel("Convergence", titlePanel("Check the convergence of the Markov chains"),
                      fluidRow(
                        column(5, renderInputsCV()),
                        column(6, uiOutput("Diagnostics"))
                      )
             ),
             tabPanel("Dates", titlePanel("Description of individual dates"), 
                      fluidRow(
                        column(5, renderInputs2()),
                        column(6, uiOutput("result2"))
                      ),
                      fluidRow(
                        column(4, renderInputs22()),
                        column(7, uiOutput("result22"))
                      )
                      ),
             tabPanel("Group of dates", titlePanel("Description of individual group of dates"),   
                      fluidRow(
                        column(4, renderInputs3()),
                        column(7, uiOutput("result3"))
                      ),
                      fluidRow(
                        column(4, renderInputs32()),
                        column(7, uiOutput("result32"))
                      )
                      ),
             tabPanel("Succession of groups", titlePanel("Description of a succession of groups"),    
                      fluidRow(
                        fluidRow( column(4, renderInputs4() ), column(8, uiOutput("Inputs4")) )
                      )
             )
  )
  
                  
  

  
))
