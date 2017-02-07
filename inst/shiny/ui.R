library(shiny)
library(shinythemes)
library(ArchaeoPhases)
library(DT)
library(dplyr)
library(hdrcde)
library(coda)

renderInputs0 <- function() {
  #wellPanel(
    fluidRow(
      h4("Post-Processing of the Markov Chain Simulated by ChronoModel or by Oxcal"),
      br(),
      h4("This is a Shiny application for ChronoModel or Oxcal users who want to use ArchaeoPhases package without having to know R software."),
      h4("The process is very simple !"),
      h4("First model your chronology with ChronoModel or Oxcal or any other software for constructing archaeological chronologies.  "),
      h4("Then extract the simulated Markov Chains and save it into a CSV file. "),
      h4("And finally, import this CSV file using the app and analyse your chronological phases. "),
      br(),
      tags$div(class="header", checked=NA,
                  tags$p("To visite ChronoModel website"), tags$a(href="http://www.chronomodel.fr/", "Click Here!"), 
               br(),
                  tags$p("To visite Oxcal website"), tags$a(href="https://c14.arch.ox.ac.uk/oxcalhelp/hlp_contents.html", "Click Here!")
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
               tags$p("This application was developed by Anne Philippe and Marie-Anne Vibet"),
               tags$p("Maintainer : Anne Philippe <anne.philippe@univ-nantes.fr>")
      )
      
  )}



renderInputs <- function() {
  wellPanel(
    fluidRow(
      fileInput(inputId = "file", label ="Choose file", multiple = FALSE),
      h5(helpText("Select separators below to read the CSV file")),
      column(6,radioButtons(inputId ='sep', label="Cell separator", choices=c(Comma=',', Semicolon=';', Tab='\t', Space=''), selected=',')),
      column(6,radioButtons(inputId='dec', label="Decimal separator", choices=c(Comma=',', Dot='.'), selected='.') )
    )
    
  )}

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
      h5(helpText("Graphical options")),
      textInput(inputId='titleIntervalsplot', label="Intervals plot title", "Intervals plot" ),
      radioButtons(inputId='intervals', label="Intervals", choices=c(CI='CI', HPD='HPD'), selected='CI') ,
      textInput(inputId='titleTempoplot', label="Tempo plot title", "Tempo plot" ),
      column(6,radioButtons(inputId='GaussCI', label="Gaussian approx", choices=c(Yes='TRUE', No='FALSE'), selected='FALSE')), 
      column(6,radioButtons(inputId='count', label="Counting process", choices=c(Number='TRUE', Probability='FALSE'), selected='TRUE'))
    )
    
  )}



renderInputs3 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of a phase"),
      selectInput("variablesalpha", "Select chain names", choices = character(0), multiple = FALSE, selected = NULL),
      selectInput("variablesbeta", "Select chain names", choices = character(0), multiple = FALSE, selected = NULL),
      br(),
      h5(helpText("Statistical options")),
      numericInput(inputId ='level2', label="Confidence level", value=0.95, min=0, max=1),
      br(),
      h5(helpText("Graphical options")),
      textInput(inputId='titlePlot2', label="Title", "Characterisation of a phase" ),
      column(6,radioButtons(inputId='color2', label="Colors", choices=c(Yes='TRUE', No='FALSE'), selected='TRUE') )
    )
  )}

renderInputs32 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of phases for at least two phases"),
      br(),
      h4(helpText("Warning : If phases are nested in each other, the CSV file should be reorganised. ")),
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
      textInput(inputId='titleMultiPhases', label="Plot title", "Characterisation of several phases" )
    )
  )}

renderInputs4 <- function() {
  wellPanel(
    fluidRow(
      h3("Selection of phases in succession"),
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
      textInput(inputId='titleSuccessionPlot', label="Plot title", "Characterisation of a succession of phases" )
    )
    
  )}

shinyUI(fluidPage(
  headerPanel('ArchaeoPhases'),
  titlePanel('   Analysis of archaeological phases'),
  
  navbarPage(" ", theme = shinytheme("cerulean"),
             tabPanel("Home", 
                      renderInputs0()
             ),
             tabPanel("Import CSV", titlePanel("Import your CSV file"), 
                      fluidRow(
                        column(5, renderInputs()),
                        column(6, uiOutput("sum"))
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
             tabPanel("Phases", titlePanel("Description of individual phases"),   
                      fluidRow(
                        column(4, renderInputs3()),
                        column(7, uiOutput("result3"))
                      ),
                      fluidRow(
                        column(4, renderInputs32()),
                        column(7, uiOutput("result32"))
                      )
                      ),
             tabPanel("Succession of phases", titlePanel("Description of a succession of phases"),    
                      fluidRow(
                        fluidRow( column(4, renderInputs4() ), column(8, uiOutput("Inputs4")) )
                      )
             )
  )
  
                  
  

  
))
