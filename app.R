library(shiny)

ui <- fluidPage(headerPanel("Minimum detectable effect size calculator", windowTitle = "MDES app"),
                tags$h4("Author: Patricio Troncoso, The University of Manchester"),
                wellPanel(tags$p("This calculator provides the Minimum Detectable Effect Size
                                 (MDES) according to the guidelines provided by the Education
                                 Endowment Foundation (EEF, 2013), which are available here: ",
                                 tags$a(href="https://educationendowmentfoundation.org.uk/public/files/Evaluation/Writing_a_Protocol_or_SAP/Pre-testing_paper.pdf", "EEF MDES guidelines.")),
                          tags$p("This is meant to be used in a 2-level Clustered Randomised Control
                                 Trial setting.
                                 This is when whole clusters (instead of individuals) are randomised.
                                 A typical situation is when complete classes or schools are allocated to
                                 either the intervention or the control/comparison group. 
                                 The underlying formula assumes that the statistical power is 0.8 and 
                                 the significance level is 0.05.
                                 If you have any comments or suggestions, please contact me via",
                                 a(href="mailto:patricio.troncoso@manchester.ac.uk", "this email")),
                          tags$p("Latest update: 28/02/2020")),
                fluidRow(column(12, wellPanel(strong("Instructions: "), "Below are the main parameters 
                                              to calculate the MDES. Default values
                                              are completely arbitrary. Modify the values and then click on the 
                                              Calculate button. The default MDES is the 2-tailed value, to obtain
                                              a one-tailed MDES, check the box."), offset= 0.5)),
                fluidRow(column(10, numericInput(inputId = "vpcempty", 
                                                 label = "Intra-class correlation of the empty model (ICC)",
                                                 value = 0.5,
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.001)), offset = 0.5),
                fluidRow(column(10, numericInput(inputId = "proprandom", 
                                                 label = "Proportion of clusters randomised to treatment",
                                                 value = 0.5,
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.001)), offset = 0.5),
                fluidRow(column(10, numericInput(inputId = "nrandom", 
                                                 label = "Total number of clusters",
                                                 value = 30,
                                                 step = 1, 
                                                 min = 0)), offset = 0.5),
                fluidRow(column(10, numericInput(inputId = "avsize", 
                                                 label = "Average size of clusters",
                                                 value = 20,
                                                 step = 0.01,
                                                 min = 0)), offset = 0.5),
                fluidRow(column(12, inputPanel(em("Modify these parameters only if you are using
                                                  a baseline covariate. See note below."),
                                               numericInput(inputId = "varexp1", 
                                                            label = "Proportion of Variance at level 1 (individuals) 
                                                            predicted by the covariate",
                                                            value = 0,
                                                            min = 0,
                                                            max = 1,
                                                            step = 0.001),
                                               numericInput(inputId = "varexp2", 
                                                            label = "Proportion of Variance at level 2 (clusters)
                                                            predicted by the covariate",
                                                            value = 0,
                                                            min = 0,
                                                            max = 1,
                                                            step = 0.001)),
                                offset = 0.5)),
                fluidRow(column(8, checkboxInput(inputId = "one.tail", 
                                                 "One-tailed MDES"), offset = 0.5)),
                fluidRow(column(8, actionButton(inputId = "calculate", label = "Calculate")),
                         offset = 0.5),
                fluidRow(column(12, h3(" ")), offset = 0),
                fluidRow(column(12, wellPanel(h3(strong(textOutput("MDES")))), offset = 0.5)),
                fluidRow(column(10, p(em(strong("Note: "), "Proportion of variance predicted 
                      by the covariate
                      should not be confused with the ICC. Variance predicted by the covariate 
                      (also known as variance explained)
                      is obtained in 2 steps (at any level): a) calculate the difference between 
                      the estimated variance in a multilevel model with the baseline 
                      covariate and another one without it (empty model); 
                      b) divide the difference by the estimated variance in the empty multilevel model.
                      For further details, see for example: Hox, J. (2010). Multilevel Analysis.
                                         Techniques and Applications. Routledge. New York and Hove. 
                                         p. 71.")), 
                                offset = 0.5)), 
                fluidRow(column(10, helpText(strong("Disclaimer: "), "This online tool comes with no warranty.
                                             Results should be used at the user's discretion."), 
                                offset = 0.5))
                )

server <- function(input, output) {
  data <- eventReactive(input$calculate, {if (input$one.tail==TRUE) {
    mdes1<-2.5*(sqrt(((input$vpcempty*(1-input$varexp2))/
                        (input$proprandom*(1-input$proprandom)*input$nrandom))+
                       ((1-input$vpcempty)*(1-input$varexp1))/
                       ((input$proprandom*(1-input$proprandom))*input$avsize*input$nrandom)))
    print((paste("One-tailed MDES", " = ", round(mdes1, digits=3))))
  }
    else {
      mdes2<-2.8*(sqrt(((input$vpcempty*(1-input$varexp2))/
                          (input$proprandom*(1-input$proprandom)*input$nrandom))+
                         ((1-input$vpcempty)*(1-input$varexp1))/
                         ((input$proprandom*(1-input$proprandom))*input$avsize*input$nrandom)))
      print(paste("Two-tailed MDES", " = ", round(mdes2, digits=3)))
    }
  })
  output$MDES <- renderText({
    data()
  })
}

shinyApp(ui=ui, server=server)