shinyUI(bootstrapPage(
  titlePanel("Model Results Multiplication"),

  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Model Data",
        fluidRow(
          column(2, wellPanel(
            h4("Request a chunk:"),
            textInput("request_slot1",
                            "Slot 1",
                            "3"),
            textInput("request_slot2",
                            "Slot 2",
                            "x"),
            textInput("request_slot3",
                            "Slot 3",
                            "4"),
            actionButton("request_button",
                            "Request"),
            checkboxInput("compare_data", "Compare Model to Data", FALSE)
      #      textInput("request_slot4",
      #                      "Slot 4",
      #                      "Slot 4"),
      #      textInput("request_slot5",
      #                      "Slot 5",
      #                      "Slot 5"),
      #      textInput("request_slot6",
      #                      "Slot 6",
      #                      "Slot 6")
          )),
          column(10, wellPanel(
            plotOutput(outputId = "main_plot", height = "300px")#,
          ))),

        fluidRow(
          h2("Parameters:"),
          column(3, wellPanel(        
             # sliderInput("inputAct", "Input Activation",0,4, 0, step = 0.5),  
              sliderInput("decay", "Decay Parameter", -1, 0, -0.5, step = 0.1)
          )),
          column(3, wellPanel(
              sliderInput("mp", "Mismatch Penalty", 0, 10, 5, step = 0.5),
              sliderInput("ans", "Activation Noise", 0, 1, 0.2, step = 0.05)
          )),
          column(3, wellPanel(
              radioButtons("ol", "Optimized Learning", c("On" = "on", "Off" = "off")),
              selectInput("simfun", "Similarity Function", c("Linear" = "linear", "Christian" = "christian"))
          )),
          column(3, wellPanel(
              selectInput("param_group", "Parameter Settings", c("Manual", "Christian", "Default"))
          ))
        ),

        fluidRow(
          h2("References:"),
          column(4, wellPanel(        
             # actionButton("reset", "Reset"),  
              radioButtons("response_time", "Response Times", c("Model Time" = "mtime", "Data Time" = "dtime")),
              sliderInput("problem", "Problem", 0, 1619, 0, step = 1)
          ))#,
          #column(4, wellPanel(
              #sliderInput("mp", "Mismatch Penalty", 0, 10, 5, step = 0.5),
              #sliderInput("ans", "Activation Noise", 0, 1, 0.2, step = 0.05)
          #)),
          #column(4, wellPanel(
              #radioButtons("ol", "Optimized Learning", c("On" = "on", "Off" = "off"))
          #))
        )
      ),
      tabPanel("Declarative Memory",
        mainPanel(dataTableOutput("chunks"), width=12)
      ),
      tabPanel("Presented Problems",
        mainPanel(dataTableOutput("problemsSoFar"), width=12)
      )
    )
  )
))#)
