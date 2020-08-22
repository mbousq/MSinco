#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      h1("MSinco"),
      # shinyjs::useShinyjs(), 
      # theme = shinytheme("paper"),
      theme = "paper.min.css",
      # tags$link(rel = "stylesheet", type = "text/css", href = "litera.css"),
      # tags$head(tags$script(src = "keyboard_bindings.js")),
      # tags$script(src = "keyboard_bindings.js"),
      tags$head(tags$script(HTML(keybinding_return))),
      
      #shinyFilesButton('files', 'File select', 'Please select a file', FALSE),
      
      # fluidRow(column(2,actionButton("folderButton", "Netcdfs Folder"),actionButton('fileButton', 'Parameter file')), column(4,checkboxGroupInput("savePlots",NULL, list("TIC" = 1, "SIM" = 2, "MSpectrum" = 3),inline = T)), column(2,actionButton("savePlotsButton", "Save plots"))
      # ),
      # wellPanel(
      #   fluidRow(column(2,actionButton("folderButton", "Netcdfs Folder")), column(10,checkboxGroupInput("savePlots",NULL, c("TIC", "SIM", "MSpectrum"),inline = T,))
      # ),
      #   fluidRow(column(2,actionButton('fileButton', 'Parameter file')), column(10,actionButton("savePlotsButton", "Save plots"))
      # )),
      
      # Add file upload manager
      #fileInput("file", label = h3("File input")),
      # hr(),
      # fluidRow(column(4, verbatimTextOutput("fileValue"))),
      
      # # App title
      # titlePanel("MSinco"),
      
      
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        # change background color of sidebar panel: include tags$style(".well {background-color:#f2f3f5;}") inside
        # default:"position:fixed;width:inherit;"
        
        sidebarPanel(width = 3,style = "position:fixed;width:22%;", 
                     actionButton('newDirButton', 'Create directory',width = "49%"),actionButton('importDirButton', 'Import directory',width = "49%"),
                     actionButton('fileButton', 'Parameter file',width = "49%"), actionButton("folderButton", "Netcdfs folder",width = "49%"), br(), 
                     uiOutput("LB"),
                     uiOutput("RB"), # hr(),
                     uiOutput("rtime"), # hr(),
                     uiOutput("rtimeL"),
                     uiOutput("rtimeR"),
                     uiOutput("mass0"),
                     uiOutput("N_atom"),
                     uiOutput("mzd"),
                     uiOutput("labelThreshold"),
                     uiOutput("selectedFragment"),
                     uiOutput("selectedFragment_tic"),
                     uiOutput("saveActivePlotsButton"),
                     uiOutput("saveTotableButton"),
                     uiOutput("runButton"),
                     # uiOutput("undoButton"),
                     #uiOutput("selectedFragment2"),
                     br(),
                     # actionButton("refreshInputsButton","Refresh Inputs"),
                     verbatimTextOutput("plot_hover_coord"),
                     
                     #                  shinyjs::hidden(uiOutput("LB")),
                     #  shinyjs::hidden(uiOutput("RB")),hr(),
                     #  shinyjs::hidden(uiOutput("rtime")),hr(),
                     #  shinyjs::hidden(uiOutput("rtimeL")),
                     # shinyjs::hidden(uiOutput("rtimeR")),
                     #  shinyjs::hidden(uiOutput("mass0")),
                     #  shinyjs::hidden(uiOutput("N_atom")),
                     # shinyjs::hidden(uiOutput("mzd")),
                     # 
                     
                     #                 uiOutput("LB"),
                     #  uiOutput("RB"),hr(),
                     #  uiOutput("rtime"),hr(),
                     #  uiOutput("rtimeL"),
                     # uiOutput("rtimeR"),
                     #  uiOutput("mass0"),
                     #  uiOutput("N_atom"),
                     # shinyjs::hidden(uiOutput("mzd")),
                     
                     
                     # Input: Selector for choosing dataset ----
                     # sliderInput("RTrange", "Range:",
                     #            min = min(rawData1()()@featureData@data$retentionTime), max = max(rawData1()()@featureData@data$retentionTime),
                     #            value = c(min(rawData1()()@featureData@data$retentionTime),max(rawData1()()@featureData@data$retentionTime)))
                     
        ),
        #Split layout
        #       splitLayout(
        #   plotOutput("tic"),
        #   plotOutput("SIM")
        # ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          #     fluidRow(column(3,checkboxGroupInput("savePlots",NULL, list("TIC", "SIM", "MSpectrum"),inline = T)), column(1, actionButton("savePlotsButton", "Save active plots"))
          # ), 
          # intCorUI("intCor"),
          # checkboxGroupInput("savePlots",NULL, list("TIC", "SIM", "MSpectrum"),inline = T), actionButton("savePlotsButton", "Save plots"),
          
          # fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
          #                      plotOutput("tic"), 
          #                      plotOutput("SIM"))),
          
          # Output: Verbatim text for data summary ----
          #verbatimTextOutput("summary"),
          
          # Output: HTML table with requested number of observations ----
          tabsetPanel(id = "tabs",
                      # The splitLayout(cellWidths = c("50%", "50%") render blurred plots (half of them)
                      # tabPanel("TIC", 
                      #           fluidRow(splitLayout(cellWidths = c("50%", "50%"), uiOutput('myTicsA'),uiOutput('myTicsB'))))
                      
                      tabPanel("TIC", fluidRow(column(6, uiOutput('myTicsA')), column(6,uiOutput('myTicsB')))),
                      #, actionButton("savePlotsButton", "Save plots")
                      tabPanel("MSpectrum", fluidRow(column(6, uiOutput('MSpecA')), column(6, uiOutput('MSpecB')))),
                      
                      tabPanel("SIM", fluidRow(column(6, uiOutput('SIMA')), column(6, uiOutput('SIMB')))),
                      
                      tabPanel("Parameters", fluidRow(column(6, DT::DTOutput("table")), column(6, wellPanel(numericInput("mzd2","Mass difference",value = 0.3),  checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isotopeCorrection")), actionButton("saveButton", "Save"),actionButton("runButton2", "Save & Run"))) ))
                      
                      # tabPanel("Parameters",id = "Parameters", fluidRow(column(5, rHandsontableOutput('hot')), column(2,  shinyjs::hidden(actionButton("saveButton", "Save file")),  shinyjs::hidden(actionButton("runButton", "Run"))), column(2,  shinyjs::hidden(numericInput("mzd2","mass difference",value = 0.3)),  shinyjs::hidden(checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isoCor", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isoCor"))))))
                      # 
                      #  rhandsontable::rHandsontableOutput('hot')
                      
          ), 
          # fluidRow(
          #          column(4,  shinyjs::hidden(numericInput("mzd2","mass difference",value = 0.3)),  shinyjs::hidden(checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isoCor", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isoCor"))))
          #          ),
          
          # Dynamic tab generation
          verbatimTextOutput("debug") #, uiOutput('mytabs')
          # tabsetPanel(type = "tabs",
          #            
          #             sapply(rep(paste0("tabPanel('", "rawData", 1:8,"')")), function(x) eval(parse(text=x)))
          #rlang::parse_exprs(rep(paste0("tabPanel('", "rawData", 1:8,"')")))
          #eval(parse(text=(rep(paste0("tabPanel('", "rawData", 1:8,"')")))))
          #tabPanel('rawData1'), tabPanel('rawData')
          #parse(text=(rep(paste0("tabPanel('", "rawData", 1:8,"')",collapse = ";"))))
          
          #tabPanel('rawData1'),tabPanel('rawData'),tabPanel('rawData3'),tabPanel('rawData4'),tabPanel('rawData5'),tabPanel('rawData6'),tabPanel('rawData7'),tabPanel('rawData8')
          # parse(text=(rep(paste0("tabPanel('", "rawData", 1:8,"', plotOutput('tic",1:8,"'))",collapse = ";")))) %>% eval()
          #      ) # parenthesis of tabsetPanel
          # length(rawData())
          # rep(paste0("tabPanel('", "rawaData", 1:length(rawData),"', plotOutput('tic",1:length(rawData),"'),","plotOutput('SIM'),", "plotOutput('MSpectrum'))",collapse = ","))
          
          # plotOutput("tic"), plotOutput("SIM"), plotOutput("MSpectrum"), dataTableOutput("table"), verbatimTextOutput("debug"),verbatimTextOutput("netcdfPath") #
          
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MSinco'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

