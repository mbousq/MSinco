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
    tags$head(tags$script(HTML(keybinding_return))),
    # tags$head(tags$script(
    #   HTML("var header = $('.navbar > .container-fluid');
    #                           header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
    # )),
    # theme = "paper.min.css",
  

    navbarPage(
      "MSinco",
      # tags$title("MSinco"),
      id= "navbar",
      selected = "Settings",
      shinyjs::useShinyjs(),
      # tags$head(tags$style(HTML(
      #   "#navbar > li:first-child { display: none; }"
      # ))),
               # selected = "home" ,
      # fluidRow( div(style="display:inline-block;",h3("MSinco")), actionButton('newDirButton', 'Create directory'),actionButton('importDirButton', 'Import directory'), actionButton('go', 'go')), hr(),
      # shinyjs::useShinyjs(),
      # tags$style(".shiny-file-input-progress {display: none}"),


      tabPanel(title = "Home",
               
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 # change background color of sidebar panel: include tags$style(".well {background-color:#f2f3f5;}") inside
                 # default:"position:fixed;width:inherit;"
                 
                 sidebarPanel(
                   width = 3,style = "position:fixed;width:22%;", 
                   # fileInput("rawDataFiles","select raw data files",multiple = T),
                   # selectInput("action", "create or import experiment",choices = c("choose action","create experiment","import experiment")),
                   
                   
                   # actionButton('addDataFiles', 'Add data files',width = "49%"),
                   # checkboxInput("includeSampleData","include sample data ?", value = FALSE), checkboxInput("includeParameters","include Parameters examples ?", value = TRUE),
                   # actionButton('start', 'Start',width = "49%"),
                   # actionButton('fileButton', 'Parameter file',width = "49%"), actionButton("folderButton", "Netcdfs folder",width = "49%"), br(), 
                   # uiOutput("LB"),
                   # uiOutput("RB"), # hr(),
                   h5("*Parameters*"),
                   shinyjs::hidden(uiOutput("selectedFragment")),
                   # uiOutput("selectedFiles"),
                   uiOutput("rtime"), # hr(),
                   uiOutput("labelThreshold"),
                   uiOutput("rtimeL"),
                   uiOutput("rtimeR"),
                   uiOutput("mass0"),
                   uiOutput("N_atom"),
                   uiOutput("mzd"),
                   # uiOutput("selectedFragment"),
                   
                   
                   # # uiOutput("selectedFragment_tic"),
                   # uiOutput("saveActivePlotsButton"),
                   # uiOutput("saveTotableButton"),
                   # uiOutput("runButton"),
                   # uiOutput("undoButton"),
                   # uiOutput("runButton"),
                   # uiOutput("run1"),
                   # uiOutput("run2"),
                   # uiOutput("run3"),
                   # uiOutput("msUI"),
                   # uiOutput("ticUI"),
                   # uiOutput("simUI"),
                   
                   # uiOutput("ticUI"),
                   # uiOutput("msUI"),
                   # uiOutput("simUI"),
                   # uiOutput("paramUI"),
                   
                   # conditionalPanel(
                   #   'input.tabs === "TIC"',
                   #   uiOutput("ticUI"),
                   # ),
                   # conditionalPanel(
                   #   'input.tabs === "MSpectrum"',
                   #   uiOutput("msUI")
                   #   ,
                   #   numericInput(inputId = "rtime",
                   #                label = "Retention time",
                   #                #min = min(values[["rawData"]])1()@featureData@data$retentionTime),
                   #                #max = max(values[["rawData"]])1()@featureData@data$retentionTime),
                   #                #value = rows(values[["DF"]],values[["DF"]][[1]] ==input$isolate(input$selectedFragment))[[2]], #max(values[["rawData"]])1()@featureData@data$retentionTime)/2,
                   #                value = 0,
                   #                step = 0.1),
                   # ),
                   # conditionalPanel(
                   #   'input.tabs === "SIM"',
                   #   uiOutput("simUI"),
                   # ),
                   # conditionalPanel(
                   #   'input.tabs === "Parameters"',
                   #   uiOutput("paramUI"),
                   # ),

                   conditionalPanel(
                     'input.tabs === "TIC" && output.selectedFragment',
                     # shinyjs::hidden(uiOutput("selectedFragment")),
                     # numericInput(inputId = "rtimeL",
                     #              label = "Retention time (left)",
                     #              #min = min(rawData1()@featureData@data$retentionTime),
                     #              #max = max(rawData1()@featureData@data$retentionTime),
                     #              value = NULL, # max(rawData1()@featureData@data$retentionTime)/2-10,
                     #              step = 0.1),
                     # 
                     # numericInput(inputId = "rtimeR",
                     #              label = "Retention time (right)",
                     #              #min = min(rawData1()@featureData@data$retentionTime),
                     #              #max = max(rawData1()@featureData@data$retentionTime),
                     #              value = NULL, # max(rawData1()@featureData@data$retentionTime)/2+10,
                     #              step = 0.1),

                     actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px"),
                     actionButton("run1", "run", width = "100%", style="margin-bottom:8px")

                                        ),
                   conditionalPanel(
                     'input.tabs === "MSpectrum" && output.selectedFragment',
                     # numericInput(inputId = "labelThreshold",
                     #              label = "Threshold intensity for labels",
                     #              #min = 0,
                     #              #max = 1,
                     #              value = NULL,
                     #              step = 5
                     # ),
                     # 
                     # numericInput("rtime", label = "retention time",
                     #                    #min = min(rawData1()@featureData@data$retentionTime),
                     #                    #max = max(rawData1()@featureData@data$retentionTime),
                     #                    #value = rows(values[["DF"]],values[["DF"]][[1]] ==input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
                     #                    value = NULL),

                     actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px"),
                     actionButton("run2", "run", width = "100%", style="margin-bottom:8px")

                   ),
                   conditionalPanel(
                     'input.tabs === "SIM" && output.selectedFragment',
                     # numericInput(inputId = "rtimeL",
                     #              label = "Retention time (left)",
                     #              #min = min(rawData1()@featureData@data$retentionTime),
                     #              #max = max(rawData1()@featureData@data$retentionTime),
                     #              value =NULL, # max(rawData1()@featureData@data$retentionTime)/2-10,
                     #              step = 0.1),
                     # 
                     # numericInput(inputId = "rtimeR",
                     #              label = "Retention time (right)",
                     #              #min = min(rawData1()@featureData@data$retentionTime),
                     #              #max = max(rawData1()@featureData@data$retentionTime),
                     #              value = NULL, # max(rawData1()@featureData@data$retentionTime)/2+10,
                     #              step = 0.1),
                     # numericInput(inputId = "mass0",
                     #              label = "mass0 (M0)",
                     #              #min = 40,
                     #              #max = 600,
                     #              value = NULL,
                     #              step = 1
                     # ),
                     # 
                     # 
                     # numericInput(inputId = "N_atom",
                     #              label = "Number of isotopomers",
                     #              #min = 1,
                     #              #max = 10,
                     #              value = NULL,
                     #              step = 1
                     # ),
                     # 
                     # numericInput(inputId = "mzd",
                     #              label = "Mass difference",
                     #              #min = 0,
                     #              #max = 1,
                     #              value = NULL,
                     #              step = 0.1),

                     actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px"),
                     actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px"),
                     actionButton("run3", "run", width = "100%", style="margin-bottom:8px")


                                        ),
                #    conditionalPanel(
                #      'input.tabs === "Parameters"',
                # 
                #      numericInput("mzd2","Mass difference",value = 0.3),
                #      checkboxGroupInput("runParameters", NULL, 
                #                         list("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),
                #                         inline = F,selected = list("savePlots", "baselineCorrection", "isotopeCorrection")), 
                #      actionButton("saveButton", "Save table", width = "49%", style="margin-bottom:8px"),
                #      actionButton("undoButton","undo", width = "49%", style="margin-bottom:8px"),
                #      actionButton("runButton2", "Save table & Run", width = "100%", style="margin-bottom:8px"), 
                #      
                #      
                #      br(),
                #      # numericInput(inputId = "rtimeL",
                #      #              label = "Retention time (left)",
                #      #              #min = min(rawData1()@featureData@data$retentionTime),
                #      #              #max = max(rawData1()@featureData@data$retentionTime),
                #      #              value =NULL, # max(rawData1()@featureData@data$retentionTime)/2-10,
                #      #              step = 0.1),
                #      # 
                #      # numericInput(inputId = "rtimeR",
                #      #              label = "Retention time (right)",
                #      #              #min = min(rawData1()@featureData@data$retentionTime),
                #      #              #max = max(rawData1()@featureData@data$retentionTime),
                #      #              value = NULL, # max(rawData1()@featureData@data$retentionTime)/2+10,
                #      #              step = 0.1),
                #      # numericInput(inputId = "mass0",
                #      #              label = "mass0 (M0)",
                #      #              #min = 40,
                #      #              #max = 600,
                #      #              value = NULL,
                #      #              step = 1
                #      # ),
                #      # 
                #      # 
                #      # numericInput(inputId = "N_atom",
                #      #              label = "Number of isotopomers",
                #      #              #min = 1,
                #      #              #max = 10,
                #      #              value = NULL,
                #      #              step = 1
                #      # ),
                #      # 
                #      # numericInput(inputId = "mzd",
                #      #              label = "Mass difference",
                #      #              #min = 0,
                #      #              #max = 1,
                #      #              value = NULL,
                #      #              step = 0.1),
                #      # 
                #      # actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px"),
                # 
                # ),

                       
                  #  actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px"),
                  #  actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px"),
                  #  actionButton("run1", "run", width = "100%", style="margin-bottom:8px"),
                  #  actionButton("run3", "run", width = "100%", style="margin-bottom:8px"),
                  # actionButton("run2", "run", width = "100%", style="margin-bottom:8px"),
                  # actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px"),
                     


                   # uiOutput("selectedFragment2"),
                   br(),
                   verbatimTextOutput("plot_hover_coord"),
                   
                 )
                 ,
                 
                 mainPanel(
                   
                   tabsetPanel(id = "tabs",
                               
                               
                               tabPanel("TIC", fluidRow(column(6, uiOutput('plots_ticA')), column(6,uiOutput('plots_ticB')))),
                               
                               tabPanel("MSpectrum", fluidRow(column(6, uiOutput('plots_msA')), column(6, uiOutput('plots_msB')))),
                               
                               tabPanel("SIM", fluidRow(column(6, uiOutput('plots_simA')), column(6, uiOutput('plots_simB'))))
                               # ,
                               # 
                               # tabPanel("Parameters", fluidRow(column(6, rhandsontable::rHandsontableOutput('hot')) ))
                               #tabPanel("Parameters", fluidRow(column(6, rhandsontable::rHandsontableOutput('hot')), column(6, wellPanel(numericInput("mzd2","Mass difference",value = 0.3),  checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isotopeCorrection")), actionButton("saveButton", "Save"),actionButton("runButton2", "Save & Run"))) ))

                               # tabPanel("Parameters",id = "Parameters", fluidRow(column(5, rHandsontableOutput('hot')), column(2,  shinyjs::hidden(actionButton("saveButton", "Save file")),  shinyjs::hidden(actionButton("runButton", "Run"))), column(2,  shinyjs::hidden(numericInput("mzd2","mass difference",value = 0.3)),  shinyjs::hidden(checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isoCor", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isoCor"))))))
                               # 
                               #  rhandsontable::rHandsontableOutput('hot')
                               # DT::DTOutput("table")
                   )
                 )
               )
            ),
      tabPanel("Parameters",
               sidebarLayout(
               sidebarPanel(width = 3,
                            
                            numericInput("mzd2","Mass difference",value = 0.3),
                            checkboxGroupInput("runParameters", NULL, 
                                               list("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),
                                               inline = F,selected = list("savePlots", "baselineCorrection", "isotopeCorrection")), 
                            actionButton("saveButton", "Save table", width = "49%", style="margin-bottom:8px"),
                            actionButton("undoButton","undo", width = "49%", style="margin-bottom:8px"),
                            actionButton("runButton2", "Save table & Run", width = "100%", style="margin-bottom:8px")
                            
                            ),
               mainPanel(
                         tabPanel("Parameters", fluidRow(column(6, rhandsontable::rHandsontableOutput('hot')) ))
               )
               )
               ),

      navbarMenu("Import",
                 tabPanel(title = "Create experiment"),
                 tabPanel(title = "Import experiment")
                 ),
      
      navbarMenu("Export",
                 tabPanel(title = "Active plots"),
                 tabPanel(title = "All plots")),
      tabPanel("Settings",
               
               wellPanel(checkboxGroupInput("settings", "Choose settings", 
                                            choiceNames = c("use parallel processing to import the data ? (parallel processing is only at the import stage, not subsequently)", "use ggplot graphics to plot ? (slower) ", "use lattice graphics to plot ? (faster)") , 
                                            choiceValues = c("parallel","ggplot","lattice"),selected = c("parallel","lattice") ))),
      tags$script(
        HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top: 15px\"><button id=\"quitButton\" type=\"button\" class=\"btn btn-danger action-button\" >Quit</button></div>')")
      )
      
               
      
      
      
      # wellPanel(checkboxGroupInput("settings", "Choose settings", 
      #                              choiceNames = c("use parallel processing to import the data ? (parallel processing is only at the import stage, not subsequently)", "use ggplot graphics to plot ? (slower) ", "use lattice graphics to plot ? (faster)", "use cached plots ? (when ON, each generated plot is saved temporarily (cached) and can be re-accessed without re-evaluation). In other words: plots are generated once only.") , 
      #                              choiceValues = c("parallel","ggplot","lattice","cache") ))
      # 
      # tabPanel(title = "runCorrections",
      #          
      # ),
      # 
      # tags$script(
      #   HTML("var header = $('.navbar > .container-fluid');
      #                         header.append('<div style=\"float:right; padding-top: 15px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
      # )
      
      )
    
      # tabPanel(title = "Import directory", value = "importDir"),
      
      #shinyFilesButton('files', 'File select', 'Please select a file', FALSE),
      
      # fluidRow(column(2,actionButton("folderButton", "Netcdfs Folder"),actionButton('fileButton', 'Parameter file')), column(4,checkboxGroupInput("savePlots",NULL, list("TIC" = 1, "SIM" = 2, "MSpectrum" = 3),inline = T)), column(2,actionButton("savePlotsButton", "Save plots"))
      # ),
      # wellPanel(
      #   fluidRow(column(2,actionButton("folderButton", "Netcdfs Folder")), column(10,checkboxGroupInput("savePlots",NULL, c("TIC", "SIM", "MSpectrum"),inline = T,))
      # ),
      #   fluidRow(column(2,actionButton('fileButton', 'Parameter file')), column(10,actionButton("savePlotsButton", "Save plots"))
      # )),
      
      
      # Sidebar layout with a input and output definitions ----
      
    # )
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

