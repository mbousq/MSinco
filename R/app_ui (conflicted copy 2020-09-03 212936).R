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
  

    fluidPage(
      h3("MSinco"),
      # tags$title("MSinco"),
      # id= "navbar",
      # selected = "Home",header = "header",
      # tags$head(tags$style(HTML(
      #   "#navbar > li:first-child { display: none; }"
      # ))),
               # selected = "home" ,
      # fluidRow( div(style="display:inline-block;",h3("MSinco")), actionButton('newDirButton', 'Create directory'),actionButton('importDirButton', 'Import directory'), actionButton('go', 'go')), hr(),
      # shinyjs::useShinyjs(), 
      # tags$style(".shiny-file-input-progress {display: none}"),


     # tabPanel(title = "Home",
               
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
                   actionButton('fileButton', 'Parameter file',width = "49%",class = "btn-primary"), actionButton("folderButton", "Netcdfs folder",width = "49%",class = "btn-primary"), br(), 
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
                   # uiOutput("selectedFragment2"),
                   br(),
                   verbatimTextOutput("plot_hover_coord"),
                   
                 )
                 ,
                 
                 mainPanel(
                   
                   tabsetPanel(id = "tabs",
                               
                               
                               tabPanel("TIC", fluidRow(column(6, uiOutput('myTicsA')), column(6,uiOutput('myTicsB')))),
                               
                               tabPanel("MSpectrum", fluidRow(column(6, uiOutput('MSpecA')), column(6, uiOutput('MSpecB')))),
                               
                               tabPanel("SIM", fluidRow(column(6, uiOutput('SIMA')), column(6, uiOutput('SIMB')))),
                               
                               tabPanel("Parameters", fluidRow(column(6, DT::DTOutput("table")), column(6, wellPanel(numericInput("mzd2","Mass difference",value = 0.3),  checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isotopeCorrection")), actionButton("saveButton", "Save"),actionButton("runButton2", "Save & Run"))) ))
                               
                               # tabPanel("Parameters",id = "Parameters", fluidRow(column(5, rHandsontableOutput('hot')), column(2,  shinyjs::hidden(actionButton("saveButton", "Save file")),  shinyjs::hidden(actionButton("runButton", "Run"))), column(2,  shinyjs::hidden(numericInput("mzd2","mass difference",value = 0.3)),  shinyjs::hidden(checkboxGroupInput("runParameters", NULL, list("savePlots", "baselineCorrection", "isoCor", "correctTracerImpurity"),inline = F,selected = list("savePlots", "baselineCorrection", "isoCor"))))))
                               # 
                               #  rhandsontable::rHandsontableOutput('hot')
                   )
                 )
               )
            #)
      # ,
      # tags$script(
      #   HTML("var header = $('.navbar > .container-fluid');
      #                         header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
      # )
      
      # 
      # navbarMenu("import",
      #            tabPanel("Import directory"),
      #            tabPanel("Create directory"))
      # tagList(
      # # tabPanel(title = "Create directory", value = "newdir"),
      # tags$script(
      #   HTML("var header = $('.navbar > .container-fluid');
      #                         header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
      # ),
      # tags$script(
      #   HTML("var header = $('.navbar > .container-fluid');
      #                         header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
      # ))
      # tags$script(
      #   HTML("var header = $('.navbar > .container-fluid');
      #                         header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"newDirButton\" type=\"button\" class=\"btn btn-primary action-button\" >Create directory</button></div>')")
      # )
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

