library(shiny)
library(DT)
library(scales)

source("funcs_for_app.txt")
a20p = read.table("song_info.txt", header=TRUE)
allcharts = readRDS("all_steps.RData")

mixes = c("1stMIX","2ndMIX","3rdMIX","4thMIX","5thMIX","6thMIX MAX","7thMIX MAX2","8thMIX EXTREME","SuperNOVA","SuperNOVA2","X","X2","X3","2013","2014","A","A20","A20 Plus")
clevels = seq(1,19,1)
difficulties = c("Beginner","Basic","Difficult","Expert","Challenge")

ui <- fluidPage(
  titlePanel("Pattern Search Over Dance Dance Revoultion A20+ Charts"),
  
  uiOutput("mygithub"),
  headerPanel(""),
  
  fluidRow(
    column(4,
      "Select Level Range",
      numericInput("clevelmin","Minimum Level",value=10,min=1,max=19),
      numericInput("clevelmax","Maximum Level",value=15,min=1,max=19),
      "Specify BPM Parameters",
      numericInput("bpmmin","Minimum BPM",value = 1, min = 1, max = 1050),
      numericInput("bpmmax","Maximum BPM",value = 1050, min = 1, max = 1050),
      checkboxInput("staticbpm", 'Require Static BPM?', FALSE),
    ),
    column(4,
      "Select Difficulties",
      checkboxGroupInput("cdiff","Difficulty",difficulties,selected=c("Expert","Challenge")),
    ),
    column(4,
      "Select Mixes",
      checkboxGroupInput("mix","Mix",mixes,selected=mixes),
    ),
  ),
  actionButton("filter","Create Filter"),
  fluidRow(
    DTOutput("filterlist"),
  ),

  fluidRow(
    column(4,
      "Hi Mom",
      radioButtons("patterntype","Please Select Type of Pattern", c("Sequence Only","Timing Only",
                  "Sequence and Timing"),("Sequence Only")),
    ),
    column(4,
      "Input Step Information",
      conditionalPanel(
        condition = "input.patterntype == 'Sequence Only'",
        textInput('po_ap', "Arrow Pattern  (e.g. L U R L D R)", "L U R L D R"),
        textInput('po_ns', "New Step? [1:yes, 0:no] (e.g. 1 0 1 1 0 1 1 0)", "1 1 1 1 1 1"),
        numericInput("po_mb", "Maximum Beats Pattern Must Occur Within", 2)
      ),
      conditionalPanel(
        condition = "input.patterntype == 'Timing Only'",
        textInput('to_aps', "Arrows Per Step [2:jump, 1:tap] (e.g. 2 1 2 1 2)", "2 1 2 1 2"),
        textInput('to_tbs', "Timing Between Steps (e.g. 0 8 16 16 4)", "0 8 8 8 8"),
      ),
      conditionalPanel(
        condition = "input.patterntype == 'Sequence and Timing'",
        textInput('pt_ap', "Arrow Pattern (e.g. L U R L D R)", "L U R L R U L"),
        textInput('pt_tbs', "Timing Between Steps (e.g. 0 8 16 16 4)", "0 16 16 8 8 16 16"),
      ),
    ),
    column(4,
      "Pattern Visualization",
      actionButton("patplotgo","Generate Visual"),
      plotOutput("patplot",height="400px",width="200px")
    )
  ),
  
  actionButton("search_start","Run Search, Please allow up to 30 seconds for large filters"),
  DTOutput("search_tab"),
  
  fluidRow(
    column(4,
      selectizeInput(inputId = "chart_to_viz", label = "Select a chart", multiple=FALSE, choices = NULL),
    ),
    column(4,
      radioButtons("xorm", "How would you like to dtermine scrolling speed?",
                    choices = c("Enter Xmod", "Specify Max Scroll Speed"), "Enter Xmod"),
      conditionalPanel(
        condition = "input.xorm == 'Enter Xmod'",
        numericInput('xmod', "Enter Xmod", 3, min=.25,max=8,step=.25),
      ),
      conditionalPanel(
        condition = "input.xorm == 'Specify Max Scroll Speed'",
        numericInput('mmod', "Specify Max Scroll Speed", 450, min=100,max=1000,step=1),
      ),
      textOutput("calcxmod")
    ),
    column(4,
      actionButton("chart_viz","Generate Chart Visual")
    )
  ),

  plotOutput("chartplot")
)


server <- function(input, output, session) {
  
  url = a("https://github.com/nrobertson573/DDR_Pattern_Search#readme", 
          href="https://github.com/nrobertson573/DDR_Pattern_Search#readme:")
  output$mygithub = renderUI({
    tagList("Github Readme link:", url)     
  })

  filterlist = eventReactive(input$filter, {
    filter.chartlist(songlist=a20p,diff=input$cdiff,level=c(input$clevelmin:input$clevelmax),
      mix=input$mix,bpm_static=input$staticbpm,bpm_min=input$bpmmin,bpm_max=input$bpmmax)
  })

  output$filterlist = renderDT({
    datatable(filter_table(filterlist(),songlist=a20p))
  })
 
  PL = eventReactive(input$patplotgo, {
    if(input$patterntype == "Sequence Only"){
      innerout = PL_make(arrow_p=input$po_ap,new_step=input$po_ns,maxbeats=input$po_mb)
    }
    if(input$patterntype == "Timing Only"){
      innerout = PL_make(arrows_per_step=input$to_aps,timing_between_steps=input$to_tbs)
    }
    if(input$patterntype == "Sequence and Timing"){
      innerout = PL_make(arrow_p=input$pt_ap,timing_between_steps=input$pt_tbs)
    }
    innerout
  })

  patplotER = eventReactive(input$patplotgo, {
      patplot_rs(PL(),input$patterntype)
  }, ignoreNULL = FALSE)
  
  output$patplot = renderPlot({
    patplotER()
  })
  
#  observeEvent(input$patplotgo, {
#    output$patplot = renderPlot({
#      patplot_rs(PL(),input$patterntype)
#    })
#  }, ignoreNULL = FALSE)
  
  search_res = eventReactive(input$search_start,{
    search_run(filterlist(),PL(),allcharts)
  })

  output$search_tab = renderDT({
    datatable(search_tab_gen(search_res(),a20p), options = list(order=list(8, 'desc')))
  })
  
  observeEvent(input$search_start, {
    updateSelectizeInput(session, inputId = "chart_to_viz", choices = sort(filterlist()))
  })
  
  rxmod = reactive( {
    fso = search_res()
    if(input$xorm == "Enter Xmod"){ rxmod = input$xmod }
    if(input$xorm == "Specify Max Scroll Speed"){
      fso = search_res()
      flo = filterlist()
      flt = filter_table(filterlist(),songlist=a20p)
      i = which(flo == input$chart_to_viz)
      cmaxbpm = filter_table(flo,songlist=a20p)[i,6]
      erg = input$mmod
      mods = c(.25,.5,.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.5,5,5.5,6,6.5,7,7.5,8)
      rxmod = mods[sum(cmaxbpm * mods <= erg)]
    }
    rxmod
  })
  
  output$calcxmod = reactive( {
   paste("Current Xmod is", rxmod())
  })
  
  observeEvent(input$chart_viz, {
    output$chartplot = renderPlot({
      patplot_shiny(input$chart_to_viz,search_res(),filterlist(), allcharts = allcharts, smod = rxmod())
#    }, width = "auto", height = "auto", res=144)
    }, height = function() .6 * session$clientData$output_chartplot_width)
  })
  
}
#shiny::devmode(TRUE)
shinyApp(ui, server)