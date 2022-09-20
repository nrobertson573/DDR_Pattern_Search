library(shiny)
library(DT)
library(scales)

source("funcs_for_app.txt")
a20p = read.table("song_info.txt", header=TRUE)
allcharts = readRDS("all_steps.RData")

mixes = c("1stMIX","2ndMIX","3rdMIX","4thMIX","5thMIX","6thMIX MAX","7thMIX MAX2",
  "8thMIX EXTREME","SuperNOVA","SuperNOVA2","X","X2","X3","2013","2014","A","A20","A20 Plus","A3",
  "Grand Prix","Pad Purchase","Removed")
clevels = seq(1,19,1)
difficulties = c("Beginner","Basic","Difficult","Expert","Challenge")

ui <- fluidPage(
  titlePanel("Pattern Search Over Dance Dance Revoultion A3 Charts"),
  
  uiOutput("mygithub"),
  h6("Updated: 2022/8/15", style="color:black"),
  headerPanel(""),
  
  tabsetPanel(
    tabPanel("Filter",
      h3("Create a filter to generate a list of charts of interest", style="color:black"),
      fluidRow(
        column(3,
          "Select Level Range",
          numericInput("clevelmin","Minimum Level",value=10,min=1,max=19,step=1),
          numericInput("clevelmax","Maximum Level",value=15,min=1,max=19,step=1),
          "Specify BPM Parameters",
          numericInput("bpmmin","Minimum BPM",value = 1, min = 1, max = 1050,step=1),
          numericInput("bpmmax","Maximum BPM",value = 1050, min = 1, max = 1050,step=1),
          checkboxInput("staticbpm", 'Require Static BPM?', FALSE),
        ),
        column(3,
          "Select Difficulties",
          checkboxGroupInput("cdiff","Difficulty",difficulties,selected=c("Expert","Challenge")),
        ),
        column(3,
          "Select Mixes",
          checkboxGroupInput("mix","Mix",mixes,selected=mixes[-c(20,21,22)]),
        ),
        column(3,
          "Event/Unlockables and Gold Cabinet",
          checkboxInput("event", 'Include event and unlockable songs?', TRUE),
          checkboxInput("gold", 'Include Gold exclusives?', TRUE),
          ),
      ),
      textOutput("print_psuedoFL"),
      textOutput("filterlist_validate"),
      actionButton("filter","Create Filter"),
      fluidRow(
        DTOutput("filterlist"),
      ),
      
      ################################################################################################
      ##trying to add the chart viz to bottom of filter panel
      conditionalPanel(
        condition = "input.filter > 0 & output.filterlist_validate == ''",
        actionButton("viztoggleFP", "Visualize charts without searching for a pattern?")
      ),
      conditionalPanel(
        condition = "input.viztoggleFP > 0",
        fluidRow(
          column(3,
           selectizeInput(inputId = "chart_to_vizFP", label = "Select a chart", multiple=FALSE, choices = NULL),
          ),
          column(3,
            radioButtons("xormFP", "How would you like to determine scrolling speed?",
              choices = c("Enter Xmod", "Specify Max Scroll Speed"), "Enter Xmod"),
            conditionalPanel(
              condition = "input.xormFP == 'Enter Xmod'",
              numericInput('xmodFP', "Enter Xmod", 3, min=.25,max=8,step=.25),
            ),
            conditionalPanel(
              condition = "input.xormFP == 'Specify Max Scroll Speed'",
              numericInput('mmodFP', "Specify Max Scroll Speed", 450, min=100,max=1000,step=1),
            ),
            textOutput("calcxmodFP")
          ),
          column(3,
            radioButtons("turnmodFP", "Apply turn mod?",
              choices = c("None","Left","Right","Mirror","Shuffle 1", "Shuffle 2", "Shuffle 3", "Shuffle 4",
                "Shuffle 5", "Shuffle 6", "Shuffle 7", "Shuffle 8"),
              "None"
            )
          ),
          column(3,
            actionButton("chart_vizFP","Generate Chart Visual")
          )
        ),
      ),      
      plotOutput("chartplotFP")
      ############################################################################################
    ),

    tabPanel("Pattern",
      h3("Create a pattern to search for over your chart list", style="color:black"),
      fluidRow(
        column(4,
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
          textOutput("print_psuedoPL"),
          textOutput("PL_validate"),
##with new checks in place I don't really need the button anymore          
##          actionButton("patplotgo","Create Pattern Object"),
          plotOutput("patplot",height="400px",width="200px")
        )
      ),

      ##considering adding some preset searches here
      actionButton("patdefault","Would you like to try a preset pattern?"),
      conditionalPanel(
        condition = "input.patdefault >= 1",
        fluidRow(
          column(2,
            "8th Note Basic Crossovers",
            verticalLayout(
              actionButton("cross1","L D R D L"),
              actionButton("cross2","L U R U L"),
              actionButton("cross3","R D L D R"),
              actionButton("cross4","R U L U R")
            ),
          ),
          column(2,
            "8th Note Lateral [Afrowalk/Scoobies] Crossovers",
            verticalLayout(
              actionButton("lat1","L U R L D R"),
              actionButton("lat2","L D R L U R"),
              actionButton("lat3","R U L R D L"),
              actionButton("lat4","R D L R U L")
            ),
          ),
          column(2,
            "8th Note Jacks",
            verticalLayout(
              actionButton("jack1","L L L L L"),
              actionButton("jack2","D D D D D"),
              actionButton("jack3","U U U U U"),
              actionButton("jack4","R R R R R")
            ),
          ),
          column(2,
            "4 Panel Spins",
            verticalLayout(
              actionButton("spin1","L D R U L"),
              actionButton("spin2","L U R D L"),
              actionButton("spin3","R D L U R"),
              actionButton("spin4","R U L D R")
            ),
          ),
          column(2,
            "Step Jumps",
            verticalLayout(
              actionButton("sj4s","4ths Short"),
              actionButton("sj4l","4ths Long"),
              actionButton("sj8s","8ths Short"),
              actionButton("sj8l","8ths Long")
            ),
          ),
        ),
      ),
    ),


    tabPanel("Search",
      h3("Search for your pattern over your chart list", style="color:black"),
      
      fluidRow(
        column(4,
          textOutput("print_psuedoSearch"),
          textOutput("search_validate"),
        ),
        column(4,
          actionButton("search_start","Run Search, Please allow up to 30 seconds when searching over all charts"),
        ),
      ),
      
      DTOutput("search_tab"),
      
      
      ##trying to add the chart viz to bottom of search panel
      conditionalPanel(
        condition = "input.search_start > 0 & output.search_validate == ''",
        actionButton("viztoggleSP", "Visualize patterns in charts?")
      ),
      conditionalPanel(
        condition = "input.viztoggleSP > 0",
      
        fluidRow(
          column(3,
            selectizeInput(inputId = "chart_to_viz", label = "Select a chart", multiple=FALSE, choices = NULL),
          ),
          column(3,
            radioButtons("xorm", "How would you like to determine scrolling speed?",
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
          column(3,
            radioButtons("turnmodSP", "Apply turn mod?",
              choices = c("None","Left","Right","Mirror","Shuffle 1", "Shuffle 2", "Shuffle 3", "Shuffle 4",
                "Shuffle 5", "Shuffle 6", "Shuffle 7", "Shuffle 8"),
              "None"
            )
          ),
          column(2,
            actionButton("chart_viz","Generate Chart Visual")
          )
        ),
      
        plotOutput("chartplot")
      ),
    ),
  )
)


server <- function(input, output, session) {

  ##generate link to readme
  url = a("https://github.com/nrobertson573/DDR_Pattern_Search#readme", 
          href="https://github.com/nrobertson573/DDR_Pattern_Search#readme:")
  output$mygithub = renderUI({
    tagList("Github Readme link:", url)     
  })

  ##now reactive instead of event reactive due to other controls
  filterlist = reactive( {
    filter.chartlist(songlist=a20p,diff=input$cdiff,level=c(input$clevelmin:input$clevelmax),
      mix=input$mix,bpm_static=input$staticbpm,bpm_min=input$bpmmin,bpm_max=input$bpmmax,
      event=input$event,gold=input$gold)
  })

  ##indicator of whether filterlist passed. trycatch lets me return a yes/no instead of error
  psuedoFL = reactive({
    mytc = tryCatch(
      expr = {
        if(all(
          input$clevelmin <= input$clevelmax,
          input$clevelmin%%1 == 0,
          input$clevelmax%%1 == 0,
          input$bpmmin <= input$bpmmax,
          input$bpmmin >= 1,
          input$bpmmax <= 1050,
          length(input$cdiff) > 0,
          length(input$mix) > 0,
          length(filterlist())>0
        ) == TRUE){"YES"}else{"NO"}	
      },
      error = function(cnd){
        "NO"
      }
    )
    mytc
  })

  ##provide feedback to user if list is valid  
  output$print_psuedoFL = renderText({
    paste("Filterlist Valid: ", psuedoFL(), sep="")
  })

  ##returns validate errors without having to click a button
  output$filterlist_validate = renderText({
    validate(
      need(input$clevelmin <= input$clevelmax, "Minimum Level must be less than Maximum Level"),
      need(input$clevelmin%%1 == 0, "Must have Minimum Level between 1 and 19"),
      need(input$clevelmax%%1 == 0, "Must have Maximum Level between 1 and 19"),
      need(input$bpmmin <= input$bpmmax, "Minimum BPM must be less than Maximum BPM"),
      need(input$bpmmin & input$bpmmin >= 1, "Must have Minimum BPM > 1"),
      need(input$bpmmax & input$bpmmax <= 1050, "Must have Maximum BPM < 1050"),
      need(input$cdiff, "Please select at least one Difficulty"),
      need(input$mix, "Please select at least one Mix"),
      need(length(filterlist())> 0, "Current selections include 0 charts")
    )
    ""
  })

  ##generate filterlist table
  observeEvent(input$filter,{
    output$filterlist = renderDT({
      validate(
        need(input$clevelmin <= input$clevelmax, "Minimum Level must be less than Maximum Level"),
        need(input$clevelmin%%1 == 0, "Must have Minimum Level between 1 and 19"),
        need(input$clevelmax%%1 == 0, "Must have Maximum Level between 1 and 19"),
        need(input$bpmmin <= input$bpmmax, "Minimum BPM must be less than Maximum BPM"),
        need(input$bpmmin & input$bpmmin >= 1, "Must have Minimum BPM > 1"),
        need(input$bpmmax & input$bpmmax <= 1050, "Must have Maximum BPM < 1050"),
        need(input$cdiff, "Please select at least one Difficulty"),
        need(input$mix, "Please select at least one Mix"),
        need(length(filterlist())> 0, "Current selections include 0 charts")
      )
      datatable(filter_table(filterlist(),songlist=a20p))
    })
  })
  ############################################################################################
  ##start new visual gen under filterlist 
  observeEvent(input$filter, {
    updateSelectizeInput(session, inputId = "chart_to_vizFP", choices = sort(filterlist()))
  })
  
  rxmodFP = reactive( {
    if(input$xormFP == "Enter Xmod"){ rxmodFP = input$xmodFP }
    if(input$xormFP == "Specify Max Scroll Speed"){
      flo = filterlist()
      flt = filter_table(filterlist(),songlist=a20p)
      i = which(flo == input$chart_to_vizFP)
      cmaxbpm = filter_table(flo,songlist=a20p)[i,6]
      erg = input$mmodFP
      mods = c(.25,.5,.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.5,5,5.5,6,6.5,7,7.5,8)
      rxmodFP = mods[sum(cmaxbpm * mods <= erg)]
    }
    rxmodFP
  })
  
  output$calcxmodFP = reactive( {
    paste("Current Xmod is", rxmodFP())
  })
  
  observeEvent(input$chart_vizFP, {
    output$chartplotFP = renderPlot({
      chart_smdat = allcharts[[which(names(allcharts) == input$chart_to_vizFP)]]
      rasterplot(chart_smdat,maxcolnum = 30, speedmod = rxmodFP(), main = input$chart_to_vizFP,
                sub = paste("Speed mod = ", rxmodFP()), turn = input$turnmodFP)
      #    }, width = "auto", height = "auto", res=144)
    }, height = function() .6 * session$clientData$output_chartplotFP_width)
  })
  ###########################################################################################  
  
  
  ##Pattern Panel Code
  PL = reactive({
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
  
  ##indicator of whether PL passed. trycatch lets me return a yes/no instead of error
  psuedoPL = reactive({
    mytc = tryCatch(
      expr = {
        if(input$patterntype == "Sequence Only"){
          arrow_p = APstr_to_vec(input$po_ap)
          new_step = input_to_vec(input$po_ns)
          maxbeats = input$po_mb
          PLTF = all(
            length(arrow_p) > 0, 
            length(new_step) > 0, 
            length(arrow_p) == length(new_step),
            maxbeats > 0,
            all(arrow_p %in% c(1,2,3,4))
          )
        }
        if(input$patterntype == "Timing Only"){
          arrows_per_step = input_to_vec(input$to_aps)
          timing_between_steps = Tstr_to_vec(input$to_tbs)
          PLTF = all(
            length(arrows_per_step) > 0,
            length(timing_between_steps) > 0,
            length(arrows_per_step) == length(timing_between_steps),
            max(arrows_per_step) <= 2,
            sum(arrows_per_step)>0,
            all(timing_between_steps %in% c(0,4,6,8,12,16,24,32,48,64))
          )
        }
        if(input$patterntype == "Sequence and Timing"){
          arrow_p = APstr_to_vec(input$pt_ap)
          timing_between_steps = Tstr_to_vec(input$pt_tbs)
          PLTF = all(
            length(arrow_p) > 0,
            length(timing_between_steps) > 0,
            length(arrow_p) == length(timing_between_steps),
            all(timing_between_steps %in% c(0,4,6,8,12,16,24,32,48,64)),
            all(arrow_p %in% c(1,2,3,4))
          )
        }
        if(PLTF == TRUE){"YES"}else{"NO"}
      },
      error = function(cnd){
        "NO"
      }
    )
    mytc
  })
  
  ##provide feedback to user if PL is valid  
  output$print_psuedoPL = renderText({
    paste("Pattern Valid: ", psuedoPL(), sep="")
  })
  
  ##returns validate errors without having to click a button
  output$PL_validate = renderText({
    PLO = PL()
    if(input$patterntype == "Sequence Only"){
      validate(
        need(
          length(PLO$arrow_p) > 0 & 
            length(PLO$new_step) > 0 & 
            length(PLO$arrow_p) == length(PLO$new_step), 
          "Arrow Sequence and New Step? must have same number of elements"
        ),
        need(PLO$maxbeats > 0, "Maximum Beats must be positive"),
        need(all(PLO$arrow_p %in% c(1,2,3,4)), 
             "Arrow Sequence requires elements of (L,D,U,R,l,d,u,r,1,2,3,4) separated by space or comma as inputs"
        )
      )
    }
    if(input$patterntype == "Timing Only"){
      validate(
        need(
          length(PLO$arrows_per_step) > 0 &
            length(PLO$timing_between_steps) > 0 &
            length(PLO$arrows_per_step) == length(PLO$timing_between_steps),
          "Arrows Per Step and Timing Between Steps must have same number of elements"
        ),
        need(max(PLO$arrows_per_step) <= 2, "DDR charts do not allow 3 arrows at once. Please ignore Uh La La La. Also Megalovania doesn't count since I haven't added freeze arrow functionality."),
        need(sum(PLO$arrows_per_step)>0, "Must have arrows to make a pattern"),
        need(all(PLO$timing_between_steps %in% c(0,4,6,8,12,16,24,32,48,64)),
             "Unsupported note type in Timing Between Steps")
      )
    }
    if(input$patterntype == "Sequence and Timing"){
      validate(
        need(
          length(PLO$arrow_p) > 0 &
            length(PLO$timing_between_steps) > 0 &
            length(PLO$arrow_p) == length(PLO$timing_between_steps),
          "Arrow Sequence and Timing Between Steps must have same number of elements"
        ),
        need(all(PLO$timing_between_steps %in% c(0,4,6,8,12,16,24,32,48,64)),
             "Unsupported note type in Timing Between Steps"),
        need(all(PLO$arrow_p %in% c(1,2,3,4)), 
             "Arrow Sequence requires elements of (L,D,U,R,l,d,u,r,1,2,3,4) separated by space or comma as inputs"
        )
      )
    }
    ""
  })
  
  
  patplotER = eventReactive((psuedoPL() == "YES"), {
      patplot_rs(PL(),input$patterntype)
  }, ignoreNULL = FALSE)

  output$patplot = renderPlot({
    validate(need(psuedoPL() == "YES", ""))
    patplotER()
  })

  ##preset patterns
  ##basic cross
  observeEvent(input$cross1, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L D R D L")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
      value = "0 8 8 8 8")
  })
  observeEvent(input$cross2, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L U R U L")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })  
  observeEvent(input$cross3, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R D L D R")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })  
  observeEvent(input$cross4, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R U L U R")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })  
  ##lateral cross
  observeEvent(input$lat1, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L U R L D R")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8 8")
  })
  observeEvent(input$lat2, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L D R L U R")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8 8")
  })  
  observeEvent(input$lat3, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R U L R D L")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8 8")
  })  
  observeEvent(input$lat4, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R D L R U L")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8 8")
  })  
  ##5 note jack
  observeEvent(input$jack1, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L L L L L")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })
  observeEvent(input$jack2, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "D D D D D")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })  
  observeEvent(input$jack3, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "U U U U U")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })  
  observeEvent(input$jack4, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence and Timing")
    updateTextInput(session, inputId = "pt_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R R R R R")
    updateTextInput(session, inputId = "pt_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
                    value = "0 8 8 8 8")
  })    
  ##4 panel spin
  observeEvent(input$spin1, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence Only")
    updateTextInput(session, inputId = "po_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L D R U L")
    updateTextInput(session, inputId = "po_ns", label = "New Step? [1:yes, 0:no] (e.g. 1 0 1 1 0 1 1 0)",
                    value = "1 1 1 1 1")
    updateNumericInput(session, inputId = "po_mb", label =  "Maximum Beats Pattern Must Occur Within", value = 2)
  })
  observeEvent(input$spin2, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence Only")
    updateTextInput(session, inputId = "po_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "L U R D L")
    updateTextInput(session, inputId = "po_ns", label = "New Step? [1:yes, 0:no] (e.g. 1 0 1 1 0 1 1 0)",
                    value = "1 1 1 1 1")
    updateNumericInput(session, inputId = "po_mb", label =  "Maximum Beats Pattern Must Occur Within", value = 2)
  })  
  observeEvent(input$spin3, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence Only")
    updateTextInput(session, inputId = "po_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R D L U R")
    updateTextInput(session, inputId = "po_ns", label = "New Step? [1:yes, 0:no] (e.g. 1 0 1 1 0 1 1 0)",
                    value = "1 1 1 1 1")
    updateNumericInput(session, inputId = "po_mb", label =  "Maximum Beats Pattern Must Occur Within", value = 2)
  })  
  observeEvent(input$spin4, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Sequence Only")
    updateTextInput(session, inputId = "po_ap", label = "Arrow Pattern (e.g. L U R L D R)", value = "R U L D R")
    updateTextInput(session, inputId = "po_ns", label = "New Step? [1:yes, 0:no] (e.g. 1 0 1 1 0 1 1 0)",
                    value = "1 1 1 1 1")
    updateNumericInput(session, inputId = "po_mb", label =  "Maximum Beats Pattern Must Occur Within", value = 2)
  })
  ##stepjumps
  observeEvent(input$sj4s, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Timing Only")
    updateTextInput(session, inputId = "to_aps", label = "Arrows Per Step [2:jump, 1:tap] (e.g. 2 1 2 1 2)",
      value = "2 1 2 1 2")
    updateTextInput(session, inputId = "to_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
      value = "0 4 4 4 4")
  })
  observeEvent(input$sj4l, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Timing Only")
    updateTextInput(session, inputId = "to_aps", label = "Arrows Per Step [2:jump, 1:tap] (e.g. 2 1 2 1 2)",
      value = "2 1 2 1 2 1 2 1 2")
    updateTextInput(session, inputId = "to_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
      value = "0 4 4 4 4 4 4 4 4")
  })
  observeEvent(input$sj8s, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Timing Only")
    updateTextInput(session, inputId = "to_aps", label = "Arrows Per Step [2:jump, 1:tap] (e.g. 2 1 2 1 2)",
      value = "2 1 2 1 2")
    updateTextInput(session, inputId = "to_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
      value = "0 8 8 8 8")
  })
  observeEvent(input$sj8l, {
    updateRadioButtons(session, inputId = "patterntype", choices = c("Sequence Only","Timing Only",
      "Sequence and Timing"),("Sequence Only"), selected = "Timing Only")
    updateTextInput(session, inputId = "to_aps", label = "Arrows Per Step [2:jump, 1:tap] (e.g. 2 1 2 1 2)",
      value = "2 1 2 1 2 1 2 1 2")
    updateTextInput(session, inputId = "to_tbs", label = "Timing Between Steps (e.g. 0 8 16 16 4)",
      value = "0 8 8 8 8 8 8 8 8")
  })


  ################################################################################
  ##Search Panel
  psuedoSearch = reactive({
    mytc = tryCatch(
      expr = {
        PFL = psuedoFL()
        PPL = psuedoPL()
        if((PFL == "YES") & (PPL == "YES")){"YES"}else{"NO"}
      },
      error = function(cnd){
        "NO"
      }
    )
    mytc
  })
  
  ##provide feedback to user if list is valid  
  output$print_psuedoSearch = renderText({
    paste("Ready to Search?: ", psuedoSearch(), sep="")
  })
  
  ##returns validate errors without having to click a button
  output$search_validate = renderText({
    validate(
      need(psuedoFL() == "YES", "Issue with filter"),
      need(psuedoPL() == "YES", "Issue with pattern")
    )
    ""
  })
  
  
  search_res = eventReactive(input$search_start,{
    PS = psuedoSearch()
    validate(
      need(PS == "YES", "Search not run"),
    )
    showModal(modalDialog("Searching..."))
    srobj = search_run(filterlist(),PL(),allcharts)
    removeModal()
    srobj
  })

  output$search_tab = renderDT({
    datatable(search_tab_gen(search_res(),a20p), options = list(order=list(8, 'desc'))) %>%
      formatStyle("pat_count", color = rgb(42, 36, 82, maxColorValue = 255), backgroundColor = rgb(142, 143, 148, alpha = 50, maxColorValue = 255))
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
      patplot_shiny(input$chart_to_viz,search_res(),filterlist(), allcharts = allcharts, smod = rxmod(),
        turn = input$turnmodSP)
#    }, width = "auto", height = "auto", res=144)
    }, height = function() .6 * session$clientData$output_chartplot_width)
  })
  
}
##shiny::devmode(TRUE)
shinyApp(ui, server)