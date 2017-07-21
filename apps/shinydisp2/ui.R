# data manip + utils
################################## UI component ####
ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.18)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      selectizeInput(inputId ="search_drug",
                     label = "Generic Name/Ingredient",
                     choices = c(drug_choices, "Start typing to search..." = ""),
                     multiple = TRUE,
                     selected = drug_choices[1]),
      selectizeInput(inputId = "search_hlt",
                     label = "Adverse Event High-Level Term",
                     choices = c("Start typing to search..." = ""),
                     multiple = TRUE),
      selectizeInput(inputId = "search_pt",
                     label = "Adverse Event Preferred Term",
                     choices = c("Start typing to search..." = ""),
                     multiple = TRUE),
      checkboxInput(inputId = "checkbox_filter_pt",
                    label = "Only see PTs from chosen HLT",
                    value = FALSE),
      div(style="display: inline-block; width: 50%;",
          textInput(inputId = "min_count",
                    label = "Min. count:",
                    value = "0")),
      div(style="display: inline-block; width: 50%;",
          textInput(inputId = "min_exp",
                    label = "Min. expected:",
                    value = "0")),
      checkboxInput(inputId = "inf_filter",
                    label = "Exclude Inf PRR from table",
                    value = FALSE),
      conditionalPanel(
        "input.search_hlt == null",
        checkboxInput(inputId = "display_total_hlt",
                      label = "Plot Total Counts for HLT instead of Distinct Counts.",
                      value = FALSE)),
      conditionalPanel(
        "input.search_pt == null",
        checkboxInput(inputId = "display_total_pt",
                      label = "Plot Total Counts for PT instead of Distinct Counts.",
                      value = FALSE)),
      # hacky way to get borders correct
      tags$div(class="form-group shiny-input-container",
               actionButton(inputId = "search_button",
                            label = "Search",
                            width = '90%')
      ),
      tags$h3(strong("Current Query:")),
      tableOutput("current_search"),
      downloadButton(outputId = "pt_data_dl",
                     label = "Export PT data"),
      downloadButton(outputId = "hlt_data_dl",
                     label = "Export HLT data"),
      menuItem("Documentation", tabName = "Documentation", icon = icon("flag")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE)
    )
  ), 
  
  dashboardBody(
    customCSS(),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                tabBox(id="tabbox",
                  
                  tabPanel(
                    title="Preferred Terms",
                    value="panel1",
                    # plotOutput(outputId = "timeplot_pt", height = "285px", click = "plot_click"),
                    h3(textOutput("current_pt_title")),
                    ggvisOutput(plot_id = "timeplot_pt"),
                    tags$br(),
                    #div(style="display:inline-block",
                    #radioButtons(inputId = "table_selection",label=NULL,choices=c("All","PT seen before","PT not seen before"),inline=TRUE)),
                    #div(style="display:inline-block",tipify(el = icon("info-circle"), trigger = "hover click",
                    #  title ="PT not seen before = adverse event(s) not indicated in drug monographs")), 
                    DT::dataTableOutput("table_pt"),
                    #tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    width = 12),
                  tabPanel(
                    title="High-Level Terms",
                    value="panel2",
                    #plotOutput(outputId = "timeplot_hlt", height = "285px"),
                    h3(textOutput("current_hlt_title")),
                    ggvisOutput(plot_id = "timeplot_hlt"),
                    tags$br(),
                    DT::dataTableOutput("table_hlt"),
                    #tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    width = 12),
                  width = 12
                )
              )
      ),
      
      tabItem(tabName = "Documentation",
              fluidRow(
                box(
                  tabPanel(
                    "Documentation",
                    withMathJax(includeMarkdown("/home/shared/DISP data/CopyOfDISP about/DISP_about.md")),
                    tags$br(),
                    width = 12),
                  width=12
                )
              )
      ),
      
      tabItem(tabName = "aboutinfo", box(
        width = 12,
        h2("About"),
        # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
        HTML(paste0(
          "<p>",
          "This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. ",
          "This is a prototyping platform to utilize open data sources (Canada Vigilance Adverse Reaction Online Database) ",
          "to conduct disproportionality analysis for safety signal detection. It provides visualizations in an interactive ",
          "format to demonstrate the results of multiple disproportionality analysis.",
          "</p>",
          "<p>",
          "DO NOT USE FOR REGULATORY DECISION MAKING! The methods described and results generated from the work presented herein ",
          "relate solely to the testing of methodologies and representations for the evaluation of drugs and AERs. This report ",
          "neither replaces nor is intended to replace or comment on any regulatory decisions made by Health Canada.",
          "</p>",
          "<p>",
          "Detailed documentation on all disproportionality analyses can be found in Documentation tab.",
          "</p>",
          "<br>",
          "<p>",
          "<strong>Data last updated: 2015-03-31</strong><br>",
          "<strong>MedDRA version: 19.0</strong><br>",
          "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
          "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
          "For more information, please refer to ",
          "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
          "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php</a>.",
          "</p>")),
        aboutAuthors()
      ))
    )
  ), 
  
  skin = "blue"
)



