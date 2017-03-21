

dashboardPage(
  dashboardHeader(title = titleWarning("CV Shiny (v0.16)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask"),
               menuSubItem('All', tabName = 'drugdata_all'),
               menuSubItem('Concomitant', tabName = 'drugdata_con'),
               menuSubItem('Suspect', tabName = 'drugdata_sus'),
               menuSubItem('Drugs per Report', tabName = 'drugdata_number')),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE),
      menuItem("Download", tabName = "download_tab", icon = icon("download"))
    ),
    conditionalPanel(
      condition = "input.name_type == 'brand'",
      selectizeInput("search_brand",
                     "Brand Name (Canadian Trade Name)",
                     c("Start typing to search..." = ""),
                     multiple = TRUE)),
    conditionalPanel(
      condition = "input.name_type == 'ingredient2'",
      selectizeInput("search_ing2", 
                     "Active Ingredient",
                     c(topings_dpd, "Start typing to search..." = ""))),
    conditionalPanel(
      condition = "input.name_type == 'ingredient'",
      selectizeInput("search_ing", 
                     "Active Ingredient",
                     c("Start typing to search..." = ""),
                     multiple = TRUE)),
    div(style="display: inline-block; width: 47%;",
        radioButtons("name_type", "Drug name type:",
                     c("Brand Name" = "brand",
                       # "DPD ing" = "ingredient2",
                       "Active Ingredient" = "ingredient"))),
    div(style="display: inline-block; vertical-align:top; width: 52%",
        radioButtons("drug_inv", "Drug Involvement:",
                     c("Suspect",
                       "Concomitant",
                       "Any"))),
    sliderInput("search_age",
                "Set Age Range",
                min = 0,
                max = 100,
                value = c(0,100)),
    conditionalPanel(
      condition = "input.search_age[1] == 100",
      checkboxInput("filter_over_100",
                    "Include results over 100?",
                    value = TRUE)),
    selectInput("search_gender",
                "Select Gender",
                c("All",
                  "Male",
                  "Female")),
    selectizeInput("search_rxn", 
                   "Preferred Term (PT)",
                   c("Start typing to search..." = ""),
                   multiple = TRUE),
    selectizeInput("search_soc",
                   "System Organ Class (SOC)",
                   c("Start typing to search..." = ""),
                   multiple = TRUE), 
    fluidRow(
      column(6,
        selectizeInput("searchStartYear",
                       label = "Start Year",
                       choices = c(1965:2016),
                       selected = 2000)),
      column(6,
        selectizeInput("searchStartMonth",
                     label = "Start Month",
                     choices = c(1:12),
                     selected = 1))),
    fluidRow(
      column(6,
             selectizeInput("searchEndYear",
                            label = "End Year",
                            choices = c(1965:2016),
                            selected = 2016)),
      column(6,
             selectizeInput("searchEndMonth",
                            label = "End Month",
                            choices = c(1:12),
                            selected = 6))),
      # hacky way to get borders correct
    conditionalPanel(
      condition = "input.search_rxn != 'disable'",
      tags$div(
        class="form-group shiny-input-container",
        actionButton("searchButton",
                     "Search",
                     width = '100%')
      ))), 
  
  dashboardBody(
    customCSS(),
    fluidRow(
      box(htmlOutput(outputId = "timeplot_title"),
          #htmlOutput(outputId = "timeplot"),
          lineChartOutput("mychart"),
          "Reports by month from Canada Vigilance Adverse Reaction Online Database.",
          width = 12
      )
    ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                tabBox(
                  tabPanel("Pie Chart",
                           h3("Reporter Type",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "Indicates who reported the adverse reaction and their relationship to the patient. ",
                                  "Slices may not be visible if they are too small.")
                              )),
                           htmlOutput("reporterplot"),
                           width = 3),
                  tabPanel("Table",
                           h3("Reporter Type",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "Indicates who reported the adverse reaction and their relationship to the patient. ",
                                  "Slices may not be visible if they are too small.")
                              )),
                           htmlOutput("reporterplot.table"),
                           width = 3),
                  width = 3),
                tabBox(
                  tabPanel("Pie Chart",
                           h3("Seriousness",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "A serious report contains a serious adverse reaction, determined by the reporter ",
                                  "of the report at the time of reporting. Slices may not be visible if they are too small.")
                              )),
                           htmlOutput("seriousplot"),
                           width = 3),
                  tabPanel("Table",
                           h3("Seriousness",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "A serious report contains a serious adverse reaction, determined by the reporter ",
                                  "of the report at the time of reporting. Slices may not be visible if they are too small.")
                              )),
                           htmlOutput("seriousplot.table"),
                           width = 3),
                  width = 3),
                box(h3("Reason(s) for Seriousness",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0("The serious condition which the adverse event resulted in. Total may sum to",
                                        " more than the total number of reports because reports can be marked serious for multiple reasons"))),
                    htmlOutput("seriousreasonsplot"),
                    width = 5)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                tabBox(
                  tabPanel("Pie Chart",
                           h3("Gender",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "Gender of the patient as it was provided by the reporter. ",
                                  "Where the gender is unknown, the reporter is unaware of the gender. ",
                                  "Where the gender is not specified, the reporter did not specify the gender of the patient."))),
                           htmlOutput("sexplot"),
                           width = 3),
                  tabPanel("Table",
                           h3("Gender",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "Gender of the patient as it was provided by the reporter. ",
                                  "Where the gender is unknown, the reporter is unaware of the gender. ",
                                  "Where the gender is not specified, the reporter did not specify the gender of the patient."))),
                           htmlOutput("sexplot.table"),
                           width = 3)
                ),
                tabBox(
                  tabPanel("Pie Chart",
                           h3("Age Group",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title =  HTML(paste0(
                                  "Age group of the patient when the adverse effect occurred.<br>",
                                  "<br>Neonate: <= 25 days",
                                  "<br>Infant: > 25 days to < 1 yr",
                                  "<br>Child: >= 1 yr to < 13 yrs",
                                  "<br>Adolescent: >= 13 yrs to < 18 yrs",
                                  "<br>Adult: >= 18 yrs to <= 65 yrs",
                                  "<br>Elderly: > 65 yrs")))),
                           htmlOutput("agegroupplot"),
                           width = 3),
                  tabPanel("Table",
                           h3("Age Group",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title =  HTML(paste0(
                                  "Age group of the patient when the adverse effect occurred.<br>",
                                  "<br>Neonate: <= 25 days",
                                  "<br>Infant: > 25 days to < 1 yr",
                                  "<br>Child: >= 1 yr to < 13 yrs",
                                  "<br>Adolescent: >= 13 yrs to < 18 yrs",
                                  "<br>Adult: >= 18 yrs to <= 65 yrs",
                                  "<br>Elderly: > 65 yrs")))),
                           htmlOutput("agegroupplot.table"),
                           width = 3)
                ),
                box(htmlOutput("agehisttitle"),
                    plotlyOutput("agehist"),
                    width = 6)
              )
      ),
      tabItem(tabName = "drugdata_all",
              fluidRow(
                tabBox(
                  tabPanel("All: Bar Graph",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("all_drugs")),
                  tabPanel("All: Data Table",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("all_drugs.table")),
                  width = 6),
                tabBox(
                  tabPanel("Bar Chart",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot")),
                  tabPanel("Table",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot.table")),
                  width = 6))),
      tabItem(tabName = 'drugdata_con',
              fluidRow(
                tabBox(
                  tabPanel("Concomitant: Bar Graph",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The health product is not suspected, but the patient was taking it at the time of the adverse reaction."))),
                           htmlOutput("concomitant_drugs")),
                  tabPanel("Concomitant: Data Table",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The health product is not suspected, but the patient was taking it at the time of the adverse reaction."))),
                           htmlOutput("concomitant_drugs.table"))),
                tabBox(
                  tabPanel("Bar Chart",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot.con")),
                  tabPanel("Table",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot.table.con")),
                  width = 6))),
      tabItem(tabName = 'drugdata_sus',
              fluidRow(
                tabBox(
                  tabPanel("Suspect: Bar Graph",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The reporter suspects that the health product caused the adverse reaction."))),
                           htmlOutput("suspect_drugs")),
                  tabPanel("Suspect: Data Table",
                           h3("Most Frequently Reported Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The reporter suspects that the health product caused the adverse reaction."))),
                           htmlOutput("suspect_drugs.table"))),
                tabBox(
                  tabPanel("Bar Chart",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot.sus")),
                  tabPanel("Table",
                           h3("Reports per Indication (all reported drugs)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste(
                                  "Indication refers to the particular condition for which a health product was taken. ",
                                  "This plot includes all indications for all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("indication_plot.table.sus")),
                  width = 6))),
      tabItem(tabName = 'drugdata_number',
              fluidRow(
                box(htmlOutput("drugcounttitle"),
                    htmlOutput("drugcount_plot"),
                    width = 12))),
      tabItem(tabName = "rxndata",
              fluidRow(
                tabBox(
                  tabPanel("Bar Chart",
                           h3("Most Frequent Adverse Events (Preferred Terms)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "MedDRA Preferred Term is a distinct descriptor (single medical concept) for a symptom, ",
                                  "sign, disease, diagnosis, therapeutic indication, investigation, surgical, or medical ",
                                  "procedure, and medical, social, or family history characteristic. For more rigorous analysis, ",
                                  "use disproportionality statistics."))),
                           htmlOutput("top_pt")),
                  tabPanel("Data Table",
                           h3("Most Frequent Adverse Events (Preferred Terms)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "MedDRA Preferred Term is a distinct descriptor (single medical concept) for a symptom, ",
                                  "sign, disease, diagnosis, therapeutic indication, investigation, surgical, or medical ",
                                  "procedure, and medical, social, or family history characteristic. For more rigorous analysis, ",
                                  "use disproportionality statistics."))),
                           htmlOutput("top_pt.table")),
                  width = 6),
                tabBox(
                  tabPanel("Bar Chart",
                           h3("Most Frequent Adverse Events (High-Level Terms)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = "For more rigorous analysis, use disproportionality statistics.")),
                           htmlOutput("top_hlt")),
                  tabPanel("Data Table",
                           h3("Most Frequent Adverse Events (High-Level Terms)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = "For more rigorous analysis, use disproportionality statistics.")),
                           htmlOutput("top_hlt.table")),
                  width = 6)),
              fluidRow(
                tabBox(
                  tabPanel("Pie Chart",
                           h3("Report Outcome",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "The report outcome represents the outcome of the reported case as described by the reporter ",
                                  "at the time of reporting and does not infer a causal relationship. The report outcome is not ",
                                  "based on a scientific evaluation by Health Canada."))),
                           htmlOutput("outcomeplot")),
                  tabPanel("Data Table",
                           h3("Report Outcome",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "The report outcome represents the outcome of the reported case as described by the reporter ",
                                  "at the time of reporting and does not infer a causal relationship. The report outcome is not ",
                                  "based on a scientific evaluation by Health Canada."))),
                           htmlOutput("outcomeplot.table")),
                  width = 4))),
      
      tabItem(tabName = "aboutinfo",
              box(
                width = 12,
                h2("About"),
                # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
                HTML(paste0(
                  "<p>",
                  "This is a beta product. DO NOT use as sole evidence to support regulatory decisions or to make decisions regarding ",
                  "medical care. Always speak to your health care provider about the risks and benefits of Health Canada regulated Products.",
                  "</p>",
                  "<p>",
                  "This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. ",
                  "This is a prototype experiment that utilizes publically available data (Canada Vigilance Adverse Reaction Online Database) ", 
                  "and provide visualizations in an interactive format. Health Canada collects and maintains a high volume of adverse event ", 
                  "reports associated with different drugs and products. This app allows users to effortlessly interact with the reports ", 
                  "database, conduct searches and view results in highly interactive dashboards. To support innovation, coordination and ", 
                  "to support Canadians, this interface permits the users to export search results (with no limitation to the number of rows) ", 
                  "in various file formats such as CSV and Excel for further exploration and experimentation.",
                  "</p>",
                  "<br>",
                  "<p>",
                  "<strong>Data last updated: 2016-06-30</strong><br>",
                  "<strong>MedDRA Version: 19.0</strong><br>",
                  "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
                  "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
                  "For more information, please refer to ",
                  "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
                  "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php</a>.",
                  "</p>")),
                aboutAuthors())), 
      tabItem(tabName = "download_tab",
              fluidRow(
                box(
                  column(
                    width = 3,
                    div(style="display: inline-block; width: 161px;",
                        selectInput("search_dataset_type",
                                    "Download Type",
                                    c("Report Data", "Drug Data", "Reaction Data"))),
                    div(style="display: inline-block; vertical-align: bottom; height: 54px;",
                        downloadButton(outputId = 'download_reports',
                                       label = 'Download')),
                    conditionalPanel(
                      "input.search_dataset_type == 'Report Data'",
                      selectizeInput("column_select_report",
                                     "Select Columns",
                                     cv_reports_names,
                                     selected = c("REPORT_ID", "DATRECEIVED", "REPORT_TYPE_ENG", "GENDER_ENG", "AGE_Y", "OUTCOME_ENG", "WEIGHT", "WEIGHT_UNIT_ENG",
                                                  "HEIGHT", "HEIGHT_UNIT_ENG", "SERIOUSNESS_ENG", "REPORTER_TYPE_ENG", "SOURCE_ENG"),
                                     multiple = TRUE)),
                    conditionalPanel(
                      "input.search_dataset_type == 'Drug Data'",
                      selectizeInput("column_select_drug",
                                     "Select Columns",
                                     cv_report_drug_names,
                                     selected = c("REPORT_ID", "DRUGNAME", "DRUGINVOLV_ENG", "ROUTEADMIN_ENG", "UNIT_DOSE_QTY", "DOSE_UNIT_ENG", "FREQUENCY", "FREQ_TIME",
                                                  "FREQUENCY_TIME_ENG", "FREQ_TIME_UNIT_ENG", "DOSAGEFORM_ENG", "INDICATION_NAME_ENG"),
                                     multiple = TRUE)),
                    conditionalPanel(
                      "input.search_dataset_type == 'Reaction Data'",
                      selectizeInput("column_select_reaction",
                                     "Select Columns",
                                     cv_reaction_names,
                                     selected = c("REPORT_ID", "DURATION", "DURATION_UNIT_ENG", "PT_NAME_ENG", "SOC_NAME_ENG", "MEDDRA_VERSION", "SMQ"),
                                     multiple = TRUE))),
                  column(
                    width = 9,
                    tableOutput("current_search")
                  ),
                  width = 12)
              )
      )
    )
  )
)

