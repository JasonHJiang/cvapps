titleWarning <- function(title) {
  list(title, span(
  "WARNING: This is a beta product. DO NOT use", br(),
  "as sole evidence to support regulatory decisions."))
}

customCSS <- function() {
  tags$head(tags$style(HTML('
.main-header .logo span {
  display: inline-block;
  line-height: normal;
  vertical-align: middle;
  font-size: smaller;
  padding-left: inherit;
}

/*
Text colour is greyed out
http://stackoverflow.com/questions/36314780/shinydashboard-grayed-out-downloadbutton
*/
.skin-blue .sidebar .btn {
  color: #444;
}
')))
}

aboutAuthors <- function() {list(
  tags$strong("Authors:"),
  fluidRow(
    box(
      "Daniel Buijs, MSc", br(),
      "Data Scientist, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "daniel.buijs@hc-sc.gc.ca",
      width = 3
    ),
    box(
      "Sophia He, BSc (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "sophia.he@canada.ca or yunqingh@sfu.ca",
      width = 3
    ),
    box(
      "Kevin Thai, BSc (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "kevin.thai@canada.ca or kthai@uwaterloo.ca",
      width = 3
    )
  )
)}