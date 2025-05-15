library(shiny)
library(rmarkdown)

source("dumbellplot.R")  # zakładając, że definiuje ui i server


ui <- navbarPage(
  "Dashboard giełda",
  tabPanel("Wprowadzenie", 
           fluidPage(
             # Osadzenie stylów CSS
             tags$style(HTML("
             .navbar {
                background-color: #2c3e50 !important;
                border-color: #2c3e50;
                position: fixed !important; /* Ustawia NavBar jako stały */
                top: 0;
                width: 100%;
                z-index: 1000; /* Zapewnia, że NavBar jest ponad zawartością */
              }
              .navbar .navbar-brand, .navbar .navbar-nav > li > a {
                color: white !important;
              }
              .navbar .navbar-nav > .active > a, 
              .navbar .navbar-nav > .active > a:hover, 
              .navbar .navbar-nav > .active > a:focus {
                background-color: #3498db !important;
                color: white !important;
              }
              .navbar .navbar-nav > li > a:hover {
                background-color: #34495e !important;
                color: white !important;
              }
              body, .fluidPage {
                padding-top: 70px !important; /* Margines u góry, aby uniknąć nakładania */
              }
              .boxed-section {
                border: 2px solid #ccc;
                border-radius: 10px;
                padding: 20px;
                margin-bottom: 30px;
                background-color: #f9f9f9;
                box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
              }
              .section-title {
                color: #2c3e50;
                font-size: 1.5em;
                margin-bottom: 15px;
                border-bottom: 2px solid #3498db;
                padding-bottom: 5px;
              }
              .sub-section {
                margin-left: 20px;
              }
              table {
                width: 100%;
                border-collapse: collapse;
                margin-bottom: 20px;
                font-family: Arial, sans-serif;
                font-size: 14px;
              }
              
              table th, table td {
                border: 1px solid #ccc;
                padding: 8px 12px;
                text-align: center;
              }
              
              table th {
                background-color: #3498db;
                color: white;
                font-weight: bold;
              }
              
              table tr:nth-child(even) {
                background-color: #f2f2f2;
              }
              
              table tr:hover {
                background-color: #d6e9f8;
              }
              ")),
             # Wczytanie treści Markdown z pliku dashboard.md
             includeMarkdown("dashboard.md")
           )
           ),
  tabPanel("Dashboard", ui)  # ui z dumbellplot.R
)

shinyApp(ui = ui, server = server)
