# Ładowanie wymaganych pakietów
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)


symbol_to_name <- data.frame(
  Symbol = c("TSLA", "NVDA", "GC=F", "EEM", "DX-Y.NYB", "DJT", "BTC-USD", "^IXIC", "^GSPC", "^VIX"), # przykładowe symbole
  Name = c("Tesla", "Nvidia", "Złoto", "Rynki wschodządze", "Siła dolara", "Spółka Donalda Trumpa", "Bitcoin", "NASDAQ", "SNP500", "Indeks strachu") # pełne nazwy
)

# Funkcja do obliczania procentowej zmiany
calculate_percent_change <- function(data, start_date, end_date) {
  # Filtrowanie danych dla wybranego zakresu dat
  filtered_data <- data %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date)) %>%
    group_by(Symbol) %>%
    summarise(
      Start_Close = first(Close),
      End_Close = last(Close),
      Percent_Change = ((End_Close - Start_Close) / Start_Close) * 100
    ) %>%
    filter(!is.na(Percent_Change)) # Usuwanie NA
  
  filtered_data <- filtered_data %>%
    left_join(symbol_to_name, by = "Symbol") %>%
    mutate(Symbol = ifelse(is.na(Name), Symbol, Name)) %>%
    select(-Name) %>%
    arrange(desc(Percent_Change))
  return(filtered_data)
}

# Połączenie stock_data i market_data (zakładamy, że istnieją)
combined_data <- bind_rows(stock_data, market_data)

# Interfejs użytkownika Shiny
ui <- fluidPage(
  titlePanel("Procentowa zmiana cen aktywów w wybranym okresie"),
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Data początkowa:", 
                value = as.Date("2024-01-01"), 
                min = as.Date("2024-01-01"), 
                max = as.Date("2025-04-20")),
      dateInput("end_date", "Data końcowa:", 
                value = as.Date("2025-04-20"), 
                min = as.Date("2024-01-01"), 
                max = as.Date("2025-04-20")),
      actionButton("update", "Aktualizuj wykres")
    ),
    mainPanel(
      plotlyOutput("dumbbell_plot")
    )
  )
)

# Serwer Shiny
server <- function(input, output, session) {
  # Obliczanie danych do wykresu po kliknięciu przycisku
  plot_data <- eventReactive(input$update, {
    data <- calculate_percent_change(combined_data, input$start_date, input$end_date)
    data$Start_Date <- input$start_date
    data$End_Date <- input$end_date
    return(data)
  })
  
  
  # Renderowanie interaktywnego wykresu
  output$dumbbell_plot <- renderPlotly({
    data <- plot_data()
    
    # Sortowanie danych względem Percent_Change
    data <- data %>%
      arrange(Percent_Change) %>%
      mutate(Symbol = factor(Symbol, levels = Symbol))  # Ustawienie kolejności jako factor levels
    
    # Tworzenie wykresu
    p <- ggplot(data, aes(y = Symbol)) +
      # Segmenty (linie) z odpowiednim kolorowaniem
      geom_segment(aes(x = 0, xend = Percent_Change,
                       yend = Symbol,
                       color = ifelse(Percent_Change >= 0, "Wzrost", "Spadek")),
                   size = 3, alpha = 0.5) + 
      # Punkty początkowe (0) w szarym kolorze
      geom_point(aes(x = 0, y = Symbol), color = "grey", size = 3) +
      # Punkty końcowe z kolorowaniem zależnym od zmiany procentowej
      geom_point(aes(x = Percent_Change, y = Symbol,
                     color = ifelse(Percent_Change >= 0, "Wzrost", "Spadek"),
                     text = paste0(
                       Symbol, "<br>",
                       "Start: ", sprintf("%.2f", Start_Close), "<br>",
                       "Koniec: ", sprintf("%.2f", End_Close), "<br>",
                       "Zmiana: ", sprintf("%.2f%%", Percent_Change)
                     )),
                 size = 3) +
      # Wyświetlanie procentowej zmiany w kolorach darkgreen lub darkred
      geom_text(aes(x = ifelse(Percent_Change >= 0, Percent_Change + 7, Percent_Change - 7),
                    y = Symbol,
                    label = sprintf("%.2f%%", Percent_Change),
                    color = ifelse(Percent_Change >= 0, "darkgreen", "darkred")), 
                size = 3.5, 
                vjust = 0.5,  # Ustawienie pionowego wyrównania
                hjust = ifelse(data$Percent_Change >= 0, 0, 1)) +  # Ustawienie poziomego wyrównania
      # Definicja kolorów dla segmentów i tekstu
      scale_color_manual(values = c("Wzrost" = "green", "Spadek" = "red",
                                    "darkgreen" = "darkgreen", "darkred" = "darkred")) +
      # Tytuł wykresu i oznaczenia osi
      labs(title = paste("Procentowa zmiana cen od", plot_data()[["Start_Date"]][1], "do", plot_data()[["End_Date"]][1]),
           x = "",
           y = "") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),     # Usunięcie tytułu osi X
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
}



# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)