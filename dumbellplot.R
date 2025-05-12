# Ładowanie wymaganych pakietów
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(corrplot)

symbol_to_name <- data.frame(
  Symbol = c("TSLA", "NVDA", "GC=F", "EEM", "DX-Y.NYB", "DJT", "BTC-USD", "^IXIC", "^GSPC", "^VIX", "^HSI", "MARA"), # przykładowe symbole
  Name = c("Tesla", "Nvidia", "Złoto", "Rynki wschodządze", "Siła dolara", "Spółka Donalda Trumpa", "Bitcoin", "NASDAQ", "S&P500", "Indeks strachu", "Indeks chiński", "MARA") # pełne nazwy
)

# Dane o wydarzeniach wojny celnej
events <- data.frame(
  Date = as.Date(c(
    "2025-02-01", "2025-02-04", "2025-02-13", "2025-03-04", 
    "2025-03-10", "2025-03-12", "2025-04-02", "2025-05-08", 
    "2025-05-12", "2025-05-12"
  )),
  Description = c(
    "Początek wojny celnej: Trump ogłasza 10% cło na towary z Chin, 25% na Kanadę i Meksyk.",
    "Chiny odpowiadają: 15% cła na węgiel i LNG, 10% na ropę, maszyny rolnicze, samochody.",
    "Zapowiedź 'ceł wzajemnych': USA planuje dopasować cła do stawek innych krajów.",
    "Eskalacja ceł: USA podnosi cła na Chiny do 20%, 25% na Kanadę i Meksyk; Chiny odpowiadają.",
    "Chiny: Dodatkowe 15% cła na drób, wieprzowinę, soję, wołowinę; spadki na rynkach.",
    "Cła na stal i aluminium: USA podnosi cła do 25%; UE odpowiada cłami na $28 mld.",
    "'Dzień Wyzwolenia': USA wprowadza 34% cła na Chiny; Chiny odpowiadają restrykcjami.",
    "Umowa z Wielką Brytanią: Obniżenie ceł na samochody, zniesienie ceł na stal i wołowinę.",
    "Deeskalacja z Chinami: 90-dniowe zawieszenie ceł; USA redukuje cła z 145% do 30%.",
    "'Całkowity reset': Trump ogłasza reset stosunków handlowych z Chinami po negocjacjach."
  )
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
  tags$head(
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    tags$link(rel = "stylesheet", href = "https://unpkg.com/tippy.js@6/animations/scale.css"),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('addTooltip', function(data) {
      tippy('#' + data.id, {
        content: data.text,
        placement: data.placement || 'top',
        animation: 'scale',
        theme: 'light-border',
        delay: [100, 100],
        arrow: true
      });
    });
  ")),
    tags$style(HTML("
    .btn-red {
      background-color: red;
      color: white;
      border-color: darkred;
      margin: 5px;
      transition-duration: 0.3s;
    }
    .btn-red:hover {
      background-color: darkred;
      color: white;
      transition-duration: 0.3s;
    }
  "))
  ),
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
      actionButton("update", "Aktualizuj wykres"),
      tags$h4("Wybierz wydarzenie:"),
      actionButton("event_trump_attack", "Zamach na Trumpa", class = "btn-red"),
      actionButton("event_trump_election", "Wybory USA", class = "btn-red"),
      actionButton("event_trade_war", "Wojna celna", class = "btn-red"),
      actionButton("event_rates_decision", "Decyzja ws. stóp (XII)", class = "btn-red"),
      actionButton("event_japan_hike", "Podwyżka stóp w Japonii", class = "btn-red"),
      actionButton("event_deepseek", "Ogłoszenie DeepSeek", class = "btn-red"),
      # Dodajemy macierz korelacji pod przyciskami w sidebarPanel
      tags$h4("Macierz korelacji (pełny zakres danych):"),
      plotOutput("correlation_plot", height = "600px")
    ),
    mainPanel(
      plotlyOutput("dumbbell_plot"),
      tags$hr(), # Separator między wykresami
      plotlyOutput("line_plot") # Nowy wykres liniowy
    )
  ),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('clickUpdate', function(message) {
    setTimeout(function() {
      document.getElementById('update').click();
    }, 100);
  });
  "))
)

# Serwer Shiny
server <- function(input, output, session) {
  observe({
    session$sendCustomMessage("addTooltip", list(id = "event_trump_attack", text = "Zamach na Trumpa – 14–16 lipca 2024"))
    session$sendCustomMessage("addTooltip", list(id = "event_trump_election", text = "Trump wygrywa wybory – 4–11 listopada 2024"))
    session$sendCustomMessage("addTooltip", list(id = "event_rates_decision", text = "Decyzja Fed ws. stóp % – 17–19 grudnia 2024"))
    session$sendCustomMessage("addTooltip", list(id = "event_deepseek", text = "Ogłoszenie DeepSeek – 27 stycznia 2025"))
    session$sendCustomMessage("addTooltip", list(id = "event_trade_war", text = "Początek wojny cłowej – 1 luty 2025"))
    session$sendCustomMessage("addTooltip", list(id = "event_japan_hike", text = "Podwyżka stóp procentowych w Japonii – 31 lipca-6 sierpnia 2024"))
  })
  observeEvent(input$event_trump_attack, {
    updateDateInput(session, "start_date", value = as.Date("2024-07-15"))
    updateDateInput(session, "end_date", value = as.Date("2024-07-17"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  observeEvent(input$event_trump_election, {
    updateDateInput(session, "start_date", value = as.Date("2024-11-04"))
    updateDateInput(session, "end_date", value = as.Date("2024-11-11"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  observeEvent(input$event_trade_war, {
    updateDateInput(session, "start_date", value = as.Date("2025-02-01"))
    updateDateInput(session, "end_date", value = as.Date("2025-04-20"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  observeEvent(input$event_rates_decision, {
    updateDateInput(session, "start_date", value = as.Date("2024-12-17"))
    updateDateInput(session, "end_date", value = as.Date("2024-12-19"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  observeEvent(input$event_japan_hike, {
    updateDateInput(session, "start_date", value = as.Date("2024-07-31"))
    updateDateInput(session, "end_date", value = as.Date("2024-08-06"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  observeEvent(input$event_deepseek, {
    updateDateInput(session, "start_date", value = as.Date("2025-01-24"))
    updateDateInput(session, "end_date", value = as.Date("2025-01-27"))
    session$sendCustomMessage(type = "clickUpdate", message = list())
  })
  
  # Obliczanie danych do wykresu po kliknięciu przycisku
  plot_data <- eventReactive(input$update, {
    data <- calculate_percent_change(combined_data, input$start_date, input$end_date)
    data$Start_Date <- input$start_date
    data$End_Date <- input$end_date
    return(data)
  })
  
  # Obliczanie danych do wykresu liniowego (znormalizowane ceny dla wojny cłowej, wszystkie aktywa)
  line_plot_data <- eventReactive(input$update, {
    filtered_data <- combined_data %>%
      filter(Date >= as.Date("2025-02-01") & Date <= as.Date("2025-05-12")) %>% # Stały okres wojny cłowej
      left_join(symbol_to_name, by = "Symbol") %>%
      mutate(Symbol_Display = ifelse(is.na(Name), Symbol, Name)) %>% # Zachowujemy oryginalny Symbol i dodajemy nazwę do wyświetlania
      group_by(Symbol) %>%
      mutate(
        Min_Close = min(Close, na.rm = TRUE), # Minimalna cena w okresie
        Max_Close = max(Close, na.rm = TRUE), # Maksymalna cena w okresie
        Normalized_Close = (Close - Min_Close) / (Max_Close - Min_Close) # Normalizacja do skali 0-1
      ) %>%
      select(Date, Symbol, Symbol_Display, Normalized_Close, Close) %>%
      ungroup()
    return(filtered_data)
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
      geom_text(aes(x = ifelse(Percent_Change >= 0,
                               Percent_Change + 0.06 * max(abs(data$Percent_Change)),
                               Percent_Change - 0.06 * max(abs(data$Percent_Change))),
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
  
  # Renderowanie wykresu liniowego (znormalizowane ceny dla wojny cłowej, z pionowymi liniami)
  output$line_plot <- renderPlotly({
    data <- line_plot_data()
    
    # Tworzenie wykresu liniowego z pionowymi liniami
    p <- ggplot(data, aes(x = Date, y = Normalized_Close, color = Symbol_Display, group = Symbol_Display)) +
      geom_line(size = 1) +
      # Dodanie pionowych linii dla wydarzeń
      geom_vline(data = events, aes(xintercept = as.numeric(Date), 
                                    text = paste0(format(Date, "%Y-%m-%d"), "<br>", Description)),
                 linetype = "dashed", color = "black", size = 0.5, alpha = 0.7) +
      labs(title = "Znormalizowane ceny aktywów (Wojna celna: 1 luty 2025 - 12 maja 2025)",
           x = "Data",
           y = "Znormalizowana cena (0-1)") +
      theme_minimal() +
      theme(legend.position = "right") # Legenda po prawej stronie
    
    # Konwersja do plotly
    plot <- ggplotly(p, tooltip = c("Symbol_Display", "Date", "Normalized_Close", "Close", "text"))
    
    # Modyfikacja widoczności linii w legendzie
    default_visible <- c("Złoto", "S&P500", "Indeks strachu") # Pełne nazwy domyślnie widoczne
    for (i in seq_along(plot$x$data)) {
      # Sprawdzamy, czy ślad jest linią aktywu (ma nazwę w pełnych nazwach)
      if (!is.null(plot$x$data[[i]]$name) && plot$x$data[[i]]$name %in% symbol_to_name$Name) {
        symbol_name <- plot$x$data[[i]]$name
        plot$x$data[[i]]$visible <- if (symbol_name %in% default_visible) TRUE else "legendonly"
      }
    }
    
    # Ustawienie układu wykresu
    plot <- plot %>% 
      layout(
        legend = list(
          title = list(text = "Aktywa"),
          orientation = "v",
          x = 1.05, # Legenda po prawej stronie
          y = 0.5
        ),
        # Zapewnienie, że tooltipy dla vline działają
        hovermode = "x unified"
      )
    
    return(plot)
  })
  
  # Renderowanie macierzy korelacji
  output$correlation_plot <- renderPlot({
    company_data <- combined_data %>%
      select(Date, Symbol, Close) %>%
      pivot_wider(names_from = Symbol, values_from = Close) %>%
      arrange(Date)
    
    # Sprawdzenie, czy są dane
    if (ncol(company_data) < 3) {
      plot.new()
      text(0.5, 0.5, "Za mało danych do wygenerowania korelacji", cex = 1.5)
    } else {
      # Macierz korelacji
      correlation_matrix <- cor(company_data[,-1], use = "pairwise.complete.obs")
      
      # Podstawienie pełnych nazw (jeśli dostępne)
      symbol_map <- setNames(symbol_to_name$Name, symbol_to_name$Symbol)
      
      colnames(correlation_matrix) <- ifelse(colnames(correlation_matrix) %in% names(symbol_map),
                                             symbol_map[colnames(correlation_matrix)],
                                             colnames(correlation_matrix))
      rownames(correlation_matrix) <- colnames(correlation_matrix)
      
      # Definicja palety czerwono-zielonej
      col_palette <- colorRampPalette(c("red", "white", "green"))(200)
      
      # Rysowanie wykresu
      corrplot::corrplot(correlation_matrix, method = "color", type = "full",
                         tl.col = "black", addCoef.col = "black", number.cex = 0.7, col = col_palette)
      
    }
  }, bg = "transparent")
}



# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)