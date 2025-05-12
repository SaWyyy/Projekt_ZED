# Ładowanie wymaganych pakietów
library(quantmod)
library(writexl)

# Zakres dat
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2025-04-20")

# Symbole aktywów (akcje i indeksy/aktywa)
symbols <- c("^GSPC", "^IXIC", "^VIX", "DX-Y.NYB", "BTC-USD", "EEM", "GC=F", "^HSI")

# Funkcja do pobierania danych
get_market_data <- function(symbol) {
  tryCatch({
    # Pobieranie danych z Yahoo Finance
    data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(
      Date = index(data),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data)),
      Volume = as.numeric(Vo(data))
    )
    df$Symbol <- symbol
    return(df)
  }, error = function(e) {
    message("Błąd przy pobieraniu danych dla ", symbol, ": ", e$message)
    return(NULL)
  })
}


# Pobierz dane i połącz
market_data <- do.call(rbind, Filter(Negate(is.null), lapply(symbols, get_market_data)))

# Eksport do pliku Excel
write_xlsx(market_data, path = "market_data.xlsx")