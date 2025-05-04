# Ładowanie wymaganego pakietu
library(quantmod)

# Zakres dat
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2025-04-20")

# Symbole spółek
symbols <- c("TSLA", "NVDA", "DJT", "MARA")

# Funkcja do pobierania danych
get_stock_data <- function(symbol) {
  tryCatch({
    stock <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(
      Date = index(stock),
      Open = as.numeric(Op(stock)),
      High = as.numeric(Hi(stock)),
      Low = as.numeric(Lo(stock)),
      Close = as.numeric(Cl(stock)),
      Volume = as.numeric(Vo(stock))
    )
    df$Symbol <- symbol
    return(df)
  }, error = function(e) {
    message("Błąd przy pobieraniu danych dla ", symbol, ": ", e$message)
    return(NULL)
  })
}

# Pobierz dane i połącz
stock_data <- do.call(rbind, Filter(Negate(is.null), lapply(symbols, get_stock_data)))