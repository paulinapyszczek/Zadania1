rm(list = ls())

library(tidyverse)

# zadanie 1

czy_podzielna <- function(x, y) {
  
  ifelse(x%%y != 0, 'nie', 'tak')
  
}

czy_podzielna(16, 4)
czy_podzielna(16, 3.5)

# zadanie 2

1/mean(1/c(120,90))

# zadanie 3

corr <- function(x, y) {
  
  cov(x, y)/(sd(x, na.rm = T) * sd(y, na.rm = T))
  
}

dane <- read.csv2('dane.csv', header = T)

corr(dane$waga, dane$wzrost)

# wartosc 97,9% oznacza, że wystepuje silna dodatnia korelacja liniowa (zwiazek liniowy) 
# pomiedzy zmiennymi wzrost i waga

# zadanie 4

stworzDataFrame <- function(ile = 1, ...) {
  
  dane <- list(...)
  n_elements <- length(dane)/(ile + 1)
  
  m <- t(matrix(dane[(n_elements+1):length(dane)], n_elements, (length(dane)/n_elements-1)))
  colnames(m) <- unlist(dane[1:n_elements])
  
  df <- as.data.frame(m)
  
  return(df)
  
}

stworzDataFrame(ile = 4,
                'imie', 'nazwisko', 'wiek', 'plec', 'miejscowosc',
                "ala", "kot", 23, 'K', 'warszawa',
                "ilona", "kowal", 22, 'K', 'krakow',
                "jan", "jakubiak", 21, 'M', NA,
                'jola', 'kowalska', 22, NA, NA
)

# zadanie 5

liczZplikow <- function(sciezka, nazwaKolumny, jakaFunkcja = "mean", DlaIluPlikow = 1) { 
  
  files <- list.files(path = sciezka)
  col <- paste0('X', nazwaKolumny)
  
  
  for (i in 1:DlaIluPlikow) {
    
    file <- files[i]
    dane <- read.csv(file = paste0(sciezka, '\\', file), na.strings = c("","NA"))
    
    
    if (length(unique(dane[[col]])) == 1) {
      
      ifelse(is.na(unique(dane[[col]])), 
             print(paste0(file, ' -  result: only NaN values')),
             print(paste0(file, ' -  result: ', unique(dane[[col]]))))
      
    } else {
      
      result <- lapply(dane[col], FUN = jakaFunkcja, na.rm = T)
      print(paste0(file, ' -  result: ', result))
      
    }
  }
}


liczZplikow(sciezka = '.\\smogKrakow', nazwaKolumny = '140_temperature'
            , jakaFunkcja = "mean", DlaIluPlikow = 12)


