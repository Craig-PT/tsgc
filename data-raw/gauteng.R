## code to prepare `guanteng` dataset from csv provided by AH.
library('dplyr')
date.format <- "%Y-%m-%d"
fname <- file.path("data-raw", "SAJan6CT.csv")
gauteng <- read.csv(fname, row.names = 1, header = TRUE, check.names = FALSE,
              na.strings = c("#NA", '#N/A')) %>%
  select(c('Date', 'Gauteng')) %>%
  rename(Cases = Gauteng) %>%
  mutate(
    Date=as.Date(Date, format = date.format),
    Cases = as.integer(as.character(Cases))
  ) %>%
  as.xts(order.by = as.Date(.$Date), ) %>%
  as.data.frame() %>%
  mutate(
    Cases = as.integer(as.character(Cases))
  ) %>%
  select(., Cases) %>%
  rename(., cum_cases=Cases) %>%
  as.xts(order.by = as.Date(rownames(.), format = date.format))

# Stick
usethis::use_data(gauteng, overwrite = TRUE, compress = 'xz')

# Quick plot to see recovering what we want.
autoplot(diff(gauteng))