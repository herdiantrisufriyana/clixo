# Load packages
library(clixo)
library(tidyverse)


# Load simulated data
input=input_example()

# Create CliXO
ontology=clixo(input$similarity)

ontology %>%
  knitr::kable(
    format='html'
    ,caption='Clique-extracted ontology (CliXO)'
  ) %>%
  kableExtra::kable_styling(full_width=T)
