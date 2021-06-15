# Shiny app to compare sets of units that have different treatments.

# Comparisons will have to be modified once the covariance among units within
# years is estimated.

library(tidyverse)

gm2_2019 <-read.csv("qdt_mass_g_m2_2019.txt") %>%
  dplyr::select(-X) %>%
  mutate(year = 2019)

gm2_2020 <-read.csv("qdt_mass_g_m2_2020.txt") %>%
  dplyr::select(-X) %>%
  mutate(year = 2020)

whap_su_years <- bind_rows(gm2_2019, gm2_2020) %>%
  mutate(year = factor(year),
         subunit_year = paste(subunit_ID, year, sep = "_"))

all_units_years <- unique(whap_su_years$subunit_year)

ui <- fluidPage(
  
  titlePanel("Compare sets of subunits"),
  
  fluidRow(
    column(3, checkboxGroupInput(inputId = "set1",
                                 label = "Select units in the first group:",
                                 choices = all_units_years)),
    column(3, checkboxGroupInput(inputId = "set2",
                                 label = "Select units in the second group:",
                                 choices = all_units_years)),
    column(6, tableOutput("group_comparison"))
  )
)

server <- function(input, output) {
  
  output$group_comparison <- renderTable({
    
    sets12 <-c(input$set1, input$set2)
    
    set1_avg <- whap_su_years %>%
      dplyr::filter(subunit_year %in% input$set1) %>%
      dplyr::select(wg_g_m2, st_g_m2, tot_g_m2) %>%
      colMeans()
    
    set2_avg <- whap_su_years %>%
      dplyr::filter(subunit_year %in% input$set2) %>%
      dplyr::select(wg_g_m2, st_g_m2, tot_g_m2) %>%
      colMeans()
    
    se_diff <- whap_su_years %>%
      dplyr::filter(subunit_year %in% sets12) %>%
      dplyr::select(wg_mass_m2_var, st_mass_m2_var, tot_mass_m2_var) %>%
      colSums() %>%
      sqrt()
    
    data.frame(set1_avg, set2_avg, se_diff) %>%
      mutate(diff = set1_avg - set2_avg,
             z = abs(diff / se_diff),
             p_value = pt(q = z, df = 30, lower.tail = FALSE))
    
  })
}

shinyApp(ui, server)
