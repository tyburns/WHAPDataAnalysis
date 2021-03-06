
# Shiny app to compare sets of units that have different treatments.
# The interface presents all units in all years in two columns with check boxes.
# Check boxes to select the sets to be compared (first column vs second column).
# The output table shows a "z-test" for wg, st and total mass per m2 in the units.
# Averages compared are NOT weighted by area of subunits.

# Comparisons will have to be modified once the covariance among units within
# years is estimated.

library(tidyverse)

gm2_2019 <-read.csv("qdt_mass_g_m2_2019.txt") %>%
  dplyr::select(-X) %>%
  mutate(year = 2019)

gm2_2020 <-read.csv("qdt_mass_g_m2_2020.txt") %>%
  dplyr::select(-X) %>%
  mutate(year = 2020)

gm2_2021 <-read.csv("qdt_mass_g_m2_2021.txt") %>%
  dplyr::select(-X) %>%
  mutate(year = 2021)

# Add columns to the 2019 and 2020 data for the total of timothy and watergrass 
# to compare to the same values in 2021. For 2019 and 2020, the total values 
# are the same as swamp timothy + watergrass values, but in 2021 the total 
# values include smartweed.
gm2_2019 <- gm2_2019 %>% 
  mutate(stwg_g_m2 = tot_g_m2,
         stwg_mass_m2_var = tot_mass_m2_var)

gm2_2020 <- gm2_2020 %>% 
  mutate(stwg_g_m2 = tot_g_m2,
         stwg_mass_m2_var = tot_mass_m2_var)
  

whap_su_years <- bind_rows(gm2_2019, gm2_2020, gm2_2021) %>%
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
      dplyr::select(wg_g_m2, st_g_m2, sw_g_m2, stwg_g_m2, tot_g_m2) %>%
      colMeans()
    
    set2_avg <- whap_su_years %>%
      dplyr::filter(subunit_year %in% input$set2) %>%
      dplyr::select(wg_g_m2, st_g_m2, sw_g_m2, stwg_g_m2, tot_g_m2) %>%
      colMeans()
    
    var_set1 <- whap_su_years %>%
      dplyr::filter(subunit_year %in% input$set1) %>%
      dplyr::select(wg_mass_m2_var, st_mass_m2_var, sw_mass_m2_var, 
                    stwg_mass_m2_var, tot_mass_m2_var) %>%
      colSums() %>%
      `/`(length(input$set1)^2)
    
    var_set2 <- whap_su_years %>%
      dplyr::filter(subunit_year %in% input$set2) %>%
      dplyr::select(wg_mass_m2_var, st_mass_m2_var, sw_mass_m2_var, 
                    stwg_mass_m2_var,tot_mass_m2_var) %>%
      colSums() %>%
      `/`(length(input$set2)^2)
    
    se_diff <- sqrt(var_set1 + var_set2)
    
    data.frame(set1_avg, set2_avg, se_diff) %>%
      mutate(variable = c("wg_g_m2", "st_g_m2", "sw_g_m2", "stwg_g_m2",
                          "tot_g_m2"),
             diff = set1_avg - set2_avg,
             z = abs(diff / se_diff),
             p_value = pt(q = z, df = 30, lower.tail = FALSE)) %>%
      dplyr::select(variable, set1_avg, set2_avg, diff, se_diff, z, p_value)
    
  })
}

shinyApp(ui, server)
