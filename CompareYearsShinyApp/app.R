# Compare total mass between years
# Compare all units measured in two years within refuges
# Add comparisons between management options and arbitrary groups of units

library(shiny)

qdt_mass_g_m2_2019 <- read.csv("qdt_mass_g_m2_2019.txt") %>%
    dplyr::select(-X)

qdt_mass_g_m2_2020 <- read.csv("qdt_mass_g_m2_2020.txt") %>%
    dplyr::select(-X)

qdt_mass_g_m2_2020 <- read.csv("qdt_mass_g_m2_2021.txt") %>%
    dplyr::select(-X)

# Differences are tested for statistical significance by assuming that estimates
# are independent and that the residual variance used to get the estimates has 
diff_test <- function(mu1, var1, mu2, var2) {
    Diff <- abs(mu2 - mu1)
    Diff_sgn <- sign(mu2 - mu1)
    se_diff <- sqrt(var1 + var2)
    z_calc <- Diff / se_diff
    p_diff <- pt(q = z_calc, df = 30)
    return(data.frame(Difference = Diff * Diff_sgn,
                      SE_of_difference = se_diff,
                      Probability_2_tailed = 1 - p_diff))
}


ui <- fluidPage(tags$h2("Compare productivity between two units in different years"),
    
    selectInput(inputId = "tot_g_m2_2019",
                label = "Unit in 2019",
                choices = unique(qdt_mass_g_m2_2019$subunit_ID)),
    selectInput(inputId = "tot_g_m2_2020",
                label = "Unit in 2020",
                choices = unique(qdt_mass_g_m2_2020$subunit_ID)),
    
    tableOutput("subunits"),
    
    tableOutput("test_diff")
)

server <- function(input, output) {
    
    output$subunits <- renderTable({
        bind_rows(
            qdt_mass_g_m2_2019 %>%
                dplyr::filter(subunit_ID == input$tot_g_m2_2019),
            qdt_mass_g_m2_2020 %>%
                dplyr::filter(subunit_ID == input$tot_g_m2_2020)) %>%
            mutate(Year = factor(c(2019, 2020))) %>% 
            dplyr::select(subunit_ID,
                          Year,
                          wg_g_m2,
                          wg_mass_m2_var,
                          st_g_m2,
                          st_mass_m2_var,
                          tot_g_m2,
                          tot_mass_m2_var)}, rownames = TRUE)
    
    output$test_diff <- renderTable({
        diff_test(mu1 = qdt_mass_g_m2_2019 %>%
                      dplyr::filter(subunit_ID == input$tot_g_m2_2019) %>%
                      pluck("tot_g_m2"),
                  var1 = qdt_mass_g_m2_2019 %>%
                      dplyr::filter(subunit_ID == input$tot_g_m2_2019) %>%
                      pluck("tot_mass_m2_var"),
                  mu2 = qdt_mass_g_m2_2020 %>%
                      dplyr::filter(subunit_ID == input$tot_g_m2_2020) %>%
                      pluck("tot_g_m2"),
                  var2 = qdt_mass_g_m2_2020 %>%
                      dplyr::filter(subunit_ID == input$tot_g_m2_2020) %>%
                      pluck("tot_mass_m2_var"))})
}


shinyApp(ui = ui, server = server)