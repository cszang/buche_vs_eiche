library(shiny)
library(bslib)
library(dplR)
library(treeclim)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)

oak <- read.rwl("data/bausenberg_oak.rwl")
beech <- read.rwl("data/bausenberg_beech.rwl")
climate <- read.csv2("data/climate_bausenberg.csv")
clim_y <- climate %>% 
  group_by(year) %>% 
  summarise(Temperatur = mean(tmean),
            Niederschlag = sum(prec),
            SPEI3_Fruehjahr = spei_3[5],
            SPEI3_Sommer = spei_3[8]) %>% 
  filter(year > 1949) %>% 
  rename(Jahr = year)
lfu <- read_csv2("data/lfu_cordex_ensemble_monthly.csv") %>% 
  mutate(gcm_rcm = paste(gcm, rcm, sep = "+"))

tidy_dendro <- function(x, species) {
  x$year <- as.numeric(rownames(x))
  x <- pivot_longer(x, -year, values_to = "rwi", names_to = "tree")
  x$species <- species
  x
}

vsfiles <- list.files("vslite", pattern = "\\.R")
lapply(vsfiles, function(x) source(paste0("vslite/", x)))

params_beech <- c(T1 = 3.80, T2 = 9.09, M1 = 0.01, M2 = 0.317)
params_oak <- c(T1 = 2.80, T2 = 9.00, M1 = 0.01, M2 = 0.499)

load("data/input_transient.rda")

# Define UI for application that draws a histogram
ui <- page_fluid(
  
  theme = bs_theme(preset = "sandstone"),
  
    navset_underline(
    
    nav_panel("Rohdaten",
              
              h2("Rohdaten für Jahrringbreiten und Klimadaten"),
              
              card(
                max_height = 400, 
                full_screen = TRUE,
                card_header("Rohdaten Buche (Jahrringbreiten)"),
                layout_sidebar(
                  sidebar = sidebar(
                    uiOutput("raw_series_beech"),
                    open = TRUE
                  ),
                  plotlyOutput("buche_roh")
                )
              )
              ,
              card(
                max_height = 400,
                full_screen = TRUE,
                card_header("Rohdaten Eiche (Jahrringbreiten)"),
                layout_sidebar(
                  sidebar = sidebar(
                    uiOutput("raw_series_oak"),
                    open = TRUE
                  ),
                  plotlyOutput("eiche_roh")
                )
              )
              ,
              fluidRow(
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    full_screen = TRUE,
                    card_header("Temperatur"),
                    plotlyOutput("temp_hist")
                  )
                ),
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    full_screen = TRUE,
                    card_header("Niederschlag"),
                    plotlyOutput("prec_hist")
                  )
                )
              )
              ,
              fluidRow(
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    full_screen = TRUE,
                    card_header("SPEI3 Frühjahr"),
                    plotlyOutput("spei3_spring_hist")
                  )
                ),
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    full_screen = TRUE,
                    card_header("SPEI3 Sommer"),
                    plotlyOutput("spei3_summer_hist")
                  )
                )
              )
              
    ),
    nav_panel("Trendbereinigung",
              
              h2("Entfernung von Alterstrend und Managementeffekten"),
              
              card(
                max_height = 400,
                card_header("Trendanpassung, Beispiel Buche"),
                layout_sidebar(
                  sidebar = sidebar(
                    radioButtons("dmethod", "Detrending-Methode", 
                                 choiceNames = c("Spline", "Negativ exponentiell"),
                                 choiceValues = c("Spline", "ModNegExp"),
                                 selected = "Spline"
                                 ),
                    sliderInput("nyrs", "Flexibilität (Jahre)", min = 10,
                                max = 100, value = 30, step = 5)
                  ),
                  plotlyOutput("buche_ausgleichskurve")
                )
              ),
              card(
                max_height = 400,
                card_header("Chronologien für Buche und Eiche"),
                plotlyOutput("chronos")
              ),
              card(
                max_height = 400,
                card_header("Dichte Jahrringindex für Buche und Eiche"),
                plotlyOutput("chrono_density")
              )
    ),
    nav_panel("Toleranz-Indices",
              
              h2("Indices der Toleranz nach Lloret et al."),
              
              card(
                max_height = 400,
                card_header("Resistenz im Artvergleich"),
                layout_sidebar(
                  sidebar = sidebar(
                    textInput("rt_years", "Dürrejahre", value = "1976, 2003, 2015"),
                    sliderInput("rt_winsize", "Fenstergröße (Jahre)", min = 3,
                                max = 11, value = 7, step = 2)
                  ),
                  plotlyOutput("rt_comp")
                )
              ),
              card(
                max_height = 400,
                card_header("Recovery im Artvergleich"),
                layout_sidebar(
                  sidebar = sidebar(
                    textInput("rc_years", "Dürrejahre", value = "1976, 2003, 2015"),
                    sliderInput("rc_winsize", "Fenstergröße (Jahre)", min = 3,
                                max = 11, value = 7, step = 2)
                  ),
                  plotlyOutput("rc_comp")
                )
              ),
              card(
                max_height = 400,
                card_header("Resilienz im Artvergleich"),
                layout_sidebar(
                  sidebar = sidebar(
                    textInput("rs_years", "Dürrejahre", value = "1976, 2003, 2015"),
                    sliderInput("rs_winsize", "Fenstergröße (Jahre)", min = 3,
                                max = 11, value = 7, step = 2)
                  ),
                  plotlyOutput("rs_comp")
                )
              ),
    ),
    nav_panel("Klimakorrelationen",
              
              h2("Einfache Korrelationen zwischen Zuwachs und Klima"),
              
              h3("Bivariate Korrelationen: nur eine Klimavariable"),
              
              fluidRow(
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    card_header("Korrelation Buche"),
                    layout_sidebar(
                      sidebar = sidebar(
                        radioButtons("variable_beech", "Variable", 
                                     choices = c("Temperatur MAM", "Temperatur JJA",
                                                 "Niederschlag MAM", "Niederschlag JJA"),
                                     selected = "Temperatur MAM"
                        ),
                        uiOutput("corr_beech")),
                      plotlyOutput("buche_korrelationen")
                    )
                  )
                )
                ,
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    card_header("Korrelation Eiche"),
                    layout_sidebar(
                      sidebar = sidebar(
                        radioButtons("variable_oak", "Variable", 
                                     choices = c("Temperatur MAM", "Temperatur JJA",
                                                 "Niederschlag MAM", "Niederschlag JJA"),
                                     selected = "Temperatur MAM"
                        ),
                        uiOutput("corr_oak")),
                      plotlyOutput("eiche_korrelationen")
                    )
                  )
                ),
                h3("Multivariate Korrelationen: mehrere Klimavariablen einbeziehen"),
                div("Hier ergibt sich das Problem der Multikollinearität:"),
                fluidRow(
                  column(
                    width = 6,
                    card(
                      max_height = 400,
                      card_header("Multikollinearität Temperatur"),
                      plotlyOutput("multicol_temp")
                    )
                  ),
                  column(
                    width = 6,
                    card(
                      max_height = 400,
                      card_header("Multikollinearität Niederschlag"),
                      plotlyOutput("multicol_prec")
                    )
                  )
                ),
                div("Lösung: Response Functions!")
              )
              
    ),
    nav_panel("Response Functions",
              
              h2("Multivariate Korrelationen mit Klimavariablen"),
              
              card(
                max_height = 400,
                card_header("Response Functions Buche"),
                layout_sidebar(
                  sidebar = sidebar(
                    checkboxGroupInput("beech_dcc_vars", "Variablen",
                                       choiceNames = list(
                                         "Mitteltemperatur",
                                         "Minimumtemperatur",
                                         "Maximumtemperatur",
                                         "Niederschlag",
                                         "SPEI1",
                                         "SPEI3",
                                         "SPEI6"
                                       ),
                                       choiceValues = list(
                                         "tmean",
                                         "tmin",
                                         "tmax",
                                         "prec",
                                         "spei_1",
                                         "spei_3",
                                         "spei_6"
                                       ),
                                       selected = list(
                                         "tmean", "prec"
                                       )),
                    checkboxGroupInput("beech_months", "Monate für Analyse",
                                       choiceNames = list(
                                         "September Vorjahr",
                                         "Oktober Vorjahr",
                                         "November Vorjahr",
                                         "Dezember Vorjahr",
                                         "Januar",
                                         "Februar",
                                         "März",
                                         "April",
                                         "Mai",
                                         "Juni",
                                         "Juli",
                                         "August",
                                         "September",
                                         "Oktober"),
                                       choiceValues = list(
                                         -9, -10, -11, -12, 1, 2, 3, 4,
                                         5, 6, 7, 8, 9, 10
                                       ),
                                       selected = list(
                                         4, 5, 6, 7, 8, 9
                                       )),
                    radioButtons("beech_dynamic", "Modus", choiceNames = list("statisch",
                                                                        "dynamisch"),
                                 choiceValues = list("static", "moving")),
                    textInput("beech_tstart", "Startjahr", value = "", width = "30%"),
                    textInput("beech_tend", "Endjahr", value = "", width = "30%"),
                    sliderInput("beech_winsize", "Fenstergröße", min = 20, max = 70, value = 30)
                  ),
                  plotOutput("beech_tcplot")
                )),
                card(
                  max_height = 400,
                  card_header("Response Functions Eiche"),
                  layout_sidebar(
                    sidebar = sidebar(
                      checkboxGroupInput("oak_dcc_vars", "Variablen",
                                         choiceNames = list(
                                           "Mitteltemperatur",
                                           "Minimumtemperatur",
                                           "Maximumtemperatur",
                                           "Niederschlag",
                                           "SPEI1",
                                           "SPEI3",
                                           "SPEI6"
                                         ),
                                         choiceValues = list(
                                           "tmean",
                                           "tmin",
                                           "tmax",
                                           "prec",
                                           "spei_1",
                                           "spei_3",
                                           "spei_6"
                                         ),
                                         selected = list(
                                           "tmean", "prec"
                                         )),
                      checkboxGroupInput("oak_months", "Monate für Analyse",
                                         choiceNames = list(
                                           "September Vorjahr",
                                           "Oktober Vorjahr",
                                           "November Vorjahr",
                                           "Dezember Vorjahr",
                                           "Januar",
                                           "Februar",
                                           "März",
                                           "April",
                                           "Mai",
                                           "Juni",
                                           "Juli",
                                           "August",
                                           "September",
                                           "Oktober"),
                                         choiceValues = list(
                                           -9, -10, -11, -12, 1, 2, 3, 4,
                                           5, 6, 7, 8, 9, 10
                                         ),
                                         selected = list(
                                           4, 5, 6, 7, 8, 9
                                         )),
                      radioButtons("oak_dynamic", "Modus", choiceNames = list("statisch",
                                                                                "dynamisch"),
                                   choiceValues = list("static", "moving")),
                      textInput("oak_tstart", "Startjahr", value = "", width = "30%"),
                      textInput("oak_tend", "Endjahr", value = "", width = "30%"),
                      sliderInput("oak_winsize", "Fenstergröße", min = 20, max = 70, value = 30)
                    ),
                    plotOutput("oak_tcplot")
                  )
              )
              
    ),
    nav_panel("Wachstumsanalyse",
              
              h2("Dendroökologische Wachstumsanalyse"),
              card(
                max_height = 400,
                full_screen = TRUE,
                card_header("Wachstumsanalyse Buche"),
                plotlyOutput("ga_beech")
                
              ),
              card(
                max_height = 400,
                full_screen = TRUE,
                card_header("Wachstumsanalyse Eiche"),
                plotlyOutput("ga_oak")
                
              )
              ),
    nav_panel("Klima der Zukunft",
              
              h2("CMIP5 Modellensemble des LfU"),
             
              card(
                max_height = 400,
                card_header("Modellprojektionen Temperatur"),
                layout_sidebar(
                  sidebar = sidebar(
                    radioButtons("scenario_viz_temp", "Szenario", 
                                 choiceNames = c("RCP2.6", "RCP4.5", "RCP8.5"),
                                 choiceValues = c("RCP_26", "RCP_45", "RCP_85"),
                                 selected = "RCP_45"
                    )),
                  plotlyOutput("scenarios_temp")
                )
              ),
              card(
                max_height = 400,
                card_header("Modellprojektionen Niederschlag"),
                layout_sidebar(
                  sidebar = sidebar(
                    radioButtons("scenario_viz_prec", "Szenario", 
                                 choiceNames = c("RCP2.6", "RCP4.5", "RCP8.5"),
                                 choiceValues = c("RCP_26", "RCP_45", "RCP_85"),
                                 selected = "RCP_45"
                    )),
                  plotlyOutput("scenarios_prec")
                )
              )
               
              ),
    nav_panel("Projektion",
              
              h2("Projektion des Wachstums im Klimawandel"),
              div(
                img(src = "vslite.png", height = "400px")
              ),
              card(
                max_height = 400,
                card_header("Wachstumsprojektion mit VSLite"),
                layout_sidebar(
                  sidebar = sidebar(
                    radioButtons("vs_rcp", "Szenario", 
                                 choices = c("RCP2.6", "RCP4.5", "RCP8.5"),
                                 selected = "RCP4.5")
                  ),
                plotlyOutput("vslite")
              )),
              fluidRow(
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    card_header("Parameter Buche"),
                    sliderInput("t1_b", "T1", min = 0, max = 8.5, value = params_beech["T1"]),
                    sliderInput("t2_b", "T2", min = 9, max = 20, value = params_beech["T2"]),
                    sliderInput("m1_b", "M1", min = 0.01, max = 0.03, value = params_beech["M1"]),
                    sliderInput("m2_b", "M2", min = 0.1, max = 0.5, value = params_beech["M2"]),
                    shiny::actionButton("reset_beech", "Reset")
                  )
                ),
                column(
                  width = 6,
                  card(
                    max_height = 400,
                    card_header("Parameter Eiche"),
                    sliderInput("t1_o", "T1", min = 0, max = 8.5, value = params_oak["T1"]),
                    sliderInput("t2_o", "T2", min = 9, max = 20, value = params_oak["T2"]),
                    sliderInput("m1_o", "M1", min = 0.01, max = 0.03, value = params_oak["M1"]),
                    sliderInput("m2_o", "M2", min = 0.1, max = 0.5, value = params_oak["M2"]),
                    shiny::actionButton("reset_oak", "Reset")
                  )
                )
                )
              )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$plot <- renderPlot({
        plot(1:10)
    })
    
    output$raw_series_beech <- renderUI({
      trees <- sort(names(beech))
      # Create the checkboxes and select them all by default
      checkboxGroupInput("input_raw_series_beech", "Bäume", 
                         choices  = trees,
                         selected = trees)
    })
    
    output$raw_series_oak <- renderUI({
      trees <- sort(names(oak))
      # Create the checkboxes and select them all by default
      checkboxGroupInput("input_raw_series_oak", "Bäume", 
                         choices  = trees,
                         selected = trees)
    })
    
    output$buche_roh <- renderPlotly({
      trees <- sort(names(beech))
      beech <- beech[, trees]
      time <- as.numeric(rownames(beech))
      beech$Jahr <- time
      beech_long <- pivot_longer(beech, cols = -Jahr, names_to = "Baum",
                                 values_to = "Jahrringbreite")
      beech_long <- beech_long %>% 
        filter(Baum %in% input$input_raw_series_beech)
      plot_ly(
        data = beech_long,
        x = ~Jahr,
        y = ~Jahrringbreite,
        color = ~Baum,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$eiche_roh <- renderPlotly({
      trees <- sort(names(oak))
      oak <- oak[, trees]
      time <- as.numeric(rownames(oak))
      oak$Jahr <- time
      oak_long <- pivot_longer(oak, cols = -Jahr, names_to = "Baum",
                                 values_to = "Jahrringbreite")
        # mutate(Jahr = lubridate::ymd(paste(Jahr, "01", "01", sep = "-")))
      oak_long <- oak_long %>% 
        filter(Baum %in% input$input_raw_series_oak)
      # ggplot(oak_long, aes(Jahr, Jahrringbreite)) +
      #   geom_line(aes(colour = Baum))
      plot_ly(
        data = oak_long,
        x = ~Jahr,
        y = ~Jahrringbreite,
        color = ~Baum,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$buche_ausgleichskurve <- renderPlotly({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs, return.info = TRUE)
      beech_curves <- beech_d$curves
      trees <- sort(names(beech))
      beech_curves <- beech_curves[, trees]
      time <- as.numeric(rownames(beech_curves))
      beech_curves$Jahr <- time
      beech_curves_long <- pivot_longer(beech_curves, cols = -Jahr, names_to = "Baum",
                                 values_to = "Ausgleichskurve")
      beech_curves_long <- beech_curves_long %>% 
        filter(Baum %in% input$input_raw_series_beech)
      trees <- sort(names(beech))
      beech <- beech[, trees]
      time <- as.numeric(rownames(beech))
      beech$Jahr <- time
      beech_long <- pivot_longer(beech, cols = -Jahr, names_to = "Baum",
                                 values_to = "Jahrringbreite")
      beech_long <- beech_long %>% 
        filter(Baum %in% input$input_raw_series_beech)
      beech_both <- full_join(beech_long, beech_curves_long)
      plot_ly(
        data = beech_both,
        x = ~Jahr) %>% 
        add_trace(
          y = ~Ausgleichskurve,
          color = ~Baum,
          type = 'scatter',
          mode = 'lines') %>% 
        add_trace(
          y = ~Jahrringbreite,
          color = ~Baum,
          type = 'scatter',
          mode = 'lines')
    })
    
    output$chronos <- renderPlotly({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_c <- chron(beech_d)
      time <- as.numeric(rownames(beech_c))
      beech_c$Jahr <- time
      names(beech_c)[1] <- "Jahrringindex"
      beech_c$Art <- "Buche"
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_c <- chron(oak_d)
      time <- as.numeric(rownames(oak_c))
      oak_c$Jahr <- time
      names(oak_c)[1] <- "Jahrringindex"
      oak_c$Art <- "Eiche"
      both_c <- rbind(beech_c, oak_c)
      plot_ly(
        data = both_c,
        x = ~Jahr,
        y = ~Jahrringindex,
        color = ~Art,
        type = 'scatter',
        mode = 'lines')
    })
    
    output$chrono_density <- renderPlotly({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_c <- chron(beech_d)
      time <- as.numeric(rownames(beech_c))
      beech_c$Jahr <- time
      names(beech_c)[1] <- "Jahrringindex"
      beech_c$Art <- "Buche"
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_c <- chron(oak_d)
      time <- as.numeric(rownames(oak_c))
      oak_c$Jahr <- time
      names(oak_c)[1] <- "Jahrringindex"
      oak_c$Art <- "Eiche"
      both_c <- rbind(beech_c, oak_c)
      plot_ly(
        data = both_c,
        x = ~Jahrringindex,
        color = ~Art,
        type = "histogram",
        histnorm = 'probability density',
        nbinsx = 30,
        opacity = 0.6
        )
    })
    
    output$buche_korrelationen <- renderPlotly({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_c <- chron(beech_d)
      if (input$variable_beech == "Temperatur MAM") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("tmean", 3:5))  
      }
      if (input$variable_beech == "Temperatur JJA") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("tmean", 6:8))  
      }
      if (input$variable_beech == "Niederschlag MAM") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("prec", 3:5))  
      }
      if (input$variable_beech == "Niederschlag JJA") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("prec", 6:8))  
      }
      
      beech_dlm_frame <- data.frame(
        Jahrringindex = beech_dlm$model[[1]],
        Klimavariable = beech_dlm$model[[2]]
      )
      fit <- lm(Jahrringindex ~ Klimavariable, data = beech_dlm_frame)
      regression_line <- data.frame(
        x = beech_dlm_frame$Klimavariable,
        y = predict(fit)
      )
      plot_ly(
        data = beech_dlm_frame,
        x = ~Klimavariable,
        y = ~Jahrringindex,
        type = 'scatter',
        mode = 'markers'
      ) %>% 
        add_trace(
          data = regression_line,
          x = ~x,
          y = ~y,
          type = 'scatter',
          mode = 'lines'
        ) %>% 
        layout(showlegend = FALSE)
      
    })
    
    output$eiche_korrelationen <- renderPlotly({
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_c <- chron(oak_d)
      if (input$variable_oak == "Temperatur MAM") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("tmean", 3:5))  
      }
      if (input$variable_oak == "Temperatur JJA") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("tmean", 6:8))  
      }
      if (input$variable_oak == "Niederschlag MAM") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("prec", 3:5))  
      }
      if (input$variable_oak == "Niederschlag JJA") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("prec", 6:8))  
      }
      
      oak_dlm_frame <- data.frame(
        Jahrringindex = oak_dlm$model[[1]],
        Klimavariable = oak_dlm$model[[2]]
      )
      fit <- lm(Jahrringindex ~ Klimavariable, data = oak_dlm_frame)
      regression_line <- data.frame(
        x = oak_dlm_frame$Klimavariable,
        y = predict(fit)
      )
      plot_ly(
        data = oak_dlm_frame,
        x = ~Klimavariable,
        y = ~Jahrringindex,
        type = 'scatter',
        mode = 'markers'
      ) %>% 
        add_trace(
          data = regression_line,
          x = ~x,
          y = ~y,
          type = 'scatter',
          mode = 'lines'
        ) %>% 
        layout(showlegend = FALSE)
      
    })
    
    output$corr_beech <- renderUI({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_c <- chron(beech_d)
      if (input$variable_beech == "Temperatur MAM") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("tmean", 3:5))  
      }
      if (input$variable_beech == "Temperatur JJA") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("tmean", 6:8))  
      }
      if (input$variable_beech == "Niederschlag MAM") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("prec", 3:5))  
      }
      if (input$variable_beech == "Niederschlag JJA") {
        beech_dlm <- dlm(beech_c, climate, selection = .mean("prec", 6:8))  
      }
      
      beech_dlm_frame <- data.frame(
        Jahrringindex = beech_dlm$model[[1]],
        Klimavariable = beech_dlm$model[[2]]
      )
      cor <- cor(beech_dlm_frame$Jahrringindex, beech_dlm_frame$Klimavariable)
      HTML("Korrelation =", round(cor, 2))
    })
    
    output$corr_oak <- renderUI({
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_c <- chron(oak_d)
      if (input$variable_oak == "Temperatur MAM") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("tmean", 3:5))  
      }
      if (input$variable_oak == "Temperatur JJA") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("tmean", 6:8))  
      }
      if (input$variable_oak == "Niederschlag MAM") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("prec", 3:5))  
      }
      if (input$variable_oak == "Niederschlag JJA") {
        oak_dlm <- dlm(oak_c, climate, selection = .mean("prec", 6:8))  
      }
      
      oak_dlm_frame <- data.frame(
        Jahrringindex = oak_dlm$model[[1]],
        Klimavariable = oak_dlm$model[[2]]
      )
      cor <- cor(oak_dlm_frame$Jahrringindex, oak_dlm_frame$Klimavariable)
      HTML("Korrelation =", round(cor, 2))
    })
    
    output$temp_hist <- renderPlotly({
      plot_ly(
        data = clim_y,
        x = ~Jahr,
        y = ~Temperatur,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$prec_hist <- renderPlotly({
      plot_ly(
        data = clim_y,
        x = ~Jahr,
        y = ~Niederschlag,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$spei3_spring_hist <- renderPlotly({
      plot_ly(
        data = clim_y,
        x = ~Jahr,
        y = ~SPEI3_Fruehjahr,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$spei3_summer_hist <- renderPlotly({
      plot_ly(
        data = clim_y,
        x = ~Jahr,
        y = ~SPEI3_Sommer,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    dcc_data_beech <- reactive({
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_c <- chron(beech_d)
      selection <- as.numeric(input$beech_months)
      mode <- input$beech_dynamic
      climate_sel <- climate %>% select(c("year", "month", input$beech_dcc_vars))
      if (any(c(input$beech_tstart, input$beech_tend) == "")) {
        timespan <- NULL
      } else {
        tstart <- as.numeric(input$beech_tstart)
        tend <- as.numeric(input$beech_tend)
        timespan <- c(tstart, tend)
      }
      
      winsize <- input$beech_winsize
      list(chrono = beech_c, climate = climate_sel, selection = selection,
           mode = mode, timespan = timespan, winsize = winsize)
    })
    
    output$beech_tcplot <- renderPlot({
      .sel <- dcc_data_beech()$selection
      .dcc <- dcc(dcc_data_beech()$chrono, dcc_data_beech()$climate, selection = .sel, 
                  dynamic = dcc_data_beech()$mode, timespan = dcc_data_beech()$timespan,
                  win_size = dcc_data_beech()$winsize)
      plot(.dcc)
    })
    
    dcc_data_oak <- reactive({
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_c <- chron(oak_d)
      selection <- as.numeric(input$oak_months)
      mode <- input$oak_dynamic
      climate_sel <- climate %>% select(c("year", "month", input$oak_dcc_vars))
      if (any(c(input$oak_tstart, input$oak_tend) == "")) {
        timespan <- NULL
      } else {
        tstart <- as.numeric(input$oak_tstart)
        tend <- as.numeric(input$oak_tend)
        timespan <- c(tstart, tend)
      }
      
      winsize <- input$oak_winsize
      list(chrono = oak_c, climate = climate_sel, selection = selection,
           mode = mode, timespan = timespan, winsize = winsize)
    })
    
    output$oak_tcplot <- renderPlot({
      .sel <- dcc_data_oak()$selection
      .dcc <- dcc(dcc_data_oak()$chrono, dcc_data_oak()$climate, selection = .sel, 
                  dynamic = dcc_data_oak()$mode, timespan = dcc_data_oak()$timespan,
                  win_size = dcc_data_oak()$winsize)
      plot(.dcc)
    })
    
    output$ga_beech <- renderPlotly({
      .sel <- dcc_data_beech()$selection
      .dcc <- dcc(dcc_data_beech()$chrono, dcc_data_beech()$climate, selection = .sel, 
                  dynamic = "static", timespan = dcc_data_beech()$timespan,
                  win_size = dcc_data_beech()$winsize)
      model <- rowSums(scale(.dcc$design$aggregate) * 
                         matrix(rep(.dcc$coef$coef,
                                    nrow(.dcc$design$aggregate)),
                                ncol = ncol(.dcc$design$aggregate),
                                byrow = TRUE))
      d <- data.frame(
        Jahr = rep(as.numeric(names(model)), 2),
        Jahrringindex = c(
          scale(model),
          scale(.dcc$truncated$tree)
        ),
        Daten = rep(c("Modell", "Beobachtungen"), each = length(.dcc$truncated$tree))
      )
      plot_ly(
        data = d,
        x = ~Jahr,
        y = ~Jahrringindex,
        color = ~Daten,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$ga_oak <- renderPlotly({
      .sel <- dcc_data_oak()$selection
      .dcc <- dcc(dcc_data_oak()$chrono, dcc_data_oak()$climate, selection = .sel, 
                  dynamic = "static", timespan = dcc_data_oak()$timespan,
                  win_size = dcc_data_oak()$winsize)
      model <- rowSums(scale(.dcc$design$aggregate) * 
                         matrix(rep(.dcc$coef$coef,
                                    nrow(.dcc$design$aggregate)),
                                ncol = ncol(.dcc$design$aggregate),
                                byrow = TRUE))
      d <- data.frame(
        Jahr = rep(as.numeric(names(model)), 2),
        Jahrringindex = c(
          scale(model),
          scale(.dcc$truncated$tree)
        ),
        Daten = rep(c("Modell", "Beobachtungen"), each = length(.dcc$truncated$tree))
      )
      plot_ly(
        data = d,
        x = ~Jahr,
        y = ~Jahrringindex,
        color = ~Daten,
        type = 'scatter',
        mode = 'lines'
      )
    })
    
    output$scenarios_temp <- renderPlotly({
      plot_data <- lfu %>% 
        group_by(rcp, year, gcm_rcm) %>%
        summarise(Temperatur = mean(temp),
                  Niederschlag = sum(prec)) %>% 
        ungroup() %>% 
        rename(Jahr = year,
               Modell = gcm_rcm) %>% 
        mutate(Modell = as.character(Modell)) %>% 
        filter(rcp == input$scenario_viz_temp, Jahr > 2025) %>%
        arrange(Modell, Jahr) 
      ensemble_mean <- plot_data %>%
        group_by(Jahr) %>%
        summarise(Mittelwert = mean(Temperatur, na.rm = TRUE)) %>% 
        mutate(Modell = "Mittel")
      plot_ly(
        data = plot_data,
        x = ~Jahr,
        y = ~Temperatur,
        color = ~Modell,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 2)
      ) %>% 
      add_trace(
        data = ensemble_mean,
        x = ~Jahr,
        y = ~Mittelwert,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 3, dash = 'dash', color = 'black')
      ) %>% 
        layout(showlegend = FALSE)
    })
    
    output$scenarios_prec <- renderPlotly({
      plot_data <- lfu %>% 
        group_by(rcp, year, gcm_rcm) %>%
        summarise(Temperatur = mean(temp),
                  Niederschlag = sum(prec)) %>% 
        ungroup() %>% 
        rename(Jahr = year,
               Modell = gcm_rcm) %>% 
        mutate(Modell = as.character(Modell)) %>% 
        filter(rcp == input$scenario_viz_prec, Jahr > 2025) %>%
        arrange(Modell, Jahr) 
      ensemble_mean <- plot_data %>%
        group_by(Jahr) %>%
        summarise(Mittelwert = mean(Niederschlag, na.rm = TRUE)) %>% 
        mutate(Modell = "Mittel")
      plot_ly(
        data = plot_data,
        x = ~Jahr,
        y = ~Niederschlag,
        color = ~Modell,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 2)
      ) %>% 
        add_trace(
          data = ensemble_mean,
          x = ~Jahr,
          y = ~Mittelwert,
          type = 'scatter',
          mode = 'lines',
          line = list(width = 3, dash = 'dash', color = 'black')
        ) %>% 
        layout(showlegend = FALSE)
    })
    
    output$vslite <- renderPlotly({
      vs_run_forward <- function(.vs_params, .temp, .prec, .syear, .eyear, .phi, 
                                 return_pretty = TRUE) {
        
        smoothx <- function(x) {
          l <- loess(trw ~ year, x)
          data.frame(year = x$year,
                     trw = predict(l))
        }
        
        out <- VSLite(syear = .syear, eyear = .eyear, phi = .phi, T = .temp, P = .prec,
                      T1 = .vs_params["T1"], T2 = .vs_params["T2"], 
                      M1 = .vs_params["M1"], M2 = .vs_params["M2"])
        if (return_pretty) {
          d <- data.frame(year = .syear:.eyear,
                     trw = t(out$trw))
          smoothx(d)
        }  else out
      }
      
      .params_beech <- c(T1 = input$t1_b, T2 = input$t2_b,
                         M1 = input$m1_b, M2 = input$m2_b)
      
      .params_oak <- c(T1 = input$t1_o, T2 = input$t2_o,
                       M1 = input$m1_o, M2 = input$m2_o)
      
      vs_beech_26 <- vs_run_forward(.params_beech, input_rcp26$temp, input_rcp26$prec,
                                    input_rcp26$syear, input_rcp26$eyear, .phi = 50.28)
      vs_beech_45 <- vs_run_forward(.params_beech, input_rcp45$temp, input_rcp45$prec,
                                    input_rcp45$syear, input_rcp45$eyear, .phi = 50.28)
      vs_beech_85 <- vs_run_forward(.params_beech, input_rcp85$temp, input_rcp85$prec,
                                    input_rcp85$syear, input_rcp85$eyear, .phi = 50.28)
      
      vs_oak_26 <- vs_run_forward(.params_oak, input_rcp26$temp, input_rcp26$prec,
                                    input_rcp26$syear, input_rcp26$eyear, .phi = 50.28)
      vs_oak_45 <- vs_run_forward(.params_oak, input_rcp45$temp, input_rcp45$prec,
                                    input_rcp45$syear, input_rcp45$eyear, .phi = 50.28)
      vs_oak_85 <- vs_run_forward(.params_oak, input_rcp85$temp, input_rcp85$prec,
                                    input_rcp85$syear, input_rcp85$eyear, .phi = 50.28)
      
      d <- data.frame(
        Jahr = c(vs_beech_26$year, vs_beech_45$year, vs_beech_85$year,
                 vs_oak_26$year, vs_oak_45$year, vs_oak_85$year),
        Jahrringindex = c(vs_beech_26$trw, vs_beech_45$trw, vs_beech_85$trw,
                 vs_oak_26$trw, vs_oak_45$trw, vs_oak_85$trw),
        RCP = rep(rep(c("RCP2.6", "RCP4.5", "RCP8.5"), each = length(vs_beech_45$trw)), 2),
        Art = rep(c("Buche", "Eiche"), each = 3 * length(vs_beech_45$trw))
      )
      d %>% 
        filter(RCP == input$vs_rcp) %>% 
        plot_ly(
          data = .,
          x = ~Jahr,
          y = ~Jahrringindex,
          color = ~Art,
          type = 'scatter',
          mode = 'lines'
        )
    })
    
    output$multicol_temp <- renderPlotly({
      cl_wide <- climate %>% 
        select(year, month, tmean) %>% 
        pivot_wider(names_from = month, values_from = tmean) %>% 
        select(-year)
      names(cl_wide) <- month.abb
      cor(cl_wide)
      
      cor_matrix <- cor(cl_wide)
      diag(cor_matrix) <- NA
      
      # Namen für die Achsen
      row_names <- rownames(cor_matrix)
      col_names <- colnames(cor_matrix)
      
      plot_ly(
        x = col_names,
        y = row_names,
        z = cor_matrix,
        type = "heatmap",
        colorscale = "RdBu",
        reversescale = FALSE
      )
        
    })
    
    output$multicol_prec <- renderPlotly({
      cl_wide <- climate %>% 
        select(year, month, prec) %>% 
        pivot_wider(names_from = month, values_from = prec) %>% 
        select(-year)
      names(cl_wide) <- month.abb
      cor(cl_wide)
      
      cor_matrix <- cor(cl_wide)
      diag(cor_matrix) <- NA
      
      # Namen für die Achsen
      row_names <- rownames(cor_matrix)
      col_names <- colnames(cor_matrix)
      
      plot_ly(
        x = col_names,
        y = row_names,
        z = cor_matrix,
        type = "heatmap",
        colorscale = "RdBu",
        reversescale = FALSE
      )
      
    })
    
    observeEvent(input$reset_beech,{
      print(params_beech["T1"])
      updateSliderInput(session, inputId = 't1_b',value = params_beech[["T1"]])
      updateSliderInput(session, inputId = 't2_b',value = params_beech[["T2"]])
      updateSliderInput(session, inputId = 'm1_b',value = params_beech[["M1"]])
      updateSliderInput(session, inputId = 'm2_b',value = params_beech[["M2"]])
    })
    
    observeEvent(input$reset_oak,{
      print(params_beech["T1"])
      updateSliderInput(session, inputId = 't1_o',value = params_oak[["T1"]])
      updateSliderInput(session, inputId = 't2_o',value = params_oak[["T2"]])
      updateSliderInput(session, inputId = 'm1_o',value = params_oak[["M1"]])
      updateSliderInput(session, inputId = 'm2_o',value = params_oak[["M2"]])
    })
    
    output$rt_comp <- renderPlotly({
      
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_d <- tidy_dendro(oak, "Eiche")
      
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_d <- tidy_dendro(beech, "Buche")
      
      both_c <- rbind(beech_d, oak_d) %>% na.omit()
      
      rt_years <- input$rt_years 
      rt_years <- as.numeric(trimws(strsplit(rt_years, ",")[[1]]))
      pre_years <- as.vector(sapply(rt_years, \(x) (x - 1):(x - (input$rt_winsize - 1) / 2)))
      post_years <- as.vector(sapply(rt_years, \(x) (x + 1):(x + (input$rt_winsize - 1) / 2)))
      pre_rwi <- both_c %>% filter(year %in% pre_years) %>% 
        group_by(species, tree) %>% 
        summarise(pre = mean(rwi))
      post_rwi <- both_c %>% filter(year %in% post_years) %>% 
        group_by(species, tree) %>% 
        summarise(post = mean(rwi))
      dr_rwi <- both_c %>% filter(year %in% rt_years) %>% 
        group_by(species, tree) %>% 
        summarise(dr = mean(rwi))
      rt_rwi <- left_join(left_join(pre_rwi, post_rwi), dr_rwi)
      rt_rwi %>% 
        mutate(rt = dr/pre) %>% 
        plot_ly(x = ~species, y = ~rt,
               type = "box", name = "Resistance") %>% 
        layout(yaxis = list(title = "Resistance (Rt)"),
               xaxis = list(title = "Art"))
      
    })
    
    output$rc_comp <- renderPlotly({
      
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_d <- tidy_dendro(oak, "Eiche")
      
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_d <- tidy_dendro(beech, "Buche")
      
      both_c <- rbind(beech_d, oak_d) %>% na.omit()
      
      rc_years <- input$rc_years 
      rc_years <- as.numeric(trimws(strsplit(rc_years, ",")[[1]]))
      
      pre_years <- as.vector(sapply(rc_years, \(x) (x - 1):(x - (input$rc_winsize - 1) / 2)))
      post_years <- as.vector(sapply(rc_years, \(x) (x + 1):(x + (input$rc_winsize - 1) / 2)))
      pre_rwi <- both_c %>% filter(year %in% pre_years) %>% 
        group_by(species, tree) %>% 
        summarise(pre = mean(rwi))
      post_rwi <- both_c %>% filter(year %in% post_years) %>% 
        group_by(species, tree) %>% 
        summarise(post = mean(rwi))
      dr_rwi <- both_c %>% filter(year %in% rc_years) %>% 
        group_by(species, tree) %>% 
        summarise(dr = mean(rwi))
      rc_rwi <- left_join(left_join(pre_rwi, post_rwi), dr_rwi)
      rc_rwi %>% 
        mutate(rc = post/dr) %>% 
        plot_ly(x = ~species, y = ~rc,
                type = "box", name = "Resistance") %>% 
        layout(yaxis = list(title = "Resistance (Rc)"),
               xaxis = list(title = "Art"))
      
    })
    
    output$rs_comp <- renderPlotly({
      
      oak_d <- detrend(oak, method = input$dmethod, nyrs = input$nyrs)
      oak_d <- tidy_dendro(oak, "Eiche")
      
      beech_d <- detrend(beech, method = input$dmethod, nyrs = input$nyrs)
      beech_d <- tidy_dendro(beech, "Buche")
      
      both_c <- rbind(beech_d, oak_d) %>% na.omit()
      
      rs_years <- input$rs_years 
      rs_years <- as.numeric(trimws(strsplit(rs_years, ",")[[1]]))
      pre_years <- as.vector(sapply(rs_years, \(x) (x - 1):(x - (input$rs_winsize - 1) / 2)))
      post_years <- as.vector(sapply(rs_years, \(x) (x + 1):(x + (input$rs_winsize - 1) / 2)))
      pre_rwi <- both_c %>% filter(year %in% pre_years) %>% 
        group_by(species, tree) %>% 
        summarise(pre = mean(rwi))
      post_rwi <- both_c %>% filter(year %in% post_years) %>% 
        group_by(species, tree) %>% 
        summarise(post = mean(rwi))
      dr_rwi <- both_c %>% filter(year %in% rs_years) %>% 
        group_by(species, tree) %>% 
        summarise(dr = mean(rwi))
      rs_rwi <- left_join(left_join(pre_rwi, post_rwi), dr_rwi)
      rs_rwi %>% 
        mutate(rs = post/pre) %>% 
        plot_ly(x = ~species, y = ~rs,
                type = "box", name = "Resilience") %>% 
        layout(yaxis = list(title = "Resilience (Rs)"),
               xaxis = list(title = "Art"))
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
