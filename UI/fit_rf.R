UI_rf <- tabPanel(title = "Propensity Score Matching (Random Forest)",
                  sidebarLayout(
                    sidebarPanel(
                      width = 4,
                      h3("Assumptions:"),
                      h5("1. Ignorability"),
                      h5("2. SUTVA"),
                      h5("3. Proper Specificaton of Propensity Scores"),
                      h5("4. Balalance"),
                      h5("5. Overlap"),
                      br(),
                      # selectInput(
                      #   inputId ="RF",
                      #   label = "Modeling Options:",
                      #   multiple = FALSE,
                      #   choices = c("","Linear Regression", "IPTW", "BART")
                      # ),
                      
                      
                      # h5("Now select which datapoints to include in the treatment group by clicking on points in the plot."),
                      # h5("How do the univariate densities compare between treatment and control?"),
                      # h5("Now randomize the selections using the below button. How do the densities compare now?"),
                      # br(),
                      # actionButton(inputId = 'fit_button',
                      #              label = "Fit Model"),
                      # br(), br(),
                      # actionButton(inputId = 'randomize_reset_button',
                      #              label = "Reset the treatment assignment"),
                      # br(), br(),
                      # HTML('<details><summary>What is this data?</summary>'),
                      # HTML('<br>The data are simulated. The values and correlations are reasonable but please do not make any material conclusions from the data.</a'),
                      # HTML('</details><br>')
                      
                    ),
                    mainPanel(
                      width = 6,
                      plotOutput('rf_plot',
                                 click = "model_plot_click"),
                      # br(),
                      # plotOutput('data_draw_plot', height = 500),
                      # absolutePanel(id = "randomization_floating_box", 
                      #               class = "floating_message",
                      #               top = 50, left = "auto", right = 50, bottom = "auto",
                      #               width = "30%", height = "auto", draggable = FALSE,
                      #               "Click on points to assign them to treatment!")
                    )
                  ))