UI_Data <- tabPanel(title = "Draw a Sample",
sidebarLayout(
 sidebarPanel(
   width = 4,
   h3("Describe Data"),
   h5("How do you want your data to look?"),
   br(),
   sliderInput(inputId = 'number',
               label = 'Sample Size',
               min = 100,
               max = 1000,
               value = 500,
               step = 50,
               animate = animationOptions(interval = 400, loop = FALSE)),
   selectInput(
     inputId ="linear",
     label = "Linear",
     multiple = FALSE,
     choices = c("Yes", "No"),
     selected = "Yes"
   ),
   selectInput(
     inputId ="common_support",
     label = "Common Support",
     multiple = FALSE,
     choices = c("Strong", "Moderate", "Weak"),
     selected = "Strong"
   ),
   
   # h5("Now select which datapoints to include in the treatment group by clicking on points in the plot."),
   # h5("How do the univariate densities compare between treatment and control?"),
   # h5("Now randomize the selections using the below button. How do the densities compare now?"),
   # br(),
   actionButton(inputId = 'dgp_button',
                label = "Resample"),
   
   actionButton(inputId = 'fit_button',
                label = "Fit Model")
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
   plotOutput('data_draw_plot',
              click = "randomization_plot_click"),
   
 #   tabsetPanel(
 #      id = 'model_tabs',
 #      type = 'tabs',
 #      tabPanel("Sampled Data",
 #               br(),
 #               plotOutput("data_draw_plot", height = 600)),
 #      tabPanel("Linear Regression",
 #               br(),
 #               plotOutput("lm_plot", height = 600)),
 #      tabPanel("IPTW",
 #               br(),
 #               plotOutput("iptw_plot", height = 600)),
 #      tabPanel("BART",
 #               br(),
 #               plotOutput("lm_plot", height = 600))
 # 
 # )
)))



