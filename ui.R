shinyUI(fluidPage(
    # download roboto font
    #HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # initiate shinyjs
    #useShinyjs(),
    
    # load custom CSS file
    #includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h1("Estimators, Estimands and Estimations"),
        windowTitle = "Estimators, Estimands and Estimations"
    ),
    
    # overall UI structure
    navbarPage(
        id = "nav",
        title = 'Options:',
        UI_Data, 
        UI_lm,
        UI_iptw,
        UI_rf,
        UI_bart
    )
))