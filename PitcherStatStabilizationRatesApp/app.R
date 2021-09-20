library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(readr)
library(reshape2)

#load in the dataframes that will be used for the app
#these files are csv versions of the PA_alphas and Pitch_alphas variables created in the "Calculate Stabilization Rates for Pitcher Statistics" file
PerPAStatsAlphas <- read_csv("PerPAStatsAlphas.csv",
                             col_names = c('Sample Size', 'K%', 'Pitches Per PA'),
                             skip = 1)
PerPitchStatsAlphas <- read_csv("PerPitchStatsAlphas.csv",
                                col_names = c('Sample Size', 'swStr%', 'cStr%',
                                              'Foul%', 'Ball%', 'InPlay%', 'HBP%'),
                                skip = 1)

ui <- fluidPage(
    
    #set the CSS style code for the app
    tags$head(
        
        tags$style(HTML("
      .shiny-output-error-validation {
        color: blue;
        font-size: 30px;
        font-family: georgia;
      }
    
    #dashboard {
      margin-bottom: 100px;
    }  
      
    #title {
      color: darkblue;
      font-size: 50px;
      font-style: bold;
      font-family: tacoma;
    }
                              
     #hyperlink {
      color: blue;
      font-style: bold;
     }
                              
    body {
        background-color: lightblue;
      }"))
    ),
    
    #writes code for the description at the top of the app
    #explains what the app is used for and what each of the stats refer to
    h1(id = 'title', "Stabilization Rates for Pitcher Stats"),
    p(id = 'description1', "This app plots the ", tags$b('stabilization rate'), " of various pitcher statistics 
    in Major League Baseball. A statistic's stabilization rate refers to how long it takes 
    for us to trust a pitcher's performance is indicative of their true skill level, rather 
    than good fortune. A more comprehensive explanation of stabilization rates can be found ",
      tags$a(id = 'hyperlink', href="https://blogs.fangraphs.com/a-long-needed-update-on-reliability/", "at this Fangraphs article. ")),
    p(id = 'description2', "This app uses the same methodology as the above article for calculating 
    stabilization rates, reproducing the study over a larger sample, from 2008 to 2019. 
    To find the stabilization rate, we calculate a particular statistic's ", tags$b('alpha level'),
      " at various sample sizes. A description of alpha levels can be found ",
      tags$a(id = 'hyperlink', href="https://blogs.fangraphs.com/a-new-way-to-look-at-sample-size-math-supplement/", "at this article. ")),
    p(id = 'description3', "As a basic definition, a statistic's alpha level represents the 
    square root of the amount a statistic explains a pitcher's true skill level. For example, 
    if strikeout rate has an alpha level of 0.5 for a given sample size, it explains 
    0.5 * 0.5 = 25% of a pitcher's true skill, with the other 75% being explained by 
    random variation around the league average. Typically, a statistic can be said to 
    stabilize at an alpha level of 0.7, which is the point that 50% of a pitcher's performance 
    is reflected by their skill (the threshold of 0.7 is not universally used, and 
    others may choose a different value). ", tags$b('Note: the value 0.7 is represented on graphs in this app by 
    a horizontal black line.')),
    p(id = 'description4', "This app focuses on the following pitcher statistics: "),
    p(id = 'description5', tags$u("Per-Plate Appearance Stats")),
    
    tags$ul(
        tags$li(tags$b("K%"), " - the percent of a pitcher's plate appearances that end in a strikeout."),
        tags$li(tags$b("Pitches Per PA"), " - the average number of pitches a pitcher throws in each plate appearance.")
    ),
    
    p(id = 'description6', tags$u("Per-Pitch Stats")),
    
    tags$ul(
        tags$li(tags$b("swStr%"), " - the percent of a pitcher's pitches that end with a swing and miss."),
        tags$li(tags$b("cStr%"), " - the percent of a pitcher's pitches that end with a called strike."),
        tags$li(tags$b("Foul%"), " - the percent of a pitcher's pitches that end with a foul."),
        tags$li(tags$b("Ball%"), " - the percent of a pitcher's pitches that end with a ball."),
        tags$li(tags$b("InPlay%"), " - the percent of a pitcher's pitches that are put in play by the opposing hitter."),
        tags$li(tags$b("HBP%"), " - the percent of a pitcher's pitches that end with a hit by pitch.")
    ),
    
    #creates the sidebar that allows the user to select stats that will appear in the plot
    sidebarLayout(
        sidebarPanel(
            h4("Plot Output"),
            selectInput("variable", NULL,
                        c("Per-Plate Appearance Stats" = "PerPAStatsAlphas",
                          "Per-Pitch Stats" = "PerPitchStatsAlphas")),
            checkboxGroupInput(inputId = 'statBox',
                               label = 'Stats Plotted:',
                               choices = c(1))
        ),
        mainPanel(id = 'dashboard',
                  
                  plotlyOutput("stabPlot")
                  
        )
        
    )
    
)

server <- function(input, output, session) {
    
    #adjusts the checkbox options for the user to select 
    #this is based on the type of stat they choose (either Per-PA Stats or Per-Pitch stats)
    observeEvent(input$variable, {
        updateCheckboxGroupInput(session,
                                 inputId = 'statBox',
                                 choices = names(get(input$variable))[-1])
    })
    
    #creates a dataframe that includes the variables the user selected
    #based on the variables they selected in the checkbox
    df <- reactive({
        
        if(!is.null(input$variable)){
            melt(get(input$variable) %>% select(`Sample Size`, input$statBox),  
                 id.vars = 'Sample Size')
        }
    })
    
    #Creates an interactive plot from the dataframe
    output$stabPlot <- renderPlotly({
        
        validate(
            need(df()$variable, 
                 "Click on a stat to the left under 'Stats Plotted' to add it to a stabilization plot.", 
                 label = 'errorMessage')
        )
        
        alphasPlot <- df() %>% ggplot(aes(x = `Sample Size`, y = value)) +
            geom_line(aes(colour = variable,
                          group = 1,
                          text = paste("Sample Size: ", `Sample Size`, "<br>Alpha Value: ",
                                       round(value, 3), "<br>Statistic: ", variable))) +
            labs(x = 'Sample Size',
                 y = 'Alpha Level') +
            geom_hline(yintercept = 0.7) +
            theme(legend.background = element_rect(fill="skyblue", size=0.5, linetype="solid"),
                  legend.title = element_blank())
        
        ggplotly(alphasPlot, tooltip = "text")
        
    })
}

#run the app
shinyApp(ui, server)