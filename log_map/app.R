### Shiny App for Logistic Map

## Requirements ----

library(shiny)
library(dplyr)
library(plotly)

## UI ----
ui <- fluidPage(
    
    # Style CSS
    tags$style(type = "text/css",
               HTML(
               ".irs-from, .irs-to {
               color: white;
               background: #440154FF
               }
               .irs-bar {
               border-top-color: #440154FF; 
               border-bottom-color: #440154FF;
               }
               .irs-bar-edge {
               border-color: #440154FF;
               }
               .irs-single, .irs-bar-edge, .irs-bar {
               background: #440154FF;
               }")),

    # Title
    titlePanel(NULL),

    # Main content
    sidebarLayout(
        
        # 3 inputs for function: x, a, n
        sidebarPanel(
            tags$p(tags$h2("The Logistic mApp",
                           style = "margin-top: 0em; margin-bottom: 0em")),
            tags$p("The simple system described by the logistic map can generate complex dynamics. Here's the crucial equation:"),
            tags$b(withMathJax(helpText("$$x_{t+1} = ax_{t}(1-x_{t})$$"))),
            tags$p("To understand its properties better we'll compare 3 different simulated populations."),
            tags$p(tags$b("1. Pick starting values for the 3 populations.")),
            tags$ul(tags$li("Some randomly chosen defaults are provided.")),
            splitLayout(numericInput(inputId = "x_1",
                                     label = NULL,
                                     min = 0,
                                     max = 1,
                                     value = round(runif(1), 2),
                                     step = 0.05),
                        numericInput(inputId = "x_2",
                                     label = NULL,
                                     min = 0,
                                     max = 1,
                                     value = round(runif(1), 2),
                                     step = 0.05),
                        numericInput(inputId = "x_3",
                                     label = NULL,
                                     min = 0,
                                     max = 1,
                                     value = round(runif(1), 2),
                                     step = 0.05)),
            tags$p(tags$b("2. Choose how many generations to simulate.")),
            tags$ul(tags$li("This determines how many times we'll interate through the equation.")),
            numericInput(inputId = "n_opts",
                         label = NULL,
                         min = 10,
                         max = 200,
                         value = 100),
            tags$p(tags$b("3. Select a range of values for the parameter", tags$em("a"), ".")),
            tags$ul(tags$li("This determines how funky the dynamics are.")),
            sliderInput(inputId = "a_opts",
                        label = NULL,
                        min = 1,
                        max = 5,
                        value = c(2.5, 4),
                        step = 0.025),
            actionButton(inputId = "go_sim",
                         label = "Simulate!"),
            width = 4
        ),

        # Output interactive plot
        mainPanel(
            tags$h4("After you run your simulation an interactive graph will appear below. The goal of this interactivity is to build non-linear intuitions."),
            tags$p("For a start:"),
            tags$ul(tags$li("Click the play button to see how the simulations of your 3 populations change as the value of", tags$em("a"), "changes."),
                    tags$li("Shift the slider manually to see how all the populations transition from steady state equilibria, to periodic cycles, to chaos."),
                    tags$li("Hover over the lines to see what value a particular population has in a given generation for that value of", tags$em("a"), "."),
                    tags$li("Pick starting values close to one another and see how for some values of", tags$em("a"), "the populations end up looking very different.")),
            plotlyOutput("interactive_plot", width = "100%", height = "100%"),
            width = 8
        )
    )
)

## Server ----
server <- function(input, output) {
    
    ## Define logistic map functions ----
    
    # Single input function
    single_map <- function(start_x, a, n) {
        
        # Empty dataframe
        pop_df <- tibble(generation = 1:n,
                         x = numeric(n))
        
        # Add first generation proportion
        pop_df$x[1] <- start_x
        
        # Simulate n generations
        for (i in 2:n) {
            pop_df$x[i] <- a*pop_df$x[i-1]*(1 - pop_df$x[i-1])
        }
        
        # Final dataframe
        pop_df
    }
    
    # Multiple input function
    multiple_map <- function(x_opts, a_opts, n_opts) {
        
        # Inputs should all be vectors
        
        # Find all possible input permutations
        inputs_df <- expand.grid("start_x" = x_opts,
                                 "a" = a_opts,
                                 "n" = n_opts)
        
        # Blank outcome df 
        outcome_df <- tibble(start_x = numeric(),
                             a = numeric(),
                             n = numeric(),
                             generation = numeric(), 
                             x = numeric())
        
        # Use single input function for all permutations
        for (i in 1:nrow(inputs_df)) {
            
            pop_df <- single_map(start_x = inputs_df$start_x[i],
                                 a = inputs_df$a[i],
                                 n = inputs_df$n[i])
            
            current_inputs <- tibble(start_x = rep(inputs_df$start_x[i],
                                                   inputs_df$n[i]),
                                     a = rep(inputs_df$a[i],
                                             inputs_df$n[i]),
                                     n = rep(inputs_df$n[i],
                                             inputs_df$n[i]))
            
            to_add <- bind_cols(current_inputs, pop_df)
            
            outcome_df <- bind_rows(outcome_df, to_add)
        }
        
        # Convert inputs into factors
        outcome_df <- outcome_df %>%
            mutate(start_x = as.factor(start_x),
                   a = as.factor(a),
                   n = as.factor(n))
        
        outcome_df
    }
    
    ## Use functions with user's inputs ----
    
    # Make reactive inputs
    x_opts <- eventReactive(input$go_sim, {
        c(input$x_1, input$x_2, input$x_3)
    })
    
    a_opts <- eventReactive(input$go_sim, {
        seq(input$a_opts[1], input$a_opts[2], by = 0.1)
    })
    
    n_opts <- eventReactive(input$go_sim, {
        input$n_opts
    })

    plot_df <- reactive(
        multiple_map(x_opts = x_opts(),
                     a_opts = a_opts(),
                     n_opts = n_opts())
    )
    
    ## Output interactive plot ----

    output$interactive_plot <- renderPlotly({
        plot_df() %>%
            plot_ly(x = ~generation,
                    y = ~x,
                    color = ~start_x,
                    colors = c("#39568CFF", "#29AF7FFF", "#FDE725FF"),
                    hoverinfo = "text+name",
                    text = ~paste("Generation:", generation, "<br>",
                                  "Population:", round(x, 4)),
                    frame = ~a) %>%
            add_lines() %>%
            animation_opts(frame = 800,
                           transition = 100,
                           easing = "linear") %>%
            animation_slider(currentvalue = list(prefix = "a = ",
                                                 color = toRGB("black"),
                                                 size = 25),
                             hide = FALSE) %>%
            layout(xaxis = list(title = "Generation"),
                   yaxis = list(title = "Pop. Prop.", range = c(0, 1)))
    }
    )
}

## Run ----
shinyApp(ui = ui, server = server)
