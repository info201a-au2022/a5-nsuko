#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
my_server <- function(input, output) {
  output$pie_plot <- renderPlotly({
    if(input$rb_chosen == 1)  # radio button
    {return(plot_co2_piechart(input$yr_chosen))} # co2 pie
    else{return(plot_pop_piechart(input$yr_chosen))} # population pie
  })
}

    

