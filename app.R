

library(shiny)
library(ggplot2)

ui <- fluidPage(
   
  
   titlePanel("Saving Simulations"),
   fluidRow(
     column(4,
            sliderInput("initial",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        value = 1000,
                        step=500),
            sliderInput("annual",
                        "Annual Contribution",
                        min=0,
                        max=50000,
                        value=2000,
                        step=500)
            ),
     
      column(4,
             sliderInput("ret_rate",
                         "Return Rate(in %)",
                         min=0,
                         max=20,
                         step=0.1,
                         value=5
                         ),
             sliderInput("grow_rate",
                         "Growth Rate(in %)",
                         min=0,
                         max=20,
                         step=0.1,
                         value=2)
            ),
     column(4,
            sliderInput("years",
                        "Years",
                        min=0,
                        max=50,
                        step=1,
                        value=20),
            selectInput("facet",
                        "Facet",
                        c("No","Yes"))
            )
     
   ),
   
   h4("Timelines"),
   plotOutput("timelines"),
   
   h4("Balances"),
   verbatimTextOutput("balances"))
   
  

   
#---------------------------------------------------------------------------------------------------
   
server <- function(input,output){
  
# create functions
    #' @title future value function
    #' @description calculate the future value of the investment
    #' @param amount = initial invested amount
    #' @param rate = annual rate of return
    #' @param years = number of years
    #' @return future value
    
    future_value <- function(amount,rate,years){
      fv=amount*(1+rate)^years
      return(fv)
    }
    
    #' @title Future Value of Annuity
    #' @description calculate the future value of annuity
    #' @param contrib: contributed amount
    #' @param rate = annual rate of return
    #' @param years = number of years
    #' @return future value
    annuity <- function(contrib,rate,years){
      fva=contrib*(((1+rate)^years-1)/rate)
      return(fva)
    }
    
    #' @title Future Value of growing annuity
    #' @description calculate the future value of growing annuity
    #' @param contrib: contributed amount
    #' @param rate = annual rate of return
    #' @param growth = annual growth rate
    #' @param years = number of years
    #' @return future value
    
    growing_annuity <- function(contrib,rate,growth,years){
      fvga=contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(fvga)
    }
 
  # reactive 
    modalities <- reactive({
      no_contrib <- rep(0,input$years+1)
      fixed_contrib <- rep(0,input$years+1)
      growing_contrib <- rep(0,input$years+1)
      
    for(i in 1:(input$years+1)){
      if (i==1){
        no_contrib[i]=input$initial
        fixed_contrib[i]=input$initial
        growing_contrib[i]=input$initial
      }else{
        no_contrib[i]=future_value(amount = input$initial, rate = input$ret_rate*10^(-2), years = i-1)
        fixed_contrib[i]=future_value(amount = input$initial, rate = input$ret_rate*10^(-2), years = i-1)+annuity(contrib = input$annual, rate = input$ret_rate*10^(-2), years = i-1)
        growing_contrib[i]=future_value(amount = input$initial, rate =input$ret_rate*10^(-2), years = i-1)+growing_annuity(contrib = input$annual, rate = input$ret_rate*10^(-2), growth = input$grow_rate*10^(-2), years =i-1)
      }
    }
      year=0:input$years
      modalities <- data.frame(year,no_contrib,fixed_contrib,growing_contrib)
      return(modalities)
  })
    
    modalities_facet <- reactive({
      no_contrib <- rep(0,input$years+1)
      fixed_contrib <- rep(0,input$years+1)
      growing_contrib <- rep(0,input$years+1)
      
      for(i in 1:(input$years+1)){
        if (i==1){
          no_contrib[i]=input$initial
          fixed_contrib[i]=input$initial
          growing_contrib[i]=input$initial
        }else{
          no_contrib[i]=future_value(amount = input$initial, rate = input$ret_rate*10^(-2), years = i-1)
          fixed_contrib[i]=future_value(amount = input$initial, rate = input$ret_rate*10^(-2), years = i-1)+annuity(contrib = input$annual, rate = input$ret_rate*10^(-2), years = i-1)
          growing_contrib[i]=future_value(amount = input$initial, rate =input$ret_rate*10^(-2), years = i-1)+growing_annuity(contrib = input$annual, rate = input$ret_rate*10^(-2), growth = input$grow_rate*10^(-2), years =i-1)
        }
      }
      value <- rep(0,3*(input$years+1))
      modalities_facet <- data.frame (
       year=rep(c(0:input$years),3),
       value=c(no_contrib,fixed_contrib,growing_contrib),
       type=factor(c(rep("no_contrib",input$years+1),rep("fixed_contrib",input$years+1),rep("growing_contrib",input$years+1)),levels = c('no_contrib','fixed_contrib','growing_contrib'))
      )
      return(modalities_facet)
    })
   
     
      
  # plot
    output$timelines <- renderPlot({
    if (input$facet=="No") {
    ggplot(data = modalities(), aes(x=year))+
      geom_line(aes(y=no_contrib,color='no_contrib'),linetype=1,size=1)+
      geom_point(aes(y=no_contrib,color='no_contrib'))+
      geom_line(aes(y=fixed_contrib,color='fixed_contrib'),linetype=1,size=1)+
      geom_point(aes(y=fixed_contrib,color='fixed_contrib'))+
      geom_line(aes(y=growing_contrib,color='growing_contrib'),linetype=1,size=1)+
      geom_point(aes(y=growing_contrib,color='growing_contrib'))+
      labs(x="Year",y="Annual Balances",title="Annual Balances of Each Savings-investing Modality")+
      scale_colour_manual("Modality",breaks = c('no_contrib','fixed_contrib','growing_contrib'),values = c('no_contrib'='red','fixed_contrib'='green','growing_contrib'='blue'))
    }else {
        ggplot(data=modalities_facet(),aes(x=year,y=value))+
        geom_line(aes(color = type))+
        geom_point(aes(color = type))+
        geom_area(aes(color = type,fill=type),alpha=0.5)+
        facet_wrap(~type)+
        labs(x="Year",y="Annual Balance",title="Annual Balance of the Different Investing Modes")+
        theme_bw()
    }
    })
    
    output$balances <- renderPrint(modalities())
}  


shinyApp(ui = ui, server = server)

