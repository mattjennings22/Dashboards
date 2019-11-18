#BUAN 5210
#Flash Project 2 Shiny App
#Matt Jennings
#November 13 2016

rm(list=ls(all=TRUE))
library(lazyeval)
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shinyjs)

#Setup and data manipulation
setwd('C:/Users/Matt/Documents/GRAD SCHOOL/BUAN 5210/Flash Project 2')
data <- read.csv("Office_Sales_Data.csv")

data$Price <- round(data$Revenue / data$Quantity, 2)
data$Cost <- data$Revenue - data$Profit
data$sign <- ifelse((data$Revenue) - (data$Cost) > 0, "positive", "negative")

profit_graph <- data %>%
  select(Region, Category, Profit) %>%
  group_by(Region, Category) %>%
  summarize(total_profit=sum(Profit))
profit_graph$sign <- ifelse(profit_graph$total_profit >= 0, "positive", "negative")

segment_subcat <- data %>%
  select(Category, Segment, Sub.Category, Quantity, Profit) %>%
  group_by(Category, Sub.Category, Segment) %>%
  summarize(number_of_orders=n(), total_quantity=sum(Quantity), quantity_per_order=(sum(Quantity)/n()), 
            total_profit=sum(Profit), avg_profit=mean(Profit))

subcategorical_share <- segment_subcat
subcategorical_share$profit_per_quantity <- subcategorical_share$total_profit/subcategorical_share$total_quantity
subcategorical_share <- subset(subcategorical_share, Sub.Category=="Accessories"|
                                 Sub.Category=="Phones"|Sub.Category=="Appliances"|Sub.Category=="Chairs")

city_filter <- subset(data, City=="Philadelphia"|City=="Houston"|City=="San Antonio"|
                        City=="New York City"|City=="Los Angeles"|City=="Seattle")

city_filter <- subset(city_filter, Sub.Category=="Binders"|Sub.Category=="Machines"|
                        Sub.Category=="Appliances"|Sub.Category=="Tables")

city_graph <- city_filter %>%
  select(City, Sub.Category, Quantity, Profit, Discount) %>%
  group_by(City, Sub.Category) %>%
  summarize(total_profit=sum(Profit), avg_discount=mean(Discount))
city_graph$sign <- ifelse(city_graph$total_profit >= 0, "positive", "negative")


#UI
body <- dashboardBody(
  fluidRow(
    column(width = 6,
           box(
             title = "Furniture is Least Profitable; Central Region is Lagging", width = NULL, 
             status = "primary", solidHeader = TRUE,
             plotOutput("profit_plot")
           ),
           box(
            width = NULL, status = "primary",
             selectInput("categoryInput", "Product Category",
                          choices = c("Furniture", "Office Supplies", "Technology"),
                          selected = "Furniture")
           )
           
    ),
    column(width = 6,
           box(
             title = "High Discounts Lead to Low Profits", width = NULL, status = "danger", solidHeader = TRUE,
             plotOutput("discount_plot")
           ),
           box(
             width = NULL, status = "danger",
             sliderInput("discountInput", "Discount Level", min = 0, max = .8, value = 0, step = .1)
           )
    )
    
    ),
  fluidRow(
    column(width = 6,
           box(
             title = "High Frequency of Discounts Lowers Profits for Some Cities; Tables Are a Profit Drain", 
             width = NULL, status = "warning", solidHeader = TRUE,
             plotOutput("city_plot")
           ),
           box(
             width = NULL, status = "warning",
             radioButtons("subcatInput", "Product Subcategory",
                          choices = c("Binders", "Machines", "Appliances","Tables"),
                          selected = "Binders", inline = TRUE)
             
           )
    ),
    column(width = 6,
           box(
             title = "Home Office Customers Generate Higher Profits", width = NULL, status = "success", 
             solidHeader = TRUE,
             plotOutput("segment_plot")
           ),
           tabBox(width = NULL,
                  title = strong("Most Profitable"),
                  side = "right",
                  selected = "Region",
                  tabPanel("City",
                           strong("New York City: $62,037")),
                  tabPanel("Subcategory", 
                           strong("Copiers: $55,618")),
                  tabPanel("Region", 
                           strong("West: $108,418"))
           )
    )

  
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard: Supporting Strategic Changes to Increase Profits", titleWidth = 700),
  dashboardSidebar(disable = TRUE),
  body
)

#Server
server <- function(input, output, session) {
  output$profit_plot <- renderPlot({
    category_filter <- profit_graph %>%
      filter(Category == input$categoryInput)

    ggplot(category_filter, aes(x = Region, y = total_profit, fill = sign)) +
      scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"), guide = FALSE) +
      geom_bar(stat = "identity") +
      ylim(-5000, 60000) +
      ylab("Total Profit ($)")
  })
  output$discount_plot <- renderPlot({
    discount_filter <- data %>%
      filter(Discount == input$discountInput)
    
    ggplot(discount_filter, aes(x=Cost, y=Revenue)) +
      scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
      geom_point(aes(color = sign)) +
      guides(color = "none") +
      geom_abline(slope = 1, linetype=3) +
      ylim(0,5000) +
      xlim(0,5000) +
      annotate("text", x = 1000, y = 4500, label = "High Profit", size = 4) +
      annotate("text", x = 3500, y = 500, label = "Negative Profit (Loss)")
  
    })
  output$segment_plot <- renderPlot({
    ggplot(subcategorical_share, aes(x=reorder(Sub.Category, profit_per_quantity), y=profit_per_quantity, 
                                     fill = Segment)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      ylab("Profit per Unit Sold ($)") +
      xlab("Subcategory") +
      theme(legend.position = c(0.9,0.1)) +
      guides(fill=guide_legend(reverse=TRUE), color=guide_legend(reverse=TRUE))
  })
  
  output$city_plot <- renderPlot({
    city_slider <- city_graph %>%
      filter(Sub.Category == input$subcatInput)
    city_slider <- city_slider[order(city_slider$Sub.Category, city_slider$total_profit),]
    
    
    ggplot(city_slider, aes(x=total_profit, y=City)) +
      geom_point(aes(color = sign), size=4) + 
      scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"), guide = FALSE) +
      guides(color = FALSE) +
      geom_vline(xintercept=0) +
      geom_segment(aes(yend=City, color= sign), xend=0, size=2) +
      ylab("City") +
      scale_y_discrete(limits=c("Houston","San Antonio","Philadelphia","Los Angeles","Seattle","New York City"))+
      xlab("Total Profit ($)") +
      xlim(-7000,15000) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
  })

} 

#App
shinyApp(ui = ui, server = server)