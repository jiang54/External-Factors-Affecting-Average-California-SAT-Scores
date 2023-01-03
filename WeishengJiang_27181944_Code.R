#import library
library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(corrplot)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(broom)

#set up ui
ui <-fluidPage(
  #initialize the shiny dashboard, set the background color
    useShinydashboard(),setBackgroundColor(color = "#fffbf3"),
    # set up the title
    titlePanel(h1("External Factors Affecting Average California SAT Scores",align='center',style="color:red")),
    sidebarLayout(
    #side panel of layout bubble map
      sidebarPanel(fluidRow(box(solidHeader = TRUE,status = "warning",collapsible = TRUE,width = 14,
        h3("Introduction",align='center'),
        h5("It is well known that different environments can have an impact on students. Our aim this time is to find the relationship between each external factor and SAT scores of California students through exploration and visualization in the given data.(SAT is the American College Entrance Examination)"),
        h3("Meaning of Factors",align='center'),
        h5("Grade: grade level. county: counties in California. calwork:Percent qualifying for CalWorks (income assistance). lunch:Percent qualifying for reduced-price lunch.
           computer:Number of computers. expenditure: Expenditure per student. income: District average income (in USD 1,000). english: Percent of English learners. read: Average reading score. math: Average math score.
           average_score: mean of read and math score"),
        h3("Question Explored by Visualization",align='center'),HTML("1. Can you display the distribution of scores of different counties in different grades? <br>2. For variables calworks,lunch, computer, expenditure, income and english, which has higher effects on the scores?"),
        h3("Bubble Map of Grades",align='center'),
        HTML("It is the Bubble Map of Grades of Different Counties in California. <br>It can be seen that KK-06 is mostly in the coastal area. <br>The distribution of KK-08 covers the whole of California, and there are more in the central and upper part of California."),
        h3("Checkbox of Grade",align='center'),
        h5("Allow users to change the Grades"),
        h3("Checkbox of County",align='center'),
        h5("Allow users to change the Counties"),))),
      mainPanel(
        #main panel of layout bubble map
        h4("Student ID:27181944, Name:Weisheng Jiang, Tutors: Mohit Gupta, Vaibhavi Bhardwaj"),fluidRow(
        box(solidHeader = TRUE,status = "info",collapsible = TRUE,title ="Selected Counties",verbatimTextOutput("value")),
        box(solidHeader = TRUE,status = "danger",collapsible = TRUE,title ="Grade",checkboxGroupInput("grades","Select grades:",c("KK-06","KK-08"),selected = c("KK-06","KK-08"))),
        box(solidHeader = TRUE,status = "danger",collapsible = TRUE,title ="Bubble Map of Grades of Different Counties in California",leafletOutput('Map')),
        box(solidHeader = TRUE,status = "primary",collapsible = TRUE,title ="Select the Counties You Wish To See",uiOutput("campSelector"))),
      )
    )
    ,
    sidebarLayout(
      #side panel of bubble chart and percentage stacked bar chart
      sidebarPanel(fluidRow(box(solidHeader = TRUE,status = "info",collapsible = TRUE,width = 14,
        h3("Select bar of X-axis",align='center'),
        h5("Allow users to change the X-axis"),
        h3("Select bar of Y-axis",align='center'),
        h5("Allow users to change the Y-axis"),
        h3("Bubble Chart of Y-Axis on X-Axis",align='center'),
        HTML("The bubble chart shows the relationship between two variables.<br>When Y-axis is average score, it can be seen that calworks, lunch, english have a strong negative relationship with average score
             <br>Income has a strong positve relationship with average score.<br>Computer has no relationship with average score.<br>Expenditure have a weak positive relationship with average score."),
        h3("Percentage stacked Bar Chart"),
        HTML("The percentage is divided into 5 levels, each present 20%.<br>When mouse on the bar, it shows the amount and percentage for this county.<br> When Y-axis is average score,it shows that Alameda have the highest percent of very high average score with 1 unit. Mateo have 5 units of very high average score, but only have 29% inside San Mateo."),
      ))),
      #main panel of bubble chart and percentage stacked bar chart
      mainPanel(fluidRow(
        box(solidHeader = TRUE,status = "primary",collapsible = TRUE,uiOutput("campSelector2"),title = "Factor Affecting (Please Ensure X and Y different Selection)"),
        box(solidHeader = TRUE,status = "primary",collapsible = TRUE,uiOutput("campSelector3"),title = "Factor Affected (Please Ensure X and Y different Selection)"),
        box(solidHeader = TRUE,status = "warning",collapsible = TRUE,title ="Bubble Chart of Y-Axis on X-Axis",plotlyOutput("bubble_chart")),
        box(solidHeader = TRUE,status = "info",collapsible = TRUE,title = "Percentage Stacked Bar Chart of Y-Axis in Different Counties ",plotlyOutput("bar_chart"))))),
      sidebarLayout(
        #side panel of correlation matrix
        sidebarPanel(fluidRow(box(solidHeader = TRUE,status = "primary",collapsible = TRUE,width = 14,
          h3("Correlation Matrix",align='center'),
         HTML("This matrix shows the correlations between each variable.<br> Based on our question, we will focus on the last row and last column.<br>
              It shows the correlationship of age score and othe external features.<br>
              From the graph and number it can be seem that, average score have a strong negative correlationship with calwork, lunch, english.<br>
              And average score have a positive correlationship with income.<br>
              Average score have no unclear correlationship with computer in weak negative side, and weak positive correlationship with expenditure."),
         ))),
        #main panel of correlation matrix
        mainPanel(fluidRow(
        box(solidHeader = TRUE,status = "success",collapsible = TRUE,title ="Correlation Matrix of Different features for All/Specific Counties and Grades",plotOutput("confusion_matrix")),
        box(HTML("For question 1, the distribution is shown in the bubble map format.KK-06 is concentrated on the coast. KK-08 covers a wide area in California.<br>
                 For question 2, the lunch has the highest negative effects on average score. The income has the highest positive effect on average score.<br>
                 Positive factor: strong: income, weak: Expenditure <br>
                 Negative factor: strong: calwork, lunch, english<br>
                 Minimal or no effect: computer
                 "),solidHeader = TRUE,collapsible = TRUE,title ="Conclusion",status = "danger")
      )
    )
    ) #end of layout correlation matrix
  )# end of ui



# set up server
server <- function(input, output) {
  #load the map data set
  df = read.csv("map_data.csv")
  #operation of layout 1 bubble map
  county = unique(df$county)
  output$campSelector <- renderUI({
    checkboxGroupInput('sel_counties','County:',c("All",county),inline = TRUE,selected ="All")
    })
  #print the selected counties
  output$value <- renderText({input$sel_counties})
      output$Map = renderLeaflet({
        #filter the county
        if("All" %in% input$sel_counties ){
            dataset_1 = df
        }else{
          dataset_1 = df[df$county%in%input$sel_counties,]
        }
        #filter the grades
        dataset_1 = dataset_1[dataset_1$grades%in%input$grades,]
        #create the leaflet map
        m = leaflet(dataset_1,width = "300px") %>% setView( -119.417931,36.778259, zoom = 5) %>% addTiles()
        pal = colorFactor(palette = 'Set1', domain =  df$grades)
        # add markers / bubbles
        m %>% addCircleMarkers(lng = ~lng, lat = ~lat,
                         label = ~county,color = ~pal(grades),radius = ~(normalized_average_score*18),fillOpacity = 0.6,opacity = 0.8,
                         popup = ~paste("County:",county,",average score:",average_score,",loc:",lat,lng)) %>% 
                         leaflet::addLegend(position = "bottomright",pal = pal, values =~grades,opacity = 0.5, title = "Grade")
      })
      #load the dataset for layout 2 and layout 3
      df2 = read.csv("CASchools_formatted.csv")
      #operation of layout 2 bubble chart, percentage stacked bar chart
      #select the external factors
      effect_factor = names(df2[5:13])
      #select bar for x
    output$campSelector2 <- renderUI({
      selectInput("x_axis",'X-axis:',effect_factor,selected = NULL)
    })
    #select bar for y
    output$campSelector3 <- renderUI({
      selectInput("y_axis",'Y-axis:',effect_factor,selected = "average_score")
    })
    #operation of bubble chart
    output$bubble_chart = renderPlotly({
      if("All" %in% input$sel_counties ){
        #filter county
        dataset_2 = df2
      }else{
        dataset_2 = df2[df2$county%in%input$sel_counties,]
      }
      #filter the grades
      dataset_2 = dataset_2[dataset_2$grades%in%input$grades,]
      #select x y based on select bar
      temp_df2 = select(dataset_2,"county",input$x_axis,input$y_axis)
      colnames(temp_df2) = c("county","x","y")

      # get he Local Polynomial Regression Fitting
      m <- loess(y ~ x, data = temp_df2)
      temp_m = augment(m)
      #add the county column
      temp_m$county = "All"
      #compute Standard Error theta
      mse = sqrt(sum((augment(m)$.fitted- mean(augment(m)$.fitted))^2)/length(augment(m)$.fitted))
      #plot of bubble chart
      fig <- plot_ly(temp_df2, x = ~x,color = ~county)%>% 
        add_markers(y = ~y, text = rownames(~county)) 
      #add the overall trend line
      fig <-  add_lines(fig,data = temp_m,y = ~.fitted,ymin = ~.fitted,ymax = ~.fitted,
                        line = list(color = 'rgba(115, 171, 255, 1)'))%>% 
        #add the Standard Error
        add_ribbons(data = temp_m,
                    ymin = ~.fitted - mse,
                    ymax = ~.fitted + mse,
                    line = list(color = 'rgba(7, 164, 181, 0.1)'),
                    fillcolor = 'rgba(7, 164, 181, 0.1)',
                    name = "Standard Error")
      #change label and legend position
      fig <- fig %>% layout(xaxis = list(title = input$x_axis),
                            yaxis = list(title = input$y_axis),
                            legend = list(xanchor = "center",font = list(size = 10)))
      
      fig
      
      
    })
    #define min max normalize function
    normalize <- function(x, na.rm = TRUE) {
      return((x- min(x)) /(max(x)-min(x)))
    }
    output$bar_chart = renderPlotly({
      #filter county
      if("All" %in% input$sel_counties ){
        dataset_2 = df2
      }else{
        dataset_2 = df2[df2$county%in%input$sel_counties,]
      }
      #filter grades
      dataset_2 = dataset_2[dataset_2$grades%in%input$grades,]
      #select county, x, y based on select bar. btw, x is not really used in bar chart, since x always counties
      temp_df2 = select(dataset_2,"county",input$x_axis,input$y_axis)
      #rename as x and y
      colnames(temp_df2) = c("county","x","y")
      #scaled 
      temp_df2$scaled_v = normalize(temp_df2$y)
      #define condition column based on the scaled value
      temp_df2 = mutate(temp_df2,level = case_when(scaled_v>=0.8 ~ 'Very High',scaled_v>=0.6 ~ 'High',scaled_v>=0.4 ~ 'Mid',scaled_v>=0.2~'Low',scaled_v>=0~'Very Low'))
      #create total order/amount column
      temp_df3 =temp_df2 %>% select(county,level) %>% group_by(county,level) %>% summarise((Total_orders = n()), .groups = 'drop')
      colnames(temp_df3) = c("county","level","Amount")
      #compute and create percentage column
      temp_df3 = temp_df3%>% group_by(county) %>% mutate(Percentage = Amount/sum(Amount))
      #used for define level order in the legend
      temp_df3$level <- factor(temp_df3$level, c("Very Low", "Low", "Mid","High","Very High"))
      #plot percentage stacked bar chart
      plot_ly(temp_df3,x = ~county, y = ~Percentage*100, type = 'bar',color = ~level,text=~paste(county,Amount,round(Percentage,2)),hoverinfo = 'text') %>% 
        layout(yaxis = list(title =paste( input$y_axis,'Percentage (%)')),barmode = "stack")
      
    })
    #operation of layout 3 correlation matrix
    output$confusion_matrix = renderPlot({
      #filter the county
      if("All" %in% input$sel_counties ){
        dataset_2 = df2
      }else{
        dataset_2 = df2[df2$county%in%input$sel_counties,]
      }
      #filter the grades
      dataset_2 = dataset_2[dataset_2$grades%in%input$grades,]
      #select the external factors
      temp_df2_1 = dataset_2 %>% select(5:13)
      #create correlation matrix
      M <- cor(temp_df2_1)
      #define color, used for pie chart and legend
      col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "#FCE903", "#FFFF00",
                                 "cyan", "#007FFF", "blue", "#00007F"))
      # plot correlation matrix
      corrplot.mixed(M, lower.col = col1(10),upper = "pie", tl.col = "black",upper.col = col1(10))
      
    })
}# end of server

# run the shiny 
shinyApp(ui, server)


