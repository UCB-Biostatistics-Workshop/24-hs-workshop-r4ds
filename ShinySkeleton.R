### Shiny skeleton for data science workshop

### Install shiny library and any other libraries you'll use in the app
library(shiny)
library(ggplot2)
library(tidyr)
library(shinyWidgets)

### Load data. Make sure directory is correct.
health=read_csv("data/ss.csv")

### Define the UI. 
ui <- fluidPage(
  
    ### Set background color using hex code. alpha.f sets transparency.
    setBackgroundColor(color = adjustcolor("#33FFEC",alpha.f = 0.25)),
    
    
    ### Application title
    titlePanel("My Shiny Skeleton"),

    ### We will put key inputs in the sidebar. Make sure there are commas separating each widget.
    sidebarLayout(
      
        sidebarPanel(
          ### Widgets go here! Widgets will typically show up in order of code. 
            # Basic widget layout. 
            ##  widget("inputname", label=h3("Display Name"),
            ##  choices=list("Choice 1"="inputkey1", "Choice 2"="inputkey2"),
            ##  selected="inputkey1")
            # Widget arguments
            ## "inputname": input name in server
            ## "Display Name": what the UI shows
            ## "Choice 1": Displayed choice in UI
            ## "Inputkey1": Choice name in server
            ## selected="inputkey1": default selection
          
          ### Radio Buttons widget displays choices as buttons to be selected by user
          
          
          radioButtons('box',label=h3("X Variable for Box Plot"),
                       choices=list("Gender"='gender', "Sleep Disorder"='sleep_disorder'),
                       selected='gender'),
          
          ### Select input allows user to choose filtering variable from a drop down menu
          selectInput("gender",label=h3("Gender"),
                      choices=list("Male"='male', "Female"='female', "Both"='both'),
                    selected='both'),
          
          ### p() displays text 
          p("Add line to scatter plot?"),
          ### Switch Input allows us to select Yes or No
          switchInput("reg",onLabel = "Yes", offLabel = "No"),
          
          p("Stratify scatterplot by sleep disorder?"),
          switchInput("strat",onLabel = "Yes", offLabel = "No")
        ),
        

        ### Main panel to show main results (plots in our case). Make sure there are commas separating the separate output and UI functions.
        mainPanel(
          
          ### Basic output layout
            #____Output("outputname")
          
          ### Plots output in UI
          plotOutput("scatter"),
          
          plotOutput("box"),
           
          ### We can change font size with h_ function
          p(h3("Key findings here:")),
          
          p(h5("type findings and interpretations"))
        )
    )
)



### Define the server logic. 
server <- function(input, output) {
  
  ### The reactive function allows us to reactively change our data for use throughout the server. 
      ## In this case, it filters the dataset based on the selected filtering input (gender).
  mydat<-reactive({
    
    ### Calls the input value of gender. First, we filter based on male
    if(input$gender=='male'){
      
      ### Filters dataset
      filtered_dat<-health %>% filter(gender=='Male')

    ### Next, we filter based on female
    }else if(input$gender=='female'){
      filtered_dat<-health %>% filter(gender=='Female')

    ### Finally, if we do not filter at all
    }else{
      filtered_dat<-health
    }

    ### mydat is a function, so we need to return a value. Here we return our filtered dataset
    return(filtered_dat)
  })
    
 
  
  ### Here we define the outputs that will be shown in the main panel of the UI.
      ## Note: since plotOutput() calls "outputID" above in the UI, we want to define the plots as output$outputID
  
  ### To create a plot to show it in the UI, we use the function renderPlot({})
  
  ### Create scatterplot output
  output$scatter <- renderPlot({
          
    ### Create new dataset where we select height and mass from mydat()
    scatdat<-mydat() %>% select(physical_activity_level,sleep_duration, sleep_disorder)
          
    ### Create initial plot of a simple scatterplot
        ## Initialize dataframe and mapping
    plot<-ggplot(data=scatdat, aes(x=physical_activity_level,y=sleep_duration))+
        ## Add scatter plot points
            geom_point()+
        ## Add in labels
            labs(x='Physical Activity Level', y='Sleep Duration')
    
    ### Stratify based on switch. 
    ### Note: the switch's value is stored as TRUE or FALSE instead of "Yes" and "No"      
    if(input$strat==TRUE){
      
      ### Adds stratification and corresponding label
      plot<-plot+aes(color=sleep_disorder)+labs(color="Sleep Disorder")
      
      }
          
    ### Add line based on switch
    if(input$reg==TRUE){
        
        ### Adds line
        plot<-plot+geom_smooth()
        
        }
    
    ### Once plot is finalized, call plot to print in the UI
    plot
    
     })
  
    ### Create box plot output
    output$box<-  renderPlot({
    
    ### Create new dataset based on boxplot values. Use select function to rename input$box to something callable in aes (I am using x).  
    boxdat<-mydat() %>% select(x=input$box, sleep_duration)
    
    ### Create boxplot
        ### Initialize dataframe and mapping
    plot<-ggplot(boxdat, aes(x=x, y=sleep_duration))+
        ### Add boxplot
            geom_boxplot()+
        ### Add labels
            labs(x=input$box)
    
    ### Once plot is finalized, call plot to print in the UI
    plot
    
    })
    

      
}

### Launches app
shinyApp(ui=ui, server=server)



### Input widgets: 

# radioButtons("hist", label = h3("Variable for Histogram"),
#              choices = list("Heart Rate" = 'heart_rate', "Age"='age'),
#              selected = 'heart_rate'),


# radioButtons('cat',label=h3("Variable for Bar Plot"),
#              choices=list("Sleep Disorder"='sleep_disorder',"Occupation"='occupation'),
#              selected='sleep_disorder'),



### Output renders:


# ### Create histogram output
# output$hist<-renderPlot({

#   ### Create new dataset based on histogram input values. Use select function to rename input$hist to something callable in aes (I am using x).  
#   histdat<-mydat() %>% select(x=input$hist)
#   
#   ### Create histogram
#   ### Initialize dataframe and mapping
#   plot<-ggplot(data=histdat,aes(x=x))+
#     ### Add histogram
#     geom_histogram(binwidth=2)+
#     ### Add labels
#     labs(x=input$hist)
#
#   ### Once plot is finalized, call plot to print in the UI
#   plot
#
# })
# 

# ### Create barplot output
# output$bar<-renderPlot({
#
#   ### Create new dataset based on bar plot input values. Use select function to rename input$bar to something callable in aes (I am using x)
#   bardat<-mydat() %>% select(x=input$cat) 
#   
#   ### Create barplot
#   ### Initialize dataframe and mapping
#   plot<-ggplot(data=bardat, aes(x=x))+
#     ### Add bar plot
#     geom_bar()+
#     ### Add labels
#     labs(x=input$cat)
#   
# })
# 
# 

