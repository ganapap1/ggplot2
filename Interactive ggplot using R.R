# Load required Libraries
library(shiny)
library(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
library(ggplot2)           # to draw or take advantage of ggplot functions
library(ggthemes)          # Special ggtheses are there in this eg theme_economist(),theme_stata(): etc
library(DT)                # for using %>% which works as a pipe in R code for filter etc
library(shinyalert)        # for alert message very nice format
library(plotly)            # to prepare interactive graphs/ charts
library(scales)            # date_format: Formatted dates
library(dplyr)             # for data manipulation and filter
library(htmlwidgets)       # to export plot as HTML file
library(shinyWidgets)      # to access all widgets- sliderTextInput 

#Setup-turn off scientific notation like 1e+06
options(scipen=999)


########################################################################################
#function to display numbers in million 
########################################################################################
fnformatMillion <- function(x){
  return(paste(formatC(as.numeric(x)/1000000, format= 'f', digits=2, big.mark= ','),' M'))
}

########################################################################################
#This function is to format percentage, default 2 digit, but you can specify as 0 or 1 or 2 
########################################################################################
fnpercent <- function(x, digits = 2, format = 'f') {      
  paste0(formatC(x * 100, format = format, digits = digits), '%')
}


########################################################################################
# Import R Script from Excel as a data frame and a dummy dataset from CSV
########################################################################################
myrscriptdf <- as.data.frame(readxl::read_excel('C:/R_Projects/GgPlot StepbyStep/RScript in Excel.xlsx'))%>%na.omit()

mydata <- as.data.frame(read.csv(file = 'C:/R_Projects/GgPlot StepbyStep/dataset/mydata.csv',header = TRUE))%>%na.omit()

mydata$Report_dt <-as.Date(mydata$Report_dt)
mydata$Revenue <- as.numeric(mydata$Revenue)
mydata$GP_per <- as.numeric(mydata$GP_per)





ui <- fluidPage(
  id = 'test',  
  
  # Formatting fluidPage with background black and font white
  ############################################################
  tags$style('#test {background-color: #000000;color:#ffffff;}'),         # #where you got:https://stackoverflow.com/questions/58745090/change-background-color-of-fluid-page-in-r-shiny
  useShinyalert(),
  column(width=12,
         column(
           width = 6,
           HTML(paste('<h4><b>',"GG Plot Demo using R Shiny App",'</b><h5>')),
           plotOutput('mggplot',width = '100%',height = '350px')
         ),
         column(
           width = 6,
           HTML(paste('<h4><b>',"R script",'</b><h5>')),
           tags$head(
             tags$style(
               paste0("#mggplotTXT{color:black; font-size:11px; font-style:bold;overflow-y:scroll;
                                            width: '100%';height: 350px;max-height: 350px; background: #ffffcd;text-align: left;}")
             )
           ),
           uiOutput(outputId = 'mscriptortable')
         )
  ),
  column(width=12,
         tags$head(
           tags$style(
             paste0("#mplotselection{color:black; font-size:12px; font-style:bold;padding-left: 5px;  
                                            width: 100%;height: 200px;max-height: 350px; background: #ffffcd;text-align: left;}"),
             paste0("#mthemeselection{color:black; font-size:12px; font-style:bold;padding-left: 5px; 
                                            width: 100%;height: 200px;max-height: 350px; background: #ffffcd;text-align: left;}"),
             paste0("#motheroptions{color:black; font-size:12px; font-style:bold;padding-left: 5px; 
                                            width: 100%;height: 200px;max-height: 350px; background: #ffffcd;text-align: left;}")
             
           )
         ),
         column(
           width = 4,style='border: 1px solid black',
           uiOutput("mplotselection"),
         ),
         column(
           width = 4,style='border: 1px solid black',
           uiOutput("mthemeselection"),
         ),
         column(
           width = 4,style='border: 1px solid black',
           uiOutput("motheroptions"),
         )
  ),
  tags$head(
    tags$style(
      paste0("#mBigsizeggplotTxt{color:black; font-size:18px; font-style:bold;overflow-y:scroll;padding-left: 10px;
                                            width: 100%;height: 550px;max-height: 550px; background: #ffffcd;text-align: left;}")
    )
  ),
  column(width = 12,
         HTML(paste('<h3><b>',"R script with Bigger font",'</b><h5>')),
         uiOutput(outputId =  "mBigsizeggplotTxt")
         
  )
)
    
server<-function(input, output, session) {

  output$mplotselection <- renderUI({
    choice <-   dplyr::filter(myrscriptdf, Grp_Code=='PLOT')$Steps
    checkboxGroupInput("mplotselection","Select Plot (multi selection)", choices = choice, selected = choice[1],width = '100%')
    
  })
  
  output$mthemeselection <- renderUI({ 
    radioButtons("mthemeselection", "Choose one Theme:",width = '100%',
                 choiceNames = 
                   c(dplyr::filter(myrscriptdf, Grp_Code=='THEME')$Steps),
                 choiceValues = c(dplyr::filter(myrscriptdf, Grp_Code=='THEME')$Steps)
    )
  })
  
  
  output$motheroptions <- renderUI({
    choice <-   c(dplyr::filter(myrscriptdf, Grp_Code %in% c('OTHERS'))$Steps)
    checkboxGroupInput("motheroptions","Other Features (multi selection)", choices = choice, selected = choice[1],width = '100%')

  })
    
  output$mscriptortable <- renderUI({
    textAreaInput(inputId ='mggplotTXT',label = NULL,width = '100%',height = '350px',value = NULL )
  })
  
  observeEvent({
    input$mplotselection
    input$mthemeselection
    input$motheroptions},{
      
      # you are supposed to un-check Clear ALL button before you select anything from formatting
      if (c("Clear ALL selection(s)")  %in% c(input$motheroptions)){
        if(length(input$motheroptions)==2){
          shinyalert(title = "Un-check Clear ALL selection(s)!", type = "error",text = 'to enable you to select other options')          
        }
        output$motheroptions <- renderUI({
          choice <-   c(dplyr::filter(myrscriptdf, Grp_Code %in% c('OTHERS'))$Steps)
          checkboxGroupInput("motheroptions","Other Features (multi selection)", choices = choice, selected = choice[1],width = '100%')
        })
      }
      
      #Processing Types Options
      ############################################################
      xx <-filter(myrscriptdf, Steps %in% c(input$mplotselection)) 
      if (nrow(xx)==0){
        output$mplotselection <- renderUI({
          choice <-   dplyr::filter(myrscriptdf, Grp_Code=='PLOT')$Steps
          checkboxGroupInput("mplotselection","Select Plot (multi selection)", choices = choice, selected = choice[1],width = '100%')
        })      
        }
      pp <- paste("ggplot(mydata) + \n\n", paste (c(xx$Action_Code), sep = " ", collapse = " + \n\n "))
      
      
      #Processing Theme Options
      ############################################################
      yy <-filter(myrscriptdf, Steps %in% c(input$mthemeselection))
      pp <- paste(pp," + \n\n ",paste (c(yy$Action_Code), sep = " ", collapse = " + \n\n "))
      
    
      #Processing Other Formatting etc
      ############################################################
      zz <-filter(myrscriptdf, Steps %in% c(input$motheroptions) & Steps !="Clear ALL selection(s)")

      if (nrow(zz)>0){
        pp <- paste(pp," + \n\n ",paste (c(zz$Action_Code), sep = " ", collapse = " + \n\n "))
      }

      # Now you add this modification and proceed to ploting
      pp <- paste("p <-",pp,"\n\n ","p")
      pp <-gsub("\r","",pp)
      
      updateTextAreaInput(session,inputId = 'mggplotTXT',value = pp )
      
      output$mggplot <- renderPlot({
        eval(parse(text = input$mggplotTXT))
      })        
      
   ################################################################
   # Make formatted text box with bigger font for R Script
   ################################################################
        uu1 <- data.frame(filter(myrscriptdf, Steps %in% c(input$mplotselection)))
        uu2 <- data.frame(filter(myrscriptdf, Steps %in% c(input$mthemeselection)))
        uu3 <- data.frame(filter(myrscriptdf, Steps %in% c(input$motheroptions) & Steps !="Clear ALL selection(s)"))
        uu  <- rbind(uu1,uu2,uu3)
        uu  <- uu[order(uu$ID),]%>%na.omit()
        uu  <- uu[,3:4]
      output$mBigsizeggplotTxt <- renderUI({
        ww<- ""
        for (i in 1:nrow(uu)){
          if (i==nrow(uu)){
            ww<-paste(ww,'<h5><b><u>',"#",uu[i,1],'</u></b><br><h5>','\n',uu[i,2],'\n')
          }
          else{
            ww<-paste(ww,'<h5><b><u>',"#",uu[i,1],'</u></b><br><h5>','\n',uu[i,2],"+",'\n')            
          }

        }
        HTML(paste('<h5><b><u>',"#","Initialize ggplot",'</u></b><br><h5>','\n','p <- ggplot(mydata) + \n',ww,'<br><br>','p'))

       })
      
    })
  

}

shinyApp(ui = ui, server = server)



