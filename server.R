library(shiny)
library(ggplot2)
library(ggthemes)

data2<-read.csv("./data/Consolidated.csv")
data2[,4] <-as.numeric(levels(data2[,4]))[data2[,4]]
cdb <- read.csv("./data/Codebook.csv",colClasses = "character")

shinyServer(
  function(input, output){
    output$tablE2 <- renderTable({
      data2[(data2$Country==input$cnt3) & (data2$Index==input$edi3),]
    })
    output$des <- renderText({
      cdb[cdb$Index==input$edi,2]
    })
    
    #Subset the data
    data3 <- reactive({
      data2[(data2$Year >= input$range[1]) & (data2$Year <= input$range[2]) & (data2$Country == input$cnt) & (data2$Index == input$edi),]
    })
    data4 <- reactive({
      data2[(data2$Year >= input$range[1]) & (data2$Year <= input$range[2]) & (data2$Country == input$cnt2) & (data2$Index == input$edi),]
    })
    data7 <- reactive({
      data2[(data2$Year==input$snapshot) & (data2$Index == input$edi),]
      # data2[data2$Index == input$edi,]
    })
    data8 <- reactive({
      data2[(data2$Year==input$snapshot) & (data2$Index == input$edi2),]
      # data2[data2$Index == input$edi2,]
    })
    data9 <- reactive({
      # merge(data7(),data8(),by=c("Country","Year"))
      merge(data7(),data8(),by="Country")
    })
    
    fit1 <- reactive({
      lm(value~Year,data=data3())
    })
    fit2 <- reactive({
      lm(value~Year,data=data4())
    })
    fit3 <- reactive({
      lm(value.y~value.x,data=data9())
    })
    

    #Create ploT
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$ploT <- renderPlot({
      data5 <- rbind(data3(),data4())
      ggplot(data5,aes(y=value,x=Year,colour=Country))+
        geom_point()+geom_smooth(method='lm')+
        xlab("year")+
        ylab(input$edi)+
        # theme(plot.background=element_rect(fill="firebrick"))
        theme_stata()+scale_colour_stata()+
        ggtitle(paste(input$edi, " for " , input$cnt," and ", input$cnt2,sep=""))+
        scale_x_continuous(limits=input$range)
    }) 
    
    #Create plOt
    output$plOt <- renderPlot({
      ggplot(data9(),aes(y=value.y,x=value.x))+
        geom_point()+
        geom_smooth(method="lm")+
        xlab(input$edi)+
        ylab(input$edi2)+
        # theme(plot.background=element_rect(fill="firebrick"))
        theme_stata()+scale_colour_stata()+
        ggtitle(paste(input$edi2, " against " , input$edi," for ", input$snapshot,sep=""))
    })
    
    #Create Data Table
    output$tablE <- renderTable({
      data6 <- data.frame(Year=data3()$Year,Selected=data3()$value, Reference=data4()$value)
    })
    #Inferential statistics
    output$llma <- renderText({
      paste(paste(input$edi," for ",input$cnt,"\n= "), paste(round(coef(fit1())[2],4)," (Year)"," + ",round(coef(fit1())[1],2),"\n\n(R-squared =",round(summary(fit1())$r.squared,3),")", sep = ""))
    })
    output$llmb <- renderText({
      paste(paste(input$edi," for ",input$cnt2,"\n= "), paste(round(coef(fit2())[2],4)," (Year)"," + ",round(coef(fit2())[1],2),"\n\n(R-squared =",round(summary(fit2())$r.squared,3),")",  sep = ""))
    })
    
    #Dynamic interaction info
    output$hover <- renderText({
      paste("Mouse hover data:","\nx=", round(input$ploT_hover$x,0), "\ny=",round(input$ploT_hover$y,4))
    })
    output$click <- renderText({
      paste("Mouse click data:","\nx=", round(input$ploT_click$x,0), "\ny=",round(input$ploT_click$y,4))
    })
    output$dblclick <- renderText({
      paste("Mouse double-click data:","\nx=", round(input$ploT_dblclick$x,0), "\ny=",round(input$ploT_dblclick$y,4))
    })
    
    output$bflgrad <- renderText({
      paste("line between click and double-click data is ", "\n=" ,(input$ploT_click$y - input$ploT_dblclick$y) / (input$ploT_click$x - input$ploT_dblclick$x)," (Year)", " + ",input$ploT_dblclick$y-input$ploT_dblclick$x*((input$ploT_click$y - input$ploT_dblclick$y) / (input$ploT_click$x - input$ploT_dblclick$x)))
    })
    output$llmc <- renderText({
      paste(paste("Best-fit-line for ",input$edi2, " against ",input$edi,"\n "),input$edi2," = ", paste(round(coef(fit3())[2],4)," (",input$edi," )"," + ",round(coef(fit3())[1],2),"\n\n(R-squared =",round(summary(fit3())$r.squared,3),")",  sep = ""))
    })
    output$brush_info <- renderPrint({
      brushedPoints(data9(),input$plOt_brush)
    })
    
})
