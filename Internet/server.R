#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidycensus)

vars<- c("B28002_001","B28002_013","S1701_C03_002","DP05_0018","DP05_0077P","DP05_0078P","DP05_0079P","DP05_0080P","DP05_0071P")
nms<- c("TOTPOP","NOINT","POVPCT","MEDAGE","WHT","BLK","IND","ASN","HSP")

county18<-get_acs("county",survey = "acs1", key = "993d02931a941bd1f5af535c637800916aa504b2",variables=vars)

cty18_wide<-data.frame(county18$GEOID[county18$variable==vars[1]],
                       county18$NAME[county18$variable==vars[1]])
for(i in 1:9){
  cty18_wide[i+2]<-county18$estimate[county18$variable==vars[i]]}


names(cty18_wide)<-c("GEOID","NAME",nms)

cty18_wide$INTPCT<- with(cty18_wide,round((1-(NOINT/TOTPOP))*100,1))


# Define server logic required to draw a scatter plot
shinyServer(function(input, output) {
   
  output$sctPlot <- renderPlot({
    # Set x and y based on inputs
    pov1<-input$pctpov[1]
    pov2<-input$pctpov[2]
    age1<-input$medage[1]
    age2<-input$medage[2]
    wht1<-input$pctwht[1]
    wht2<-input$pctwht[2]
    
    y<- cty18_wide$INTPCT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                            age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                            wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
    if(input$variable=="povpct"){
                  x<- cty18_wide$POVPCT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                            age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                            wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
                  labx<-"Percent of children in Poverty"}
            
    if(input$variable=="medage"){
                  x<- cty18_wide$MEDAGE[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                              age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                              wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
                  labx<-"Median Age"}
    if(input$variable=="pctwht"){
      x<- cty18_wide$WHT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                              age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                              wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
                  labx<-"Percent Non-Hispanic White"}
    fit<- lm(y ~ x)
    
    #Draw the plot
    plot(x=x,y=y,xlab=labx,ylab="Percent of Households with Home Internet")
    abline(fit)
    
  })
    output$R2 <- renderText({
      # Set x and y based on inputs
      pov1<-input$pctpov[1]
      pov2<-input$pctpov[2]
      age1<-input$medage[1]
      age2<-input$medage[2]
      wht1<-input$pctwht[1]
      wht2<-input$pctwht[2]
      
      y<- cty18_wide$INTPCT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                              age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                              wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
      if(input$variable=="povpct"){
        x<- cty18_wide$POVPCT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                                age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                                wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
        labx<-"Percent of children in Poverty"}
      
      if(input$variable=="medage"){
        x<- cty18_wide$MEDAGE[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                                age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                                wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
        labx<-"Median Age"}
      if(input$variable=="pctwht"){
        x<- cty18_wide$WHT[pov1<=cty18_wide$POVPCT & cty18_wide$POVPCT<=pov2 & 
                             age1<=cty18_wide$MEDAGE & cty18_wide$MEDAGE<=age2 &
                             wht1<=cty18_wide$WHT & cty18_wide$WHT<=wht2]
        labx<-"Percent Non-Hispanic White"}
      fit<- lm(y ~ x)
      paste("R2 value: ", round(summary(fit)$r.squared,2))
      })
    
  
})
