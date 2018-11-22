
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(lubridate)
library(magrittr)
library(DT)
library(RColorBrewer)
library(formattable)
library(stringr)

color  = c("#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#79BA45")

df_pass = data.frame(0,0,0,0,0,0,0,0)
names(df_pass) = c("tv_spends","hs_spends","tv_reach","hs_reach","tv_imp","hs_imp","actual_optimal","actual_tail")
input_month = unique(df %>% arrange(desc(date)) %>% select(Month))$Month

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "prism"),
                    dashboardSidebar(collapsed = TRUE,
                                     sidebarMenu()),
                    dashboardBody(
                      fluidRow(
                        fluidRow(
                          #column(1,imageOutput("image")),
                          column(3,selectInput("advertiser","Select advertiser name",advertiser_names,selected = NULL, multiple = FALSE)),
                          column(2,selectInput("discount","Select % Discount",as.character(seq(0,30,5)),selected = 0,multiple = FALSE)),
                          column(2,selectInput("month","Select Month",append("All",input_month),selected = NULL,multiple = FALSE)),
                          column(3,uiOutput("ui")),
                          column(2,selectInput("tg","Select TG",unique(df_cpm$tg),selected = NULL,multiple = FALSE))
                          
                        ),style='padding:10px;',
                        fluidRow(
                          column(12,h4("Advertiser TV Trends",
                                       style = 
                                         "font-family: 'Roboto', sans-serif, 'Times New Roman', serif;
                                       color: #ffffff;
                                       font-weight: normal;
                                       font-size: 15px;
                                       text-align: center;
                                       border-radius: 2px;
                                       border-bottom: 3px solid #424344;
                                       border-top: 3px solid #424344;
                                       background-color: #424344"))#,style='padding:10px;'
                          ),
                        fluidRow(
                          column(4,plotlyOutput("table_vertical")),
                          column(5,plotlyOutput("plot2")),
                          column(3,plotlyOutput("pie_brands")),style='padding:10px;'
                        ),
                        
                        fluidRow(
                          column(3,plotlyOutput("plot3")),
                          column(4,plotlyOutput("table_genre")),
                          column(2,h4("Longtail Spend:")),
                          column(1,verbatimTextOutput("tail")),
                          column(5,verbatimTextOutput("tail_insights_fill")),
                          column(5,verbatimTextOutput("tail_insights")),
                          style='padding:10px;'
                        ),
                        fluidRow(
                          column(12,h4("TV + Hotstar Plan",
                                       style = 
                                         "font-family: 'Roboto', sans-serif, 'Times New Roman', serif;
                                       color: #ffffff;
                                       font-weight: normal;
                                       font-size: 15px;
                                       text-align: center;
                                       border-radius: 2px;
                                       border-bottom: 3px solid #424344;
                                       border-top: 3px solid #424344;
                                       background-color: #424344"))#,style='padding:10px;'
                          ),
                        fluidRow(
                          column(12,verbatimTextOutput("campaign_info"))
                        ),
                        fluidRow(
                          column(2,verbatimTextOutput("stack_fill1")),
                          column(2,plotlyOutput("plot_stack_spends")),
                          column(2,plotlyOutput("plot_stack_actual_impressions")),
                          column(2,plotlyOutput("plot_stack_impressions")),
                          column(2,plotlyOutput("plot_stack_reach")),
                          column(2,verbatimTextOutput("stack_fill2"))
                        ),
                        fluidRow(
                          column(12,h4("Insights",style = 
                                         "font-family: 'Roboto', sans-serif, 'Times New Roman', serif;
                                       color: #ffffff;
                                       font-weight: normal;
                                       font-size: 15px;
                                       text-align: center;
                                       border-radius: 2px;
                                       border-bottom: 3px solid #424344;
                                       border-top: 3px solid #424344;
                                       background-color: #424344"))#,style='padding:10px;'
                          ),
                        
                        fluidRow(
                          column(12,verbatimTextOutput("insights_summary")),
                          column(6,verbatimTextOutput("insights_tv")),
                          column(6,verbatimTextOutput("insights_hs")),style='padding:10px;'
                        ),
                        
                        fluidRow(
                          column(12,h4("Ad Product Solutions",style = 
                                         "font-family: 'Roboto', sans-serif, 'Times New Roman', serif;
                                       color: #ffffff;
                                       font-weight: normal;
                                       font-size: 15px;
                                       text-align: center;
                                       border-radius: 2px;
                                       border-bottom: 3px solid #424344;
                                       border-top: 3px solid #424344;
                                       background-color: #424344"))#,style='padding:10px;'
                          ),
                        fluidRow(
                          column(6,h4("Select campaign objective",style = "text-align: right; font-size: 15px")),
                          column(6,uiOutput("in_obj")),
                          style='background-color: #bdbdbd"; padding:10px;'
                        ),
                        fluidRow(
                          column(1,imageOutput("image1_1",width = "250%", height = "20px")),
                          column(2,h4("Awareness",style="text-align: left")),
                          column(1,imageOutput("image1_2",width = "230%", height = "10px")),
                          column(2,h4("Sustenance",style="text-align: left")),
                          column(1,imageOutput("image1_3",width = "230%", height = "20px")),
                          column(2,h4("Engagement",style="text-align: left")),
                          column(1,imageOutput("image1_4",width = "230%", height = "15px")),
                          column(2,h4("Content Solutions",style="text-align: left")),style='padding:10px;'
                        ),
                        #fluidRow(
                        #  #column(12,DT::dataTableOutput("table_ad_prod")),
                        #  column(3,imageOutput("image1_1",width = "100%", height = "100px")),
                        #  column(3,imageOutput("image1_2",width = "100%", height = "100px")),
                        #  column(3,imageOutput("image1_3",width = "100%", height = "100px")),
                        #  column(3,imageOutput("image1_4",width = "100%", height = "100px")),style='padding:10px;'
                        #),
                        fluidRow(
                          #column(12,DT::dataTableOutput("table_ad_prod")),
                          column(3,imageOutput("image2_1",width = "100%", height = "100px")),
                          column(3,imageOutput("image2_2",width = "100%", height = "100px")),
                          column(3,imageOutput("image2_3",width = "100%", height = "100px")),
                          column(3,imageOutput("image2_4",width = "100%", height = "100px")),style='padding:10px;'
                        ),
                        fluidRow(
                          column(3,textOutput("ad_product_1")),
                          column(3,textOutput("ad_product_2")),
                          column(3,textOutput("ad_product_3")),
                          column(3,textOutput("ad_product_4"))
                        ),
                        tags$head(tags$link(
                          rel = "stylesheet", 
                          href="https://fonts.googleapis.com/css?family=Roboto"
                        )
                        ,tags$style(HTML('
                                         .main-header .logo {
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         font-size: 24px;
                                         }
                                         .content-wrapper,
                                         .left-side {background-color: #ffffff;
                                         }
                                         
                                         .row,
                                         #insights_tv {background-color: #ffffff;
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         text-align: center;
                                         line-height: 2;}
                                         #insights_hs {background-color: #ffffff;
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         text-align: center;
                                         line-height: 2;}
                                         #insights_summary {background-color: #ffffff;
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         text-align: center;
                                         line-height: 2;}                                         
                                         
                                         
                                         .col-sm-4,
                                         #tail_insights {background-color: #ffffff;
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         text-align: center;
                                         line-height: 2;}
                                         #tail_insights_fill {background-color: #ffffff;
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         font-weight: normal;
                                         text-align: center;
                                         line-height: 2;
                                         border-style: hidden;}
                                         
                                         .skin-green .main-header .navbar {
                                         background-color: #79BA45;
                                         }
                                         
                                         .skin-green .main-header .logo {
                                         background-color: #79BA45;
                                         }
                                         
                                         .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
                                         background-color: #ffffff;
                                         }
                                         
                                         .col-sm-12,
                                         #campaign_info {background-color: #ffffff;
                                         border-style: hidden;}
                                         
                                         table.dataTable thead th, table.dataTable thead td
                                         {background-color: #dbdbdb;}
                                         
                                         code, kbd, pre, samp {
                                         font-family: "Roboto", sans-serif, "Times New Roman", serif;
                                         }
                                         
                                         
                                         
                                         ')))
                        )
                        )
                        )

server <- function(input, output) {
  
  df_pas = reactiveValues(tv_spends = 0)
  df_pas = reactiveValues(hs_spends = 0)
  df_pas = reactiveValues(tv_reach = 0)
  df_pas = reactiveValues(hs_reach = 0)
  df_pas = reactiveValues(tv_imp = 0)
  df_pas = reactiveValues(hs_imp = 0)
  df_pas = reactiveValues(actual_optimal = 0)
  df_pas = reactiveValues(actual_tail = 0)
  df_pas = reactiveValues(tv_freq = 0)
  df_pas = reactiveValues(hs_freq = 0)
  df_pas = reactiveValues(rate = 0)
  df_pas = reactiveValues(default_brand = "")
  df_pas = reactiveValues(default_month = "")
  
  output$ui = renderUI({
    #brand_names = unique(df$Brand[df$Advertiser == as.character(input$advertiser)]) 
    if(input$month == "All"){
      brand_names = append(as.character(unique(df$Brand[df$Advertiser == as.character(input$advertiser)])),"All") 
    }
    if(as.character(input$month) != "All"){
      brand_names = append(as.character(unique(df$Brand[df$Advertiser == as.character(input$advertiser) & df$Month == as.character(input$month) & !(df$CS.15.40.AB.M ==0 | df$CS.15.40.AB.F ==0 | df$CS.22.40.AB.M ==0 | df$CS.22.40.AB.F ==0)])),"All") 
    }
    
    selectInput("brand","Select brand",brand_names,selected = "All")
  })
  
  output$in_obj = renderUI({
    #brand_names = unique(df$Brand[df$Advertiser == as.character(input$advertiser)]) 
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    obj = as.character(unique(df_ad_prod$Campaign.objective[df_ad_prod$Vertical == vertical]))
    selectInput("objective",NULL,obj,selected = "Branding")
  })
  
  output$tail_insights = renderText({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All"))){
      if(is_in(input$month,unique(df_tvhs$Month))){
        df1 = df_cpm %>% filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == as.character(input$month), df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
        msg = paste("Plan efficiency insights\nBrand: ",sub("(.)", ("\\U\\1"), tolower(input$brand), pe=TRUE)," Month:",input$month,sep="")
      }      
      else {
        df1 = df_cpm %>% filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1], df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
        msg = paste("Plan efficiency insights\nBrand: ",sub("(.)", ("\\U\\1"), tolower(input$brand), pe=TRUE)," Month:",unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1],sep="")
      }
      if(nrow(df1)>0){
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm = df1$cpm/(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      
      }
      else{
        msg = paste("Data not available for ",input$brand," for the month of ",unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1],sep="")
      }
    }
    else {
      df1 = df_cpm %>% filter(df_cpm$Advertiser == as.character(input$advertiser), 
                              df_cpm$tg == as.character(input$tg)) %>% 
        select(Channel,Genre,Spends,cpm)      
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm = df1$cpm/(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      #print(df1)
      #round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      msg = paste("Plan efficiency insights\nAdvertiser: ",sub("(.)", ("\\U\\1"), tolower(input$advertiser), pe=TRUE),", All available months",sep="")
    }
    tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
    tail_percent = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^5/(sum(df1$Spends)/10^7),0)
    df2 = df1[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment",]
    times = round( (sum(df2$Spends)/sum(df2$cpm))/(sum(df1$Spends)/sum(df1$cpm)) ,0)
    if(!is.na(times)){
      msg = paste(msg,"\nOf the total ",nrow(df1[,])," channles in your mix, ",nrow(df1[df1$outlier_mean=="Yes",])," were inefficient.",sep="")
      msg = paste(msg,"\nEstimated longtail spend of ",tail,"Cr (",tail_percent,"% of overall plan)")
      msg = paste(msg,"\nEnglish, HD, Infotainment clusters were operating at ",
                  times,"X of the overall CPM",sep="")
      msg
    }
    else{
      msg
    }
    
  })
  
  output$tail_insights_fill = renderText({"\n\n\n"})
  
  output$tail = renderText({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All"))){
      if(is_in(input$month,unique(df_tvhs$Month))){
        df1 = df_cpm %>% filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == as.character(input$month), df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      }      
      else {
        df1 = df_cpm %>% filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1], df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      }
      if(nrow(df1)>0){
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm = df1$cpm/(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      #print(df1)
      round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      }
      else{
        "na"
      }
    }
    else {
      df1 = df_cpm %>% filter(df_cpm$Advertiser == as.character(input$advertiser), 
                              df_cpm$tg == as.character(input$tg)) %>% 
        select(Channel,Genre,Spends,cpm)      
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm = df1$cpm/(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      #print(df1)
      round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
    }
  })
  
  ##Monthly Spends - bar chart
  output$plot2 = renderPlotly({
    
    if ((is.null(input$brand) || as.character(input$brand) == "All") && is.null(input$tg)){
      df1 = df %>% 
        group_by(Month,Month_No,date) %>% 
        filter(Advertiser == as.character(input$advertiser)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1)) %>% 
        arrange(date)
    }
    if ((is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg)){
      df1 = df %>% 
        group_by(Month,Month_No,date) %>% 
        filter(Advertiser == as.character(input$advertiser)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1)) %>% 
        arrange(date)
    }
    else{
      df1 = df %>% 
        group_by(Month,Month_No,date) %>% 
        filter(Advertiser == as.character(input$advertiser),Brand == as.character(input$brand)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1)) %>% 
        arrange(date)
    }  
    df1$spends = round(df1$spends*(1-as.numeric(input$discount)/100),1)
    df1 = df1 %>% arrange(df1$date)
    df1$Month = format(df1$date,"%b-%y")
    #print(df1)
    xform = list(categoryorder = "array", categoryarray = df1$Month)
    plot_ly(df1) %>% 
      add_trace(x = df1$Month, y = df1$spends, type = "bar",hoverinfo = "text",text = df1$spends ,
                textposition = "auto", name = "Spends", color = I("gray")) %>%
      layout(title = 'Monthly Spends (Cr)',font = list(family = "Roboto",size = 10,color = 'black'),
             xaxis = xform,
             yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE)) %>%
      layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
      layout(paper_bgcolor='rgb(255, 255, 255)')
    
  })
  
  ##Vertical players - Table
  output$table_vertical = renderPlotly({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    df1 = df %>% group_by(Advertiser) %>% 
      filter(HS.Vertical == vertical) %>% 
      summarise(Spends = sum(Spends)) %>% arrange(-Spends)
    df1$Spends = round(df1$Spends/10^7,1)
    df2 = data.frame("Total",sum(df1$Spends))
    if(nrow(df1)>=10){df1 = df1[1:10,]}
    names(df2) = names(df1)
    df1 = rbind(df2,df1)
    #names(df1) = c(paste("Top 10 Advertisers in ",vertical,sep=""),"Spends (Cr)")
    #DT::datatable(df1[c(0:10),],rownames = FALSE, options = list(dom = 't',columnDefs = list(list(className = 'dt-center', targets =0:1)))) %>%
    #  formatStyle( 0, target= 'row',color = 'black', backgroundColor = 'white', lineHeight='99.99%')
    #df1 = df1[1:10,]
    
    df1$Spends[df1$Advertiser == input$advertiser] = df1$Spends[df1$Advertiser == input$advertiser]*(1-as.numeric(input$discount)/100)
    df1 = df1 %>% arrange(df1$Spends)
    df1$Spends[df1$Advertiser == 'Total'] = sum(df1$Spends[df1$Advertiser != 'Total'])
    #df1$Advertiser = str_wrap(df1$Advertiser,width = 20)
    yform = list(categoryorder = "array", categoryarray = df1$Advertiser)
    df1$Advertiser = factor(df1$Advertiser,levels = df1[["Advertiser"]])
    plot_ly(df1) %>% 
      add_trace(x = df1$Spends, y = df1$Advertiser, type = "bar",orientation = 'h',hoverinfo = "text",text = paste(df1$Advertiser," (",df1$Spends,")",sep="") ,
                textposition = "auto",marker = list(color = c(as.character(toRGB(color[(12-nrow(df1)):11])))) ) %>%
      #add_trace(x = sum(df1$Spends), y = "Total", type = "bar",orientation = 'h',
      #          marker = list(color = c(as.character(toRGB("#79BA45")))), hoverinfo = "text",text=as.character(round(sum(df1$Spends),1)),textposition = "auto") %>%
      layout(title = paste("Top 10 Advertisers in ",vertical,sep=""),font = list(family = "Roboto",size = 10,color = 'black'),
             yaxis = list(showticklabels = FALSE,categoryarray = df1$Advertiser, categoryorder = "array"),
             xaxis = list(showticklabels = FALSE,showgrid = FALSE,zeroline = FALSE),showlegend = FALSE)
    
    
  })
  
  output$table_ad_prod = DT::renderDataTable({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    df1 = df_ad_prod %>% filter(Vertical1 == vertical)
    #names(df1) = paste("Top 10 Advertiser in ",vertical,sep="")
    DT::datatable(df1[3:10],rownames = FALSE, options = list(dom = 't',columnDefs = list(list(className = 'dt-center', targets = 0:7)))) %>%
      formatStyle( 0, target= 'row',color = 'black', backgroundColor = 'white', lineHeight='99.99%')
  })
  
  ##Genre-wise spends - pie chart    
  output$plot3 = renderPlotly({
    
    if ((is.null(input$brand) || as.character(input$brand) == "All") && (is.null(input$month) || (as.character(input$month) == "All"))){
      df3 = df %>% 
        group_by(Genre) %>% 
        filter(Advertiser == as.character(input$advertiser)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1))
    }
    if ((is.null(input$brand) || as.character(input$brand) == "All") && !(is.null(input$month) || (as.character(input$month) == "All"))){
      df3 = df %>% 
        group_by(Genre) %>% 
        filter(Advertiser == as.character(input$advertiser),Month == as.character(input$month)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1))
      
    }
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && (is.null(input$month) || (as.character(input$month) == "All"))){
      df3 = df %>% 
        group_by(Genre) %>% 
        filter(Advertiser == as.character(input$advertiser),Brand == as.character(input$brand)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1))
      
    }
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !(is.null(input$month) || (as.character(input$month) == "All"))){
      df3 = df %>% 
        group_by(Genre) %>% 
        filter(Advertiser == as.character(input$advertiser),Brand == as.character(input$brand), Month == as.character(input$month)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1))
      
    }
    df3$spends = df3$spends*(1-as.numeric(input$discount)/100)
    tot_spend = sum(df3$spends)
    df4 = df3[(df3$spends*100/tot_spend)>2,]
    df_others = df3[1,]
    df_others$Genre = "Others"
    df_others$spends = sum(df3$spends[(df3$spends*100/tot_spend)<2])
    #df4[nrow(df4)+1,] = c("Others",sum(df3$spends[(df3$spends*100/tot_spend)<2]))
    df4 = rbind(df4,df_others)
    
    plot_ly(df4, labels = df4$Genre, values = df4$spends, type = 'pie', textposition = 'inside',
            textinfo = 'label+text',text = as.character(percent(df4$spends/sum(df4$spends),0)) #put textinfo = 'label+value' for actual percentage
            ,hoverinfo = 'label+value+text', marker = list(colors = colorRampPalette(brewer.pal(12,"Paired"))(nrow(df4))) ) %>% 
      layout(title = 'Genre-wise Spends',font = list(family = "Roboto",size = 10,color = 'black'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
      layout(paper_bgcolor='rgb(255, 255, 255)')
    
  })
  
  ##HD, Eng, Infotainment contribution - table
  output$table_genre =renderPlotly({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) ){
      if(is_in(input$month,unique(df_tvhs$Month))){
        df_tail = df_cpm %>% 
          filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == as.character(input$month), df_cpm$tg == as.character(input$tg)) %>% 
          select(Channel,Genre,Spends,cpm)
      }
      else {
        df_tail = df_cpm %>% 
          filter(df_cpm$Brand == as.character(input$brand), df_cpm$Month == unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1], df_cpm$tg == as.character(input$tg)) %>% 
          select(Channel,Genre,Spends,cpm)
      }
    }
    else {
      df_tail = df_cpm %>% 
        filter(df_cpm$Advertiser == as.character(input$advertiser),df_cpm$tg == as.character(input$tg)) %>% 
        select(Channel,Genre,Spends,cpm)
    }
    
    if(nrow(df_tail)>0){
    df_tail$Spends = df_tail$Spends*(1-as.numeric(input$discount)/100)
    df_tail$cpm_1 = df_tail$Spends/df_tail$cpm
    df_tail$cpm_1[!is.finite(df_tail$cpm_1)]=0
    df_tail$outlier_mean = "No"
    df_tail$outlier_mean[df_tail$cpm_1>(mean(df_tail$cpm_1)*1.5)]="Yes"
    df_tail$outlier_mean[df_tail$Genre == "English" | df_tail$Genre == "HD" | df_tail$Genre == "Infotainment"] = "Yes3"
    #print(df_tail)
    #df_tail = df_tail[df_tail$outlier_mean %like% "Yes",]
    df_tail$Genre1 = df_tail$Genre
    df_tail$Genre1[df_tail$outlier_mean == "No"] = "Optimal Channels"
    df_tail$Genre1[df_tail$outlier_mean == "Yes"] = "Other Longtail"
    
    dft = df_tail %>% group_by(Genre1) %>% summarise(spends = sum(Spends),imp = sum(cpm))
    dft$cpm = round(dft$spends/dft$imp,0)
    dft$spends = round(dft$spends/10^7,1)
      
    dft = dft %>% arrange(-cpm)  
    
    dft_tot = data.frame(Genre1 = "Average CPM",spends = sum(dft$spends), imp = sum(dft$imp))
    dft_tot$cpm = round(dft_tot$spends*10^7/dft_tot$imp,0)
    dft = rbind(dft,dft_tot)
    dft$spends = round(dft$spends,1)
    
    check_yes3 = c("HD","English","Infotainment","Other Longtail")[c("HD","English","Infotainment") %in% dft$Genre1]
    
    if("Other Longtail" %in% dft$Genre1){
    for(i in length(check_yes3)-1){
      if(dft$cpm[dft$Genre1=="Other Longtail"]/dft$cpm[dft$Genre1==check_yes3[i]] >9){
        dft = dft[!dft$Genre1=="Other Longtail",]
      }
    }
    }
    
    
    yform = list(categoryorder = "array", categoryarray = rev(dft$Genre1),size = 10)
    
    if(nrow(dft)>0){
      
      plot_ly(dft) %>% 
        add_trace(x = dft$cpm, y = dft$Genre1, type = "bar",orientation = 'h',hoverinfo = "text",text = dft$cpm ,
                  textposition = "auto",marker = list(color = c(as.character(toRGB(color[(12-nrow(dft)):11])))) ) %>%
        layout(title = "Inefficient channels (CPM)",font = list(family = "Roboto",size = 10,color = 'black'),
               yaxis = yform,
               xaxis = list(showticklabels = FALSE,showgrid = FALSE,zeroline = FALSE),showlegend = FALSE,
               margin = list(l = 150))
    }
    else{
      
      plot_ly(df2) %>% 
        add_trace(x = c(2), y = c(0), type = "bar",orientation = 'h',hoverinfo = "text",text = "Spends too low" ,
                  textposition = "middle right",marker = list(color = I("white")) ) %>%
        #add_trace(x = sum(df1$Spends), y = "Total", type = "bar",orientation = 'h',
        #          marker = list(color = c(as.character(toRGB("#79BA45")))), hoverinfo = "text",text=as.character(round(sum(df1$Spends),1)),textposition = "auto") %>%
        layout(title = "Inefficient channels (CPM) -Spends too low",font = list(family = "Roboto",size = 10,color = 'black'),
               yaxis = yform,
               xaxis = list(showticklabels = FALSE,showgrid = FALSE,zeroline = FALSE),showlegend = FALSE,
               margin = list(l = 150))
    }
    
    
    
    }
    else{
      plot_ly(df2) %>% 
        add_trace(x = c(2), y = c(0), type = "bar",orientation = 'h',hoverinfo = "text",text = "Spends too low" ,
                  textposition = "middle right",marker = list(color = I("white")) ) %>%
        #add_trace(x = sum(df1$Spends), y = "Total", type = "bar",orientation = 'h',
        #          marker = list(color = c(as.character(toRGB("#79BA45")))), hoverinfo = "text",text=as.character(round(sum(df1$Spends),1)),textposition = "auto") %>%
        layout(title = "Data not available for the said month",font = list(family = "Roboto",size = 10,color = 'black'),
               yaxis = yform,
               xaxis = list(showticklabels = FALSE,showgrid = FALSE,zeroline = FALSE),showlegend = FALSE,
               margin = list(l = 150))
    }
  })
  
  ##Top brands for an advertiser - table
  output$pie_brands = renderPlotly({
    
    if (is.null(input$month) || (as.character(input$month) == "All")){
      df1 = df %>% 
        group_by(Brand) %>% 
        filter(Advertiser == as.character(input$advertiser)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1)) %>%
        arrange(spends)
    }
    if (!(is.null(input$month) || (as.character(input$month) == "All"))){
      df1 = df %>% 
        group_by(Brand) %>% 
        filter(Advertiser == as.character(input$advertiser),Month == as.character(input$month)) %>% 
        summarize(spends = round(sum(Spends)/10^7,digits =1)) %>%
        arrange(spends)
    }
    df1$spends = df1$spends*(1-as.numeric(input$discount)/100)
    df1$percent = round(df1$spends*100/sum(df1$spends),0)
    df1 = df1 %>% arrange(-spends)
    #df2 = head(df1,n = 10)
    df2 = df1
    df2$percent = df2$percent/100
    names(df2) = c("Top Brands","Spends","Percentage")
    if(nrow(df2)>20){
      df_tot = data.frame("Others",sum(df2$Spends[21:nrow(df2)]),0)
      names(df_tot) = names(df2)
      df2 = df2[1:20,]
      df2 = rbind(df2,df_tot)  
    }
    
    #DT::datatable(df2,rownames = FALSE, 
    #              options = list(dom = 't',columnDefs = list(list(className = 'dt-center', targets =0:2)))) %>% 
    #  formatPercentage("Percentage") %>%
    #  formatStyle( 0, target= 'row',color = 'black', backgroundColor = 'white', lineHeight='100%')
    plot_ly(df2, labels = df2$`Top Brands`, values = df2$Spends, type = 'pie', textposition = 'inside',
            textinfo = 'label+value', text = as.character(percent(df2$Spends/sum(df2$Spends),0)), 
            hoverinfo = 'label+text+value'
            ,marker = list(colors = colorRampPalette(brewer.pal(12,"Paired"))(nrow(df2))) ) %>% 
      layout(title = 'Brands by spends (Cr)',font = list(family = "Roboto",size = 10,color = 'black'))%>%
      layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
      layout(paper_bgcolor='rgb(255, 255, 255)')
    
  })  
  
  output$plot_stack_spends = renderPlotly({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month)) ){
      
      df2 = df_cpm %>%
        filter(df_cpm$tg == input$tg, df_cpm$Brand == input$brand,df_cpm$Month == input$month) %>%
        group_by(Brand,Month,date) %>% summarise(Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      #print(df1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$Revenue = tail
      df2$Spends = round(df2$Spends/(10^7),1)
      df2$Spends = df2$Spends - df2$Revenue
      x_lab = c("Spends (Cr)")
      
      df_pas$tv_spends = df2$Spends
      df_pas$hs_spends = df2$Revenue
      
      plot_ly(x = x_lab, y = c(df2$Spends), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$Spends), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(df2$Revenue), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(df2$Revenue),marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>% 
        layout(showlegend = FALSE)
    }
    
    else{
      df2 = df_cpm %>%
        filter(df_cpm$tg == input$tg, df_cpm$Advertiser == input$advertiser) %>%
        group_by(Brand,Month,date) %>% summarise(Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df_pas$default_brand = df2$Brand
      df_pas$default_month = df2$Month
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      #print(df1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$Revenue = tail
      df2$Spends = round(df2$Spends/(10^7),1)
      df2$Spends = df2$Spends - df2$Revenue
      x_lab = c("Spends (Cr)")
      
      df_pas$tv_spends = df2$Spends
      df_pas$hs_spends = df2$Revenue
      
      
      plot_ly(x = x_lab, y = c(df2$Spends), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$Spends),marker = list(color = "#bdbdbd") ) %>%
        add_trace(y =  c(df2$Revenue), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(df2$Revenue), marker = list(color = "#79BA45") ) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
      
    }
  })
  
  output$plot_stack_actual_impressions = renderPlotly({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month))){
      df2 = df_cpm %>%
        filter(df_cpm$Brand == input$brand,df_cpm$tg == input$tg,df_cpm$Month == input$month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month,
                              df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      x_lab = c("TV Impressions (Mn)")
      
      df_pas$actual_optimal = df2$imp
      df_pas$actual_tail = tail_imp
      
      plot_ly(x = x_lab, y = c(df2$imp), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$imp), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(tail_imp), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(tail_imp), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
      
    }
    else{
      df2 = df_cpm %>%
        filter(df_cpm$tg == input$tg, df_cpm$Brand == df_pas$default_brand, df_cpm$Month == df_pas$default_month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      x_lab = c("TV Impressions (Mn)")
      
      df_pas$actual_optimal = df2$imp
      df_pas$actual_tail = tail_imp
      
      plot_ly(x = x_lab, y = c(df2$imp), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$imp), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(tail_imp), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(tail_imp), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE,showgrid = FALSE,zeroline = FALSE,gridcolor = "#ffffff")
      
    }
  })
  
  output$plot_stack_impressions = renderPlotly({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month))){
      df2 = df_cpm %>%
        filter(df_cpm$Brand == input$brand,df_cpm$tg == input$tg,df_cpm$Month == input$month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      
      
      #impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      impression = round((tail*10^4)/df_pas$rate,1)
      x_lab = c("Impressions (Mn)")
      
      df_pas$tv_imp = df2$imp
      df_pas$hs_imp = impression
      
      plot_ly(x = x_lab, y = c(df2$imp), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$imp), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(impression), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(impression), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
      
    }
    else{
      df2 = df_cpm %>%
        filter(df_cpm$tg == input$tg, df_cpm$Brand == df_pas$default_brand, df_cpm$Month == df_pas$default_month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      
      
      
      
      impression = round((tail*10^4)/df_pas$rate,1)
      #impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      x_lab = c("Impressions (Mn)")
      
      df_pas$tv_imp = df2$imp
      df_pas$hs_imp = impression
      
      plot_ly(x = x_lab, y = c(df2$imp), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$imp), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(impression), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(impression), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
      
    }
  })
  
  output$plot_stack_reach = renderPlotly({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month))){
      df2 = df_cpm %>%
        filter(df_cpm$Brand == input$brand,df_cpm$tg == input$tg,df_cpm$Month == input$month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      df_tv_freq = df_tvhs %>% filter(Brand == input$brand,tg == input$tg,Month == input$month)
      df_tv_freq$freq = df_tv_freq$imp/df_tv_freq$cov
      df_tv_freq = df_tv_freq[1,]
      df2$cov = df2$imp/df_tv_freq$freq
      df_pas$tv_freq = df_tv_freq$freq
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg 
                                                         & df_tvhs$Month == input$month])) <= 20 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 200
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 20 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) <= 30 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 250
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 30 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) <= 45 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 413
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 45 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 550
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) <= 20 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 450
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 20 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) <= 30 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 562.5
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 30 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) <= 45 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 929.25
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == input$brand & df_tvhs$tg == input$tg & df_tvhs$Month == input$month])) > 45 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 1237.5
      }
      
      
      df2$impression = round((tail*10^4)/df_pas$rate,1)
      #df2$impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      df2$reach = round(df2$impression/3,1)
      i=3
      
      reach_limit = df_reach_limit$value[df_reach_limit$tg == input$tg]
      zapr = df_reach_limit$zapr[df_reach_limit$tg == input$tg]
      
      while(df2$reach > reach_limit){
        i=i+1
        df2$reach = round(df2$impression/i,1)
      }
      df_pas$hs_freq = i
      print(paste("hs_freq:",df_pas$hs_freq))
      df2$reach = round(df2$reach * zapr,1)
      
      x_lab = c("Reach (Mn)")
      
      df_pas$tv_reach = df2$cov
      df_pas$hs_reach = df2$reach
      
      plot_ly(x = x_lab, y = c(df2$cov), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$cov), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(df2$reach), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(df2$reach), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
    }
    else{
      df2 = df_cpm %>%
        filter(df_cpm$tg == input$tg, df_cpm$Brand == df_pas$default_brand, df_cpm$Month == df_pas$default_month) %>%
        group_by(Brand,Month,date) %>% summarise(imp = sum(cpm),Spends = sum(Spends)) %>% arrange(date,Spends)
      df2 = df2[nrow(df2),]
      df2$imp = round(df2$imp/(10^3),1)
      df2$Spends = df2$Spends*(1-as.numeric(input$discount)/100)
      
      df_tv_freq = df_tvhs %>% filter(Brand == df2$Brand,tg == input$tg,Month == df2$Month)
      df_tv_freq = df_tv_freq[1,]
      df_tv_freq$freq = df_tv_freq$imp/df_tv_freq$cov
      df2$cov = df2$imp/df_tv_freq$freq
      df_pas$tv_freq = df_tv_freq$freq
      
      df1 = df_cpm %>% filter(df_cpm$Brand == df2$Brand, df_cpm$Month == df2$Month, df_cpm$tg == as.character(input$tg)) %>% select(Channel,Genre,Spends,cpm)
      df1$Spends = df1$Spends*(1-as.numeric(input$discount)/100)
      df1$cpm_1 = df1$Spends/df1$cpm
      df1$cpm_1[!is.finite(df1$cpm_1)]=0
      df1$outlier_mean = "No"
      df1$outlier_mean[df1$cpm_1>(mean(df1$cpm_1)*1.5)]="Yes"
      df1$outlier_mean[df1$Genre == "English" | df1$Genre == "HD" | df1$Genre == "Infotainment"] = "Yes"
      tail_imp = round(sum(df1$cpm[df1$outlier_mean == "Yes"])/10^3,1)
      tail = round(sum(df1$Spends[df1$outlier_mean == "Yes"])/10^7,1)
      df2$imp = round(df2$imp - tail_imp,1)
      #print(df1)
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg 
                                                         & df_tvhs$Month == df2$Month])) <= 20 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 200
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 20 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) <= 30 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 250
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 30 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) <= 45 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 413
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 45 
         & (input$tg %in%  c("CS.22.40.AB.MF","CS.15.40.AB.M","CS.15.40.AB.MF","CS.22.40.AB.M"))){
        df_pas$rate = 550
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) <= 20 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 450
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 20 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) <= 30 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 562.5
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 30 
         & as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) <= 45 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 929.25
      }
      if(as.numeric(as.character(df_tvhs$Avg.Ad.Length.y[df_tvhs$Brand == df2$Brand & df_tvhs$tg == input$tg & df_tvhs$Month == df2$Month])) > 45 
         & (input$tg %in%  c("CS.15.40.AB.F","CS.22.40.AB.F"))){
        df_pas$rate = 1237.5
      }
      
      
      df2$impression = round((tail*10^4)/df_pas$rate,1)
      #df2$impression = round((tail*10^4)/df_rates_lookup$rate[df_rates_lookup$tg == input$tg],1)
      df2$reach = round(df2$impression/3,1)
      i=3
      
      reach_limit = df_reach_limit$value[df_reach_limit$tg == input$tg]
      zapr = df_reach_limit$zapr[df_reach_limit$tg == input$tg]
      
      while(df2$reach > reach_limit){
        i=i+1
        df2$reach = round(df2$impression/i,1)
      }
      df_pas$hs_freq = i
      print(paste("hs_freq:",df_pas$hs_freq))
      df2$reach = round(df2$reach * zapr,1)
      
      x_lab = c("Reach (Mn)")
      
      df_pas$tv_reach = df2$cov
      df_pas$hs_reach = df2$reach
      
      plot_ly(x = x_lab, y = c(df2$cov), type = 'bar', name = 'TV',hoverinfo = "text",textposition = "auto",
              text = c(df2$cov), marker = list(color = "#bdbdbd")) %>%
        add_trace(y =  c(df2$reach), name = 'Hotstar',hoverinfo = "text",textposition = "auto",
                  text = c(df2$reach), marker = list(color = "#79BA45")) %>%
        layout(yaxis = list(title = '',showticklabels = FALSE,showlegend = FALSE,showgrid = FALSE), barmode = 'stack') %>%
        layout(plot_bgcolor='rgb(255, 255, 255)') %>% 
        layout(paper_bgcolor='rgb(255, 255, 255)') %>%
        layout(showlegend = FALSE)
      
    }
  })
  
  output$campaign_info = renderText({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All"))){
      if(!is_in(input$month,unique(df_tvhs$Month))){
        
        
        msg = "Impressions calculated at "
        msg = paste(msg,df_pas$rate," CPM for the selected TG.",sep = "")
        #msg = paste(msg,df_rates_lookup$rate[df_rates_lookup$tg == input$tg]," CPM for the selected TG.",sep = "")
        msg = paste(msg,"\nThis data is available Dec 2017 onwards, please select accordingly.\nNow showing data for the month of ",
                    unique(df_tvhs %>% arrange(Month) %>% select(Month))[nrow(unique(df_tvhs %>% arrange(Month) %>% select(Month))),1],
                    sep = "")
      }
      else {
        
        
        msg = "Impressions calculated at "
        msg = paste(msg,df_pas$rate," CPM for the selected TG.",sep = "")
        #msg = paste(msg,df_rates_lookup$rate[df_rates_lookup$tg == input$tg]," CPM for the selected TG.",sep = "")
        
      }
      msg
      
    }
    else{
      
      msg = "Showing data for brand with max revenue. "
      #msg = paste(msg,"Brand:",df2$Brand,".\nImpressions calculated at ",df_rates_lookup$rate[df_rates_lookup$tg == input$tg]," CPM for the selected TG.",sep = "")
      msg = paste(msg,"Brand:",sub("(.)", ("\\U\\1"), tolower(df_pas$default_brand), pe=TRUE),", Month:",df_pas$default_month,".\nImpressions calculated at ",as.character(df_pas$rate)," CPM for the selected TG.",sep = "")
      msg
      
    }
  })
  
  output$insights_tv = renderText({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month))){
      if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == input$brand & df_tvhs$Month == input$month]))>0){
        df1 = df_tvhs %>% filter(Brand == input$brand, Month == input$month, tg == input$tg) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical,Avg.Ad.Length.x,CTR,impression,Completed.Views)
        msg_tv = "TV"
        if(mean(df1$tv_freq) >6){
          msg_tv1 = paste("Your campaign was over-exposed on TV", " (Freq:",as.character(round(mean(df1$tv_freq),0)),")",
                          " This may result in fatigue or have negative impact on brand metrics. Hotstar can provide control on frequencies.",sep="")
          msg_tv = paste(msg_tv,str_wrap(msg_tv1,width = 90),sep="\n")
        }
        
        msg_tv
      }
      else if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == input$brand & df_tvhs$Month == input$month]))==0){
        df1 = df_tvhs %>% filter(Brand == input$brand, Month == input$month, tg == input$tg) %>% select(Brand,Frequency,tv_freq)
        msg_tv = "TV"
        if(mean(df1$tv_freq) >6){
          msg_tv1 = paste("\nYour campaign was over-exposed on TV", " (Freq:",as.character(round(mean(df1$tv_freq),0)),")",
                          " This may result in fatigue or have negative impact on brand metrics. Hotstar can provide control on frequencies.",sep="")
          msg_tv = paste(msg_tv,str_wrap(msg_tv1,width = 90),sep="\n")
        }
        
        msg_tv
      }
    }
    else{
      if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == df_pas$default_brand & df_tvhs$Month == df_pas$default_month]))>0){
        df1 = df_tvhs %>% filter(Brand == df_pas$default_brand, Month == df_pas$default_month, tg == input$tg) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical,Avg.Ad.Length.x,CTR,impression,Completed.Views)
        msg_tv = "TV"
        if(mean(df1$tv_freq) >6){
          msg_tv1 = paste("Your campaign was over-exposed on TV", " (Freq:",as.character(round(mean(df1$tv_freq),0)),")",
                          " This may result in fatigue or have negative impact on brand metrics. Hotstar can provide control on frequencies.",sep="")
          msg_tv = paste(msg_tv,str_wrap(msg_tv1,width = 90),sep="\n")
        }
        
        msg_tv
      }
      else if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == df_pas$default_brand & df_tvhs$Month == df_pas$default_month]))==0){
        df1 = df_tvhs %>% filter(Brand == df_pas$default_brand, Month == df_pas$default_month, tg == input$tg) %>% select(Brand,Frequency,tv_freq)
        msg_tv = "TV"
        if(mean(df1$tv_freq) >6){
          msg_tv1 = paste("\nYour campaign was over-exposed on TV", " (Freq:",as.character(round(mean(df1$tv_freq),0)),")",
                          " This may result in fatigue or have negative impact on brand metrics. Hotstar can provide control on frequencies.",sep="")
          msg_tv = paste(msg_tv,str_wrap(msg_tv1,width = 90),sep="\n")
        }
        
        msg_tv
      }
    }
  })
  
  output$insights_summary = renderText({
    msg = paste("TV Inefficient channels contributed ~",round(df_pas$hs_spends*100/(df_pas$tv_spends+df_pas$hs_spends),0),
                "% of the overall spends gave you only ",round(df_pas$actual_tail*100/(df_pas$actual_optimal+df_pas$actual_tail),0),
                "% impressions.\nPutting this money on Hotstar would have given ",
                round(df_pas$hs_imp*100/(df_pas$actual_optimal+df_pas$actual_tail),0),"% (",
                round(df_pas$hs_imp/df_pas$actual_tail,0),"X) Impressions and ",
                round(df_pas$hs_reach*100/((df_pas$actual_tail/df_pas$tv_freq)+df_pas$tv_reach),0),"% of the plan reach",sep="")
    
    msg
  })
  
  output$insights_hs = renderText({
    if (!(is.null(input$brand) || as.character(input$brand) == "All") && !is.null(input$tg) && !(is.null(input$month) || (as.character(input$month) == "All")) && is_in(input$month,unique(df_tvhs$Month))){
      if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == input$brand & df_tvhs$Month == input$month]))>0){
        msg_hs = "Hotstar"
        df1 = df_tvhs %>% filter(Brand == input$brand, Month == input$month, tg == input$tg) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical,Avg.Ad.Length.x,CTR,impression,Completed.Views)
        
        df_vertical = df_tvhs %>% filter(Vertical == df1$Vertical, Month == input$month) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical)
        df_vertical[is.na(df_vertical)] = 0
        if(mean(df1$Frequency) < mean(df_vertical$Frequency) | mean(df1$Frequency) < 3 ){
          msg_hs1 = paste("\nYour campaign was under-exposed on Hotstar.",
                          " To build recall and awareness, minimum average frequency should be 3.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        if(mean(df1$Frequency) > 8){
          msg_hs1 = paste("\nYour campaign was over-exposed on Hotstar.",
                          " This may result in fatigue or have negative impact on brand metrics.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        if(mean(df1$Revenue) < mean(df_vertical$Revenue) | mean(df1$Revenue) < 10^5){
          msg_hs1 = paste("\nYour campaign size is under-indexed on Hotstar.",
                          " Recommended campaign size is minimum 10 lakhs and 30+ lakhs for any visible movement in brand metrics.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        ctr_bench = with(df1,
                         ifelse(mean(Avg.Ad.Length.x) > 0.05 & mean(Avg.Ad.Length.x) <0.06,0.04,
                                ifelse(mean(Avg.Ad.Length.x) > 0.10 & mean(Avg.Ad.Length.x) <0.20,0.05,
                                       ifelse(mean(Avg.Ad.Length.x) > 0.21 & mean(Avg.Ad.Length.x) <0.30, 0.06,
                                              ifelse(mean(Avg.Ad.Length.x) > 0.31 & mean(Avg.Ad.Length.x) <0.60, 0.09,
                                                     ifelse(mean(Avg.Ad.Length.x) > 0.60, 0.07, 0))))))
        
        if(mean(df1$CTR) > ctr_bench){
          msg_hs1 = paste("\nYour campaign CTR was ",
                          as.character(round(mean(df1$CTR)/ctr_bench,1)),
                          "X of Average CTR. Reflection of precise targeting or creative with clear call to action.",
                          sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        completion_bench = with(df1,
                                ifelse(mean(Avg.Ad.Length.x) > 0.05 & mean(Avg.Ad.Length.x) <0.06,0.85,
                                       ifelse(mean(Avg.Ad.Length.x) > 0.10 & mean(Avg.Ad.Length.x) <0.20,0.96,
                                              ifelse(mean(Avg.Ad.Length.x) > 0.21 & mean(Avg.Ad.Length.x) <0.30, 0.95,
                                                     ifelse(mean(Avg.Ad.Length.x) > 0.31 & mean(Avg.Ad.Length.x) <0.60, 0.92,
                                                            ifelse(mean(Avg.Ad.Length.x) > 0.60, 0.92, 0))))))
        
        if(sum(df1$Completed.Views)/sum(df1$impression) > completion_bench){
          msg_hs1 = paste("\nYour campaign Completion%  was higher than Platform B'Marks .Reflection of Engaging creative.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        msg_hs
      }
      else if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == input$brand & df_tvhs$Month == input$month]))==0){
        msg_hs = "Brand not on Hotstar"
        
        msg_hs
      }
    }
    else{
      if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == df_pas$default_brand & df_tvhs$Month == df_pas$default_month]))>0){
        msg_hs = "Hotstar"
        df1 = df_tvhs %>% filter(Brand == df_pas$default_brand, Month == df_pas$default_month, tg == input$tg) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical,Avg.Ad.Length.x,CTR,impression,Completed.Views)
        
        df_vertical = df_tvhs %>% filter(Vertical == df1$Vertical, Month == df_pas$default_month) %>% select(Brand,Frequency,tv_freq,Revenue,campaign_name,Vertical)
        df_vertical[is.na(df_vertical)] = 0
        if(mean(df1$Frequency) < mean(df_vertical$Frequency) | mean(df1$Frequency) < 3 ){
          msg_hs1 = paste("\nYour campaign was under-exposed on Hotstar.",
                          " To build recall and awareness, minimum average frequency should be 3.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        if(mean(df1$Frequency) > 8){
          msg_hs1 = paste("\nYour campaign was over-exposed on Hotstar.",
                          " This may result in fatigue or have negative impact on brand metrics.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        if(mean(df1$Revenue) < mean(df_vertical$Revenue) | mean(df1$Revenue) < 10^5){
          msg_hs1 = paste("\nYour campaign size is under-indexed on Hotstar.",
                          " Recommended campaign size is minimum 10 lakhs and 30+ lakhs for any visible movement in brand metrics.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        ctr_bench = with(df1,
                         ifelse(mean(Avg.Ad.Length.x) > 0.05 & mean(Avg.Ad.Length.x) <0.06,0.04,
                                ifelse(mean(Avg.Ad.Length.x) > 0.10 & mean(Avg.Ad.Length.x) <0.20,0.05,
                                       ifelse(mean(Avg.Ad.Length.x) > 0.21 & mean(Avg.Ad.Length.x) <0.30, 0.06,
                                              ifelse(mean(Avg.Ad.Length.x) > 0.31 & mean(Avg.Ad.Length.x) <0.60, 0.09,
                                                     ifelse(mean(Avg.Ad.Length.x) > 0.60, 0.07, 0))))))
        
        if(mean(df1$CTR) > ctr_bench){
          msg_hs1 = paste("\nYour campaign CTR was ",
                          as.character(round(mean(df1$CTR)/ctr_bench,1)),
                          "X of Average CTR. Reflection of precise targeting or creative with clear call to action.",
                          sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        completion_bench = with(df1,
                                ifelse(mean(Avg.Ad.Length.x) > 0.05 & mean(Avg.Ad.Length.x) <0.06,0.85,
                                       ifelse(mean(Avg.Ad.Length.x) > 0.10 & mean(Avg.Ad.Length.x) <0.20,0.96,
                                              ifelse(mean(Avg.Ad.Length.x) > 0.21 & mean(Avg.Ad.Length.x) <0.30, 0.95,
                                                     ifelse(mean(Avg.Ad.Length.x) > 0.31 & mean(Avg.Ad.Length.x) <0.60, 0.92,
                                                            ifelse(mean(Avg.Ad.Length.x) > 0.60, 0.92, 0))))))
        
        if(sum(df1$Completed.Views)/sum(df1$impression) > completion_bench){
          msg_hs1 = paste("\nYour campaign Completion%  was higher than Platform B'Marks .Reflection of Engaging creative.",sep="")
          msg_hs = paste(msg_hs,str_wrap(msg_hs1,width = 90),sep="\n")
        }
        
        msg_hs
      }
      else if(sum(!is.na(df_tvhs$campaign_name[df_tvhs$Brand == df_pas$default_brand & df_tvhs$Month == df_pas$default_month]))==0){
        msg_hs = "Brand not on Hotstar"
        
        msg_hs
      }
    }
  })
  
  output$image1_1 <- renderImage({
    list(src = "/home/ubuntu/ad_products/Awareness.png",
         contentType = 'image/png',
         width =25,
         height =25,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image1_2 <- renderImage({
    list(src = "/home/ubuntu/ad_products/Sustenance.png",
         contentType = 'image/png',
         width =35,
         height =35,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image1_3 <- renderImage({
    list(src = "/home/ubuntu/ad_products/Engagement.png",
         contentType = 'image/png',
         width =40,
         height =40,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image1_4 <- renderImage({
    list(src = "/home/ubuntu/ad_products/Content Solutions.png",
         contentType = 'image/png',
         width =25,
         height =25,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image2_1 <- renderImage({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod1 = df_ad_prod$Awareness[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    list(src = paste("/home/ubuntu/ad_products/",ad_prod1,".png",sep=""),
         contentType = 'image/png',
         width =80,
         height =80,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image2_2 <- renderImage({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod2 = df_ad_prod$Sustenance[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    list(src = paste("/home/ubuntu/ad_products/",ad_prod2,".png",sep=""),
         contentType = 'image/png',
         width =80,
         height =80,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image2_3 <- renderImage({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod3 = df_ad_prod$Engagement[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    list(src = paste("/home/ubuntu/ad_products/",ad_prod3,".png",sep=""),
         contentType = 'image/png',
         width =80,
         height =80,
         alt = "")
  }, deleteFile = TRUE)
  
  output$image2_4 <- renderImage({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod4 = df_ad_prod$Content.Solutions[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    list(src = paste("/home/ubuntu/ad_products/",ad_prod4,".png",sep=""),
         contentType = 'image/png',
         width =80,
         height =80,
         alt = "")
  }, deleteFile = TRUE)
  
  
  output$ad_product_1 = renderText({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod1 = df_ad_prod$Awareness[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    print(vertical)
    print(as.character(ad_prod1))
    as.character(ad_prod1)
  })
  
  output$ad_product_2 = renderText({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod2 = df_ad_prod$Sustenance[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    as.character(ad_prod2)
  })
  
  output$ad_product_3 = renderText({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod3 = df_ad_prod$Engagement[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    as.character(ad_prod3)
  })
  
  output$ad_product_4 = renderText({
    vertical = unique(df$HS.Vertical[df$Advertiser == input$advertiser])
    ad_prod4 = df_ad_prod$Content.Solutions[df_ad_prod$Vertical == vertical & df_ad_prod$Campaign.objective == input$objective]
    as.character(ad_prod4)
  })
  
}

shinyApp(ui, server)