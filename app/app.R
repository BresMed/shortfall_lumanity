# shortfall app

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(waiter)

# load ref df with English life table (ONS) and HRQoL estimates (HSE and MVH) by age and sex 
ref_df = read.csv("./data/ref_df_appended.csv")
mvh_df = read.csv("./data/mvh_df.csv")

# load function to compute life and quality-adjusted life expectancies
source("./R/compQale.R")

# load modal div content
source("./R/modalDivs.R")
source("./R/landingDiv.R")

# intensity_cols = colorRampPalette(c("black","orange","red"))

# rename highchart donwload btn
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)

# consistent digits
fRound <- function(str, digits = 2, width = 2){
  formatC(str, digits = 2, width = 2, format = "f")
}

ui <- fillPage(
  
  # use bootstrap 5
  suppressDependencies("bootstrap"),
  tags$script(src="www/utils.js"),
  tags$script(
    src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
    integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
    crossorigin="anonymous"
  ),
  if(file.exists("www/google_analytics.html")){tags$head(tags$head(includeHTML("www/google_analytics.html")))},
  tags$title("QALY Shortfall Calculator"),
  tags$link(
    href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
    rel="stylesheet",
    integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
    crossorigin="anonymous"
  ),
  # enable shinyjs
  useShinyjs(),
  # load js scripts
  includeScript("./www/utils.js"),
  # load custom css
  includeCSS("style.css"),
  # show loading screen
  use_waiter(),
  waiter_show_on_load(color = "", html = landingDiv()),
  div(
    style = "position:absolute; left:-9999px;",
    actionLink("showNotify","")
  ),
  
  
  div(
    class="main d-flex flex-row", 
    style="min-height: 100%;",
    
    
    # INPUT CONTROL PANEL -----
    
    div(
      class="d-flex flex-column border flex-grow-1 px-3",
      style="min-width: 250px; max-width: 300px; flex-basis: 100px;",
      
      # title
      div(
        class = "h3 mb-5 mt-3 text-center fw-light",
        "QALY SHORTFALL CALCULATOR"
      ),
      
      # inputs
      div(
        class = "d-flex flex-column justify-content-center input-bar",
        
        
        # patient cohort age and % female
        # numeric inputs allow 1 decimal place
        div(
          class = "control-label text-center mb-2  ",
          "Age of the patient population"
        ),
        autonumericInput("start_age", label = NULL, minimumValue = 0, maximumValue = 99, decimalPlaces = 1, value = 0, width = "100%"),
        div(
          class = "control-label text-center mb-2  mt-4",
          "% female in the patient population"
        ),
        autonumericInput("sex_mix", label = NULL, minimumValue = 0, maximumValue = 100, decimalPlaces = 1, value = 50, width = "100%"),
        
        
        # pop norm
        div(
          class = "control-label text-center mb-2  mt-4",
          style = "line-height:1",
          "Select scenario", 
          tags$br(),tags$small("(see",
          actionLink("sources-2", "sources", "data-bs-toggle"="modal", "data-bs-target"="#sourcesModal"),
          "for details)"
        )),
        selectizeInput(
          inputId = "utils", 
          label = NULL, 
          selected = "dsu_2014",
          choices = list(
            "Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "dsu_2014",
            "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "dsu",
            "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "vanHout",
            "Alternative C: MVH value set + health state profiles" = "mvh",
            "Alternative D: MVH value set + HSE 2012+14" = "tto"
          )
        ),
        
        # Remaining QALYs
        div(
          class = "control-label text-center mb-2  mt-4",
          HTML("Remaining QALYs of<br> untreated (discounted)"), 
        ),
        
        div(
          class = "d-flex flex-row align-items-center justify-content-center",
          # style = "white-space: nowrap !important;",
          actionButton("take_1","-", class = "btn-adj mx-3 flex-fill"), 
          autonumericInput(
            inputId = "remaining_qalys", 
            label = NULL, 
            minimumValue = 0, maximumValue = 49, decimalPlaces = 2,
            value = 10, 
            width = "100%"
          ),
          actionButton("add_1","+", class = "btn-adj mx-3 flex-fill"), 
        ),
        
        # discount rate
        div(
          class = "control-label text-center mb-2 mt-4",
          "Discount rate"
        ),
        div(
          class = "d-flex flex-row align-items-center justify-content-center",
          # style = "white-space: nowrap !important;",
          actionButton("take_1_disc","-", class = "btn-adj mx-3 flex-fill"), 
          autonumericInput(
            inputId = "disc_rate", 
            label = NULL, 
            minimumValue = 0, maximumValue = 10,
            decimalPlaces = 1,
            value = 1.5, currencySymbol = "%",
            currencySymbolPlacement = "s",
            width = "100%"
          ),
          actionButton("add_1_disc","+", class = "btn-adj mx-3 flex-fill"), 
        ),
        
        #new option to calculate discount rate either at start of year or at midpoint
        div(
          class = "control-label text-center mb-2 mt-4",
          "Discount factor in each year"
        ),
        div(
          class = "d-flex flex-row align-items-center justify-content-center",
          # style = "white-space: nowrap !important;",
          selectizeInput(
            inputId = "disc_time",
            label = NULL, 
            selected = "start",
            choices = list(
              "Calculated at start of year" = "start",
              "Calculated at midpoint" = "midpoint"
            )
          ),
        ),
        div(
          class = "mt-2 ms-5 align-items-center justify-content-center",
          checkboxInput("no_discount", "No discounting", value = F),
        )
        
      ),
      
        # credits
        HTML('<a 
             id = "credits" 
             style = "cursor: pointer;" 
             class = "credits-container credits small a text-start" 
             href="#" 
             data-bs-toggle="tooltip" 
             title="" 
             data-bs-original-title="Paul Schneider, Simon McNamara, James Love-Koh, Tim Doran, Nils Gutacker. QALY Shortfall Calculator. 2021. https://shiny.york.ac.uk/shortfall"
             >
             &copy; Schneider et al. 2021
             </a>')
    ),
      
        
        
    # RESULTS PANEL ------    
    div(
      class="d-flex flex-row flex-grow-1 flex-wrap align-items-start align-content-start  justify-content-center p-3",
      style="flex-basis: 300px; margin-top: 50px;",
      
      # table card
      div(
        class="res-card w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3",
          # card header
          div(
            class = "fs-4 mb-3 mt-2 ms-2",
            "Remaining QALYS"
          ),
          
          # row 1
          div(
            class = "res-line ",
            div(
              class = "res-left",
              "without the disease: "
            ),
            div(
              class = "res-right",
              div(
                class = "rbadge bg-primary_col",
                textOutput("qales_healthy_txt", inline = T) 
              )
            ),
          ),
          
          # row 2
          div(
            class = "res-line",
            div(
              class = "res-left",
              "with the disease: "
            ),
            div(
              class = "res-right",
              div(
                class = "rbadge bg-primary_col",
                textOutput("qales_ill_txt", inline = T) 
              )
            )
          ),
          
          # row 3
          div(
            class = "res-line",
            div(
              class = "res-left",
              "absolute shortfall: "
            ),
            div(
              class = "res-right",
              div(
                class = "rbadge bg-primary_col",
                textOutput("abs_short_txt", inline = T) 
              )
            ),
          ),
          
          # row 4
          div(
            class = "res-line mb-2",
            div(
              class = "res-left",
              "proportional shortfall: "
            ),
            div(
              class = "res-right",
              div(
                class = "rbadge bg-primary_col",
                textOutput("prop_short_txt", inline = T) 
              )
            )
          ),
          
          # row 5
          div(
            class = "res-line mb-2",
            div(
              class = "res-left",
              HTML('<span class ="qaly-weight-info">
              QALY weight
                <span class="tooltiptext p-4 bg-dark text-light shadow rounded small">
                  <table>
                    <tr>
                      <th>Multiplier</th>
                      <th>% shortfall</th>
                      <th>abs. shortfall</th>
                    </tr>
                    <tr>
                      <td>x 1</td>
                      <td>&lt;0.85</td>
                      <td>&lt;12</td>
                    </tr>
                    <tr>
                      <td>x 1.2</td>
                      <td>0.85-0.95</td>
                      <td>12-18</td>
                    </tr>
                    <tr>
                      <td>x 1.7</td>
                      <td>&#8805;0.95</td>
                      <td>&#8805;18</td>
                    </tr>
                  </table>
              </span>
            
            <sup><small><i class="fa fa-question-circle" role="presentation" aria-label="question-circle icon"></i>
            </small></sup>:</span>'
                   )
            ),
            div(
              class = "res-right",
              div(
                class = "rbadge bg-primary_col",
                textOutput("mltplr_txt", inline = T) 
              )
            )
          ),
          
          # action button
          div(
            class = "d-flex  flex-row justify-content-center flex-wrap w-100 pt-3 mt-3 border-top",
          actionButton("info", "info",icon = icon("info-circle"), class = "btn-info-2 my-2", "data-bs-toggle"="modal", "data-bs-target"="#infoModal"),
          downloadButton("download", "download", icon = icon("download"),class = "btn-info-2 my-2"),
          actionButton("sources", "sources", icon = icon("book"), class = "btn-info-2 my-2", "data-bs-toggle"="modal", "data-bs-target"="#sourcesModal"),
          actionButton("code", "code", icon = icon("code"), class = "btn-info-2 my-2"),
          actionButton("contact", "contact", icon = icon("envelope"),class = "btn-info-2 my-2"),
          )
          
          
        )
      ),
      
      
      
      # figure card
      div(
        class="res-card w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3 w-100",
          
            selectizeInput(
              inputId = "chart_type", 
              label = NULL, width = "100%",
              # selected = "bar",
              choices = list(
                "Select chart type" ="",
                "Absolute shortfall" = "bar",
                "Proportional shortfall" = "pie",
                "Cumulative QALYs" = "cumulative_qalys",
                "HRQoL by year" = "hrqol",
                "Cumulative survival" = "S_cumulativ"
              )),
        highchartOutput("high_chart")
      )
      )
    
      
    )
  ),
  
  
  
  modalDivs()
  
)




server <- function(input, output, session){
  
  # notify users of the last update
  observeEvent(input$showNotify,ignoreInit = F,ignoreNULL = T, {
    showNotification(
      HTML("
           <div>
            <span class=\"fw-bold\">Update! </span>
            <span class=\"fw-light\">28 January 2023</span>
           </div>
           <div>
            New reference case!
           </div>"
           ),
      duration = 10, 
      type = "warning"
      )
    showNotification(
      HTML("
           <div>
            <span class=\"fw-bold\"></span>
            <span class=\"fw-light\">30 January 2023</span>
           </div>
           <div>
            We resolved a minor bug in the code concerning the combination of male and female life expectancy.
           </div>"
      ),
      duration = 10, 
      type = "warning"
    )
  })
  
   # add_1 and take_1 button logics -----
   observeEvent(input$add_1,{
     req(input$remaining_qalys)
     updateAutonumericInput(session, "remaining_qalys", value = input$remaining_qalys+1)
   })
   observeEvent(input$take_1,{
     req(input$remaining_qalys)
     updateAutonumericInput(session, "remaining_qalys", value = input$remaining_qalys-1)
   })
  
  
   # discount logic ---------
   # add_1 disc
   observeEvent(input$add_1_disc,{
     req(input$disc_rate)
     updateAutonumericInput(session, "disc_rate", value = input$disc_rate+0.1)
   })
   observeEvent(input$take_1_disc,{
     req(input$disc_rate)
     updateAutonumericInput(session, "disc_rate", value = input$disc_rate-0.1)
   })
  
   # no discount checkbox
   observeEvent(input$no_discount,{
     if(input$no_discount){
       disable("disc_rate")
       disable("add_1_disc")
       disable("take_1_disc")
       disable("disc_time")
       updateAutonumericInput(session, "disc_rate", value = 0)
     } else {
       enable("disc_rate")
       enable("add_1_disc")
       enable("take_1_disc")
       enable("disc_time")
       updateAutonumericInput(session, "disc_rate", value = 3.5)
     }
   })
  
  
   # reset remaining LE based on start age -----
   observeEvent(input$start_age,{
     req(input$start_age, input$remaining_qalys)
     max_le = 100-(input$start_age)
  
     if(input$remaining_qalys > max_le){
       updateAutonumericInput(session, "remaining_qalys", value = max_le, options = list(maximumValue = max_le))
     } else {
       updateAutonumericInput(session, "remaining_qalys", options = list(maximumValue = max_le-1))
     }
  
   })
  
  
  
  
  
  
   # REACTIVE DATA -----------
   dat = reactiveValues()
  
   observe({
     
     req(input$utils, input$sex_mix, input$start_age, input$disc_rate, input$disc_time)
  
     if(input$utils == "mvh"){
       util_df =   mvh_df
       utils = "tto"
     }
  
     if(input$utils == "vanHout" | input$utils== ""){
       util_df = ref_df
       utils = "cw"
     }
     
     if(input$utils == "tto" | input$utils== ""){
       util_df = ref_df
       utils = "tto"
     }
     
     if(input$utils == "dsu" ){
       util_df = ref_df
       utils = "co"
     }
     
     if(input$utils == "dsu_2014" ){
       util_df = ref_df
       utils = "dsu_2014"
     }
     
     if(input$disc_time == "start"){
       #calculate discount rate at start of year
       disc_adjust = 0
     }
     else {
       #calculate discount rate at midpoint of year
       disc_adjust = 0.5
     }
  
     dat$res = compQale(
       ons_df = util_df,
       prop_female = input$sex_mix/100,
       start_age = input$start_age,
       disc_rate = input$disc_rate/100,
       utils = utils,
       cycle_length_days = 365.25,
       disc_adjust = disc_adjust
     )
  
     dat$shortfall_abs = dat$res$Qx[1] - input$remaining_qalys
  
     dat$shortfall_prop = dat$shortfall_abs / dat$res$Qx[1]
     
     
     dat$q_weight = ifelse(
       dat$shortfall_prop >= 0.95 | dat$shortfall_abs >= 18,
       1.7,
       ifelse(
         dat$shortfall_prop >= 0.85 | dat$shortfall_abs >= 12,
         1.2,
         1
       )
     )
   })
  
  
  
   # TEXT OUTPUTS ---------
   output$qales_healthy_txt = renderText({fRound(dat$res$Qx[1],2)})
   output$qales_ill_txt = renderText({fRound(input$remaining_qalys)})
   output$abs_short_txt = renderText({fRound(dat$shortfall_abs,2)})
   output$prop_short_txt = renderText({paste0(fRound(dat$shortfall_prop*100,2),"%")})
   output$mltplr_txt = renderText({paste0("x ",dat$q_weight)})
   
  
  
  
   # HIGH CHARTS ---------
   output$high_chart = renderHighchart({
     
     req(input$utils, input$sex_mix, input$start_age, input$disc_rate, input$disc_time)
     
     highchart_out() %>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(
           target = "_blank"
           ),
         chartOptions = list(
           chart = list(
             backgroundColor = "white"
           )
         ),
         buttons = list(
           contextButton = list(
             symbol = "download",
             verticalAlign = "bottom",
             horizontalAlign = "left",
             #titleKey = "oinf",
             #          menuItems = NULL,
             onclick = JS("function () {
                     this.exportChart();
                 }")
           )
         )
       )
   })
  
   highchart_out = reactive({
  
     req(input$utils, input$sex_mix, input$start_age, input$disc_rate, dat$shortfall_abs, input$disc_time)
    
     if(dat$shortfall_abs < 0){
       p_error = highchart() %>%
         hc_title(
           text = "Error: QALYs must be lower with the disease.",
           align = "center",
           x=-10,
           verticalAlign = 'middle',
           floating = "true",
           style = list(
             fontSize = "16px",
             color = "#7cb5ec"
           )
         )
       return(p_error)
     }
  
     if(input$chart_type == "pie"){
       short_fall = data.frame(
         type = c("With disease", "% Shortfall"),
         percent = c(100 - dat$shortfall_prop*100, dat$shortfall_prop*100),
         col = c("green","gray")
       )
       
       shortfall_str = paste0(round(dat$shortfall_prop*100,1))
       shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
  
       p1 = highchart() %>%
         hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
         hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
         hc_chart(
           style = list(
             fontFamily = "Inter"
           )
         ) %>%
         hc_tooltip(
           valueDecimals = 1,
           valueSuffix = '%'
         ) %>%
         hc_colors(c("#7cb5ec","gray"))
  
  
       return(p1)
     }
  
  
  
     if(input$chart_type == "bar" | input$chart_type == ""){
  
       short_fall = data.frame(
         name = c("QALYs with disease","Absolute shortfall","QALYs without disease"),
         value = c(input$remaining_qalys,dat$shortfall_abs, max(dat$res$Qx[1])),
         color = c("#7cb5ec","#6d757d","#3e6386"),
         a = c(F,F,T)
       )
  
       shortfall_str = paste0(round(dat$shortfall_abs,2))
       shortfall_str = paste0("Absolute QALY shortfall:<b>",shortfall_str,"</b>")
  
       p1 = highchart() %>%
         hc_add_series(
           data = short_fall, "waterfall",
           pointPadding = "0",
           hcaes(
             name = name,
             y = value, isSum=a,
             color = color
           ),
           name = "QALYs"
         ) %>%
         hc_title(text = shortfall_str, align = "left",x=40,y=20,  verticalAlign = 'top', floating = "true", style = list(fontSize = "16px")) %>%
         hc_chart(
           style = list(
             fontFamily = "Inter"
           )
         ) %>%
         hc_tooltip(
           valueDecimals = 2
         ) %>%
         hc_xAxis(
           categories = short_fall$name
  
         )%>%
         hc_boost(enabled = FALSE)#%>%
       #hc_colors(c("red","#7cb5ec","red"))
  
       return(p1)
     }
  
  
     disc_str = input$disc_rate > 0
  
     y_max = max(dat$res$Qx[1])
     if(input$chart_type == "cumulative_qalys"){
       title = round(max(dat$res$Qx[1]),2)
       title = paste0("QALYs without the disease: <b>",title,"</b>",ifelse(disc_str,"(discounted)",""))
       ytitle = "Cumulative QALYs"
     }
     if(input$chart_type == "hrqol"){
       title = paste0("HRQoL over the lifecourse", ifelse(disc_str,"(undiscounted)",""))
       ytitle = "EQ-5D score"
       y_max = 1
     }
     if(input$chart_type == "S_cumulativ"){
       title = paste0("Cumulative survival")
       ytitle = "S(t)"
       y_max = 1
     } 
  
  
  
  
     # y_max = max(dat$res$Qx[1])
     # y_max = ifelse(y_max>50, 80, ifelse(y_max > 30, 50, 30))
  
     plot_df = data.frame(
       age = dat$res$age,
       var = dat$res[,input$chart_type]
     )
  
  
     highchart(
       hc_opts = list(),
       theme = getOption("highcharter.theme"),
       type = "chart",
       width = NULL,
       height = NULL,
       elementId = NULL,
       google_fonts = getOption("highcharter.google_fonts")
     ) %>%
       hc_add_series(
         plot_df, type = "area",
         name = "Shortfall", color = "#7cb5ec",
         hcaes(x = "age", y= "var"),
         tooltip = list(enabled = FALSE),
         fast = T) %>%
  
       hc_title(
         text = title,
         y = 60, x=-50,
  
         style = list(
           fontSize = "16px"
         )
       ) %>%
  
  
       hc_plotOptions(
         line = list(
           marker = list(
             enabled = "false",
             fillColor = "transparent",
             width = 0,
             height = 0,
             enabledThreshold = 99,
             radius = 1
           )
         ),
         series = list(
           tooltip = list(
             enabled = TRUE,
             followPointer = "true",
             fillColor = "transparent"
           )
         ),
         area = list(
           states = list(
             hover = list(
               enabled = TRUE
             )
           ),
           marker = list(
             enabled = FALSE,
             fillColor = "blue",
             width = 1,
             height = 1,
             enabledThreshold = 10,
             radius = 1
           )
         )
       ) %>%
       hc_xAxis(
         title = list(text = "Age"),
         gridLineColor= 'lightgray',
         gridLineWidth= 1,
         gridLineDashStyle = "Dot",
         tickLength= 10,
         tickWidth= 2,
         tickmarkPlacement= 'between'
       ) %>%
       hc_yAxis(
         title = list(text = ytitle), 
         max = y_max
       ) %>%
       hc_tooltip(
         enabled = TRUE,
         valueDecimals = 2,
         pointFormat = '{point.y} ',
         valueSuffix = ' '
       ) %>%
       hc_chart(
         style = list(
           fontFamily = "Inter"
         )
       ) %>%
       hc_legend(
         enabled = F
       )
  
  
  
   })
  
  
  
   # download handler ------
   output$download <- downloadHandler(
     filename = function() {
       paste("shortfall-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
  
       download_data = data.frame(
         parameter = c(
           "patient age",
           "% female",
           "discount rate",
           "QALYs without disease",
           "QALYs with disease",
           "absolute shortfall",
           "proportional shortfall",
           "QALY weight"
         ),
         value = c(
           input$start_age,
           input$sex_mix,
           round(input$disc_rate,2),
           round(dat$res$Qx[1],2),
           round(input$remaining_qalys,2),
           round(dat$shortfall_abs,2),
           round(dat$shortfall_prop,3),
           dat$q_weight
         )
       )
  
       write.csv(download_data, file, row.names = F)
  
     }
   )

  
}


shinyApp(ui, server)
