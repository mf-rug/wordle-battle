library(wordle)
library(utf8)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(DT)
library(tidyverse)
# library(rdrop2)

# debg <- TRUE
# forceword <- 'EDGED'


fluidPage(
  useShinyjs(),
  # HTML("<script src='https://cc.cdn.civiccomputing.com/9/cookieControl-9.x.min.js'></script>"),
  tags$style(HTML("div#out img {width: 90vw; height: auto; max-height:3000px; max-width: 1350px;}")),
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$style(HTML("
      .header {
          display: flex;
          max-width:600px;
          justify-content: space-between;
          margin: 0 auto;
        }
      ")),
    # here comes the cookie stuff
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=G-ZQTHGTNKW7'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-ZQTHGTNKW7', { cookie_flags: 'SameSite=None;Secure' });
</script>"),
#cookie control for user
# tags$script(src = "cookie_control_config.js"),
  ),
  extendShinyjs(text = 'shinyjs.getidcookie = function(params) {
                          var time = Cookies.get("id");
                          Shiny.onInputChange("cookie", time);
                        }',
                functions = "getidcookie"),
  

  
  fluidRow(div(style = 'line-height:0.3',br()),div(style = 'height:0.3vh', class='header', 
                                                   div(style="display: inline-block;vertical-align:top", HTML('&nbsp&nbsp')),
                                                   div(style="display: inline-block;vertical-align:top", HTML('<a href = "https://shiny.fuerstlab.com/wordle_battle_en/"><span style="background-color:#e7e3e3;">🇬🇧</span></a>'), HTML('<a href = "https://shiny.fuerstlab.com/wordle_battle_de/">🇩🇪</a>'), HTML('<a href = "https://shiny.fuerstlab.com/wordle_battle_es/">🇪🇸</a>')),
                                                   div(style="display: inline-block;vertical-align:top",htmlOutput('title')),
                                                   div(style="display: inline-block;vertical-align:top",HTML('<a href="https://www.fuerstlab.com" target="_blank"><span title="This R shiny app was created by Max Fürst."><i class="fa fa-question-circle"></i></span></a>')),
                                                   div(style="display: inline-block;vertical-align:top", HTML('&nbsp&nbsp'))
  )),
  br(),
  HTML('<hr style="width:300px; margin-left: auto; margin-top: 4px; margin-bottom: 4px" />'),

  fluidRow(div(style = 'margin-top:0; text-align:center', 
               div(style = "display: inline-block;vertical-align:top", htmlOutput('message')), 
               div(style = "display: inline-block;vertical-align:top", HTML('&nbsp'), hidden(actionBttn('restart', HTML('<i class="fa fa-redo"></i>'), size = 'xs', style = 'gradient', color = 'danger'))
  ))),
  
  HTML('<hr style="width:300px; margin-left: auto; margin-top: 4px; margin-bottom: 0px" />'),
  
  
  plotOutput('squares',  height='50vh', width = '100%'),
  div(id = 'keyb',
      fluidRow(div(style = 'text-align:center; line-height:3',
                   tags$table(style = "width: 100%",
                              tags$tr(
                                tags$td(
                                  style = "width: 100%",
                                  align = "center",
                                  div(style="display: inline-block;vertical-align:top",uiOutput('Q')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('W')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('E')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('R')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('T')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('Y')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('U')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('I')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('O')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('P')),
                                  div(style="display: inline-block;vertical-align:top",uiOutput('Ue'))
                                ))))
      ),
      
      fluidRow(div(style = 'text-align:center; line-height:3',
                   tags$table(style = "width: 100%",
                              tags$tr(
                                tags$td(
                                  style = "width: 100%",
                                  align = "center",
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('Ph')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('A')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('S')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('D')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('F')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('G')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('H')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('J')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('K')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('L')),
                                  hidden(div(id = 'njehide', style = "display: inline-block;vertical-align:top", uiOutput('Nje'))),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('Oe')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('Ae'))
                                ))))
      ),
      fluidRow(div(style = 'text-align:center; line-height:3',
                   tags$table(style = "width: 100%",
                              tags$tr(
                                tags$td(
                                  style = "width: 100%",
                                  align = "center",
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('Enter')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('Z')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('X')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('C')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('V')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('B')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('N')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('M')),
                                  div(style = "display: inline-block;vertical-align:top", uiOutput('bs'))
                                )))))
  ),
  hr(),HTML('<div style = text-align:center;><strong><big><big>Score board</big></big></strong></div>'), 
  HTML('<hr style="margin-top: 10px; margin-bottom: 10px" />'),
  fluidRow(column(plotOutput('out', width = '90vw', height='90vh'), width=12, align='center'))
)


