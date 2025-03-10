#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(bslib)
library(dplyr)



# theming options
surgitel_theme <- bs_theme(version = 5,
                           base_font  = font_google("Work Sans"),
                           bg = "white",
                           fg = "#1f0900",
                           primary = "#004793",
                           secondary = "#FFB300")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    shinybrowser::detect(),
    # Your application UI logic
    page_fluid(


      titlePanel("",
                 tags$head(tags$link(rel = "icon", type = "png", href = "icons/logo-small.png"),
                           tags$title("Surgitel Laser Safety"))),

      theme = surgitel_theme,
      card(

        fluidRow(column(12,align='center',
                        h5(strong("Search eye protection by selecting a loupe style and laser device"))))
      ),
      fluidRow(
        column(
          4,
          align = 'center',
          selectInput(
            inputId = "loupestyle",
            label = h4(strong("Surgitel Frame")),
            choices = sort(surgitel_data$`Surgitel Frame`),
            selected = 1
          )
        ),
        #        column(
        #          3,
        #          align = 'center',
        #          selectInput(
        #            inputId = "style",
        #            label = h4(strong("Loupe Style")),
        #            choices = sort(surgitel_data$`Style`),
        #            selected = 1
        #          )
        #        ),
        column(
          4,
          align = 'center',
          selectInput(
            inputId = "mfg",
            label = h4(strong("Manufacturer")),
            choices = sort(dental_data$`Laser Mfg`),
            selected = 1
          )
        ),
        column(
          4,
          align = 'center',
          selectInput(
            inputId = "mod",
            label = h4(strong("Model")),
            choices = dental_data$`Laser Model`,
            selected = 1
          )
        )),
      fluidRow(

        column(
          12,
          align = "center",
          br(),
          actionButton("run",
                       # remove                       icon = icon("magnifying-glass"),
                       #                       style='padding-left:50px;padding-right:50px;padding-top:1px;padding-bottom:1px; font-size:80%',
                       style='padding-left:30px;padding-right:30px;padding-top:5px;padding-bottom:1px; font-size:30%',
                       h5(strong("Search")),
                       class = "btn-secondary"))
      ),
      br(),
      fluidRow(
        column(12,
               p("Your information not available in the dropdowns? Contact Innovative Optics at (763)425-7789"))
      ),
      conditionalPanel(
        condition = "input.run",
        card(fluidRow(column(12, align = "center",
                             h3(style = {
                               "color: #004793;"
                             },
                             em("Device Information")),
                             #tableOutput("userInfo"))
                             uiOutput("userInfoUI")),
        ),
        fluidRow(column(12,
                        align = "center",
                        h3(style = {
                          "color: #004793;"
                        },
                        em("Compatible Innovative Optics Product")),
                        uiOutput("tableInfoUI"))),
        #tableOutput("tableInfo"))),
        #fluidRow(column(6, align = 'center',
        #                 imageOutput("productImageF")),
        #          column(6, align = 'center',
        #                 imageOutput("productImageB")))
        ),
        ################################################################################
        ###6/27/24 Edited by Melissa Rich
        ## Remove Back picture
        ################################################################################
        fluidRow(column(12,align = 'center',
                        uiOutput("order_link"))),
        fluidRow(column(12,id = 'productImageDiv', align = 'center',
                        #style ="scale: 75%;"
                        imageOutput("productImageF",
                                    height = "75%"
                                    #width = '50%'
                        ))),

        #        card(class = "box-shadow",
        #             fluidRow(column(12,
        #                             align = 'center',
        #                             h3(style = {
        #                               "color: #004793;"
        #                             },
        #                             em("Frequently Purchased Together")))),
        #             fluidRow(
        #               column(4, align = 'center',
        #                      imageOutput("rec1"),
        #                      tableOutput("tableRec1")),
        #               column(4, align = 'center',
        #                      imageOutput("rec2"),
        #                      tableOutput("tableRec2")),
        #               column(4, align = 'center',
        #                      imageOutput("rec3"),
        #                      tableOutput("tableRec3")))
        #        )
      ),
      card(card_footer(h5(
        style = {
          "color: #FFB300;
                         text-shadow: 1px 1px 1px black;"
        },
        "Powered by Innovative Optics")))
    ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

#I'm not sure why this function is not working. But added at the beginning at the top
#where it works with titlePanel function - checking with Rachdyan to find out why.
# - 1/27/25, MLR
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    #  favicon(),
    bundle_resources(
      path = app_sys("app/www/icons"),
      app_title = "Surgitel Laser Safety"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #   tags$link(href="https://uploads-ssl.webflow.com/642bc00aa11863508034d79d/css/refractives.webflow.3cbf59264.min.css", rel="stylesheet", type="text/css")

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
