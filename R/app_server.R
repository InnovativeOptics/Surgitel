#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


library(dplyr)
# list loupe image paths to filter from
loupe_image_paths <- tibble("LoupeImages" = list.files(path = "www/SurgitelLoupeImages/"))

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

googledrive::drive_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")
googlesheets4::gs4_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")

sheet_id <- googledrive::drive_get("Dental_data")$id

surgitel_data <- googlesheets4::read_sheet(sheet_id, sheet = "Loupe_types", col_types = "c") %>%
  filter(`Mfg` == 'Surgitel Current Models') %>%
  rename(`Surgitel Frame` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number` )



# Load dental data
lens_data <- googlesheets4::read_sheet(sheet_id, sheet = "Lens_details") %>%
  select(-VLT)



dental_data <- googlesheets4::read_sheet(sheet_id, sheet = "laser_info", col_types = "c") %>%
  filter(`Laser Mfg` != "") %>%
  select(-Website) %>%
  #mutate(VLT = scales::percent(as.numeric(VLT))) %>%
  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))


app_server <- function(input, output, session) {
  # The application server logic
  observeEvent(input$mfg,{
    # filter dental data to select mfg
    mfg_filtered_dental_data <- dental_data %>%
      filter(`Laser Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(mfg_filtered_dental_data$`Laser Model`))
  })



  loupe_insert <- eventReactive(c(input$loupestyle ),{
    result <- surgitel_data %>%
      filter(`Surgitel Frame` == input$loupestyle) %>%

      distinct()

    print("\nLoupe Insert")
    print(result)
    result
  })

  selected_data <- eventReactive(c(input$loupestyle, input$mod),{
    req(input$mfg)
    result <- dental_data %>%
      filter(`Laser Mfg` == input$mfg,
             `Laser Model` == input$mod) %>%
      distinct() %>%
      DentalLibrary::generate_lens_link(loupe_insert = loupe_insert())



    print("\nSelected Data")
    print(result)
    result
  })
  user_info <- eventReactive(input$run,{
    result <- tibble(
      "Surgitel Loupe Style" = loupe_insert()$`Surgitel Frame`,
      "Laser Information" = glue::glue_safe(selected_data()$`Laser Mfg`, " ", selected_data()$`Laser Model`),
      "Laser Specifications" = selected_data()$Wavelengths) %>%
      distinct()

    #print(result)
    result
  })

  output$userInfo <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 {
                                   user_info()
                                 })

  table_info <- eventReactive(input$run,{

    result <- tibble("INVO Part Number" = selected_data()$`INVO Part Number`,
                     "Optical Density Specifications" = selected_data()$`Optical Density`,
                     "Visible Light Transmission" = selected_data()$VLT)

    print("\ntable info")
    print(result)
    result
  })


  output$tableInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  height="100%",
                                  {
                                    table_info()
                                  }, sanitize.text.function = function(x) x)
  rec1_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec1`)
  })
  output$tableRec1 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec1_table()
                                  })
  rec2_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec2`)
  })
  output$tableRec2 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec2_table()
                                  })
  rec3_table <- eventReactive(input$run,{
    tibble("INVO Part Number" = selected_data()$`Rec3`)
  })
  output$tableRec3 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  {
                                    rec3_table()
                                  })
  image_location <- eventReactive(input$run,{
    req(input$loupestyle)
    req(input$mfg)
    req(input$mod)
    loupe_rec <- loupe_image_paths %>%
      filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "", input$loupestyle)) &
              # stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "", input$style)) &
               stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
               )
      )

    loupe_rec_front <- loupe_rec[loupe_rec %>% stringr::str_detect('FRONT')]



    if(length(loupe_rec_front) > 1){
      front_dist <- stringdist::stringdist(input$loupestyle, loupe_rec_front)
      loupe_rec_front <- loupe_rec_front[which.min(front_dist)]
    }



    loupe_rec_side <- loupe_rec[loupe_rec %>% stringr::str_detect('SIDE')]



    if(length(loupe_rec_side) > 1){
      side_dist <- stringdist::stringdist(input$loupestyle, loupe_rec_side)
      loupe_rec_side <- loupe_rec_front[which.min(front_dist)]
    }



    loupe_rec_top <- loupe_rec[loupe_rec %>% stringr::str_detect('TOP')]



    if(length(loupe_rec_top) > 1){
      side_dist <- stringdist::stringdist(input$loupestyle, loupe_rec_top)
      loupe_rec_top <- loupe_rec_front[which.min(front_dist)]
    }
    print("\nLoupe Rec")
    print(loupe_rec)

    result <- c(glue::glue_safe("www/SurgitelLoupeImages/", loupe_rec$LoupeImages[[1]]),
                if_else(selected_data()$`Eyewear Lens Compatible` %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
                ),
                if_else(selected_data()$`Eyewear Lens Compatible` %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
                ),
                glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"))
    #}

    print("\n Image Location")
    print(result)

  })
  output$productImageF <- renderImage({
    list(src = image_location()[[1]],
         width = "400px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
  # output$productImageB <- renderImage({
  #   list(src = image_location()[[5]],
  #        width = "400px",
  #        contentType = "image/png")
  # }
  # ,deleteFile = FALSE)

  output$rec1 <- renderImage({
    list(src = image_location()[[2]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec2 <- renderImage({
    list(src = image_location()[[3]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec3 <- renderImage({
    list(src = image_location()[[4]],
         height = "300px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)
}

