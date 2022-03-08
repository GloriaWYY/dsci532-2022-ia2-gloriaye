library(dash)
library(dashBootstrapComponents)
# library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(readr)

app <-
  Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


crime <- read_csv("data/crime_clean.csv")

#----------------------to be moved
crime <- crime %>% mutate(CRIME_CATEGORY = crime_category) %>% select(-crime_category)

# crime <- crime  %>%
#   mutate(
#     CRIME_CATEGORY = case_when(
#       TYPE %in% c("Offence Against a Person",
#                   "Mischief",
#                   "Homicide") ~ "Violent crimes",
#       TYPE %in% c(
#         "Theft from Vehicle",
#         "Break and Enter Commercial",
#         "Break and Enter Residential/Other",
#         "Theft of Bicycle",
#         "Theft of Vehicle",
#         "Other Theft"
#       ) ~ "Property crimes",
#       TYPE %in% c(
#         "Vehicle Collision or Pedestrian Struck (with Injury)",
#         "Vehicle Collision or Pedestrian Struck (with Fatality)"
#       ) ~ "Vehicle collision"
#     )
#   )  %>%
#   mutate(
#     TYPE = case_when(
#       TYPE == "Vehicle Collision or Pedestrian Struck (with Fatality)" ~ "Vehicle collision, Fatal",
#       TYPE == "Vehicle Collision or Pedestrian Struck (with Injury)" ~ "Vehicle collision, Injured",
#       TRUE ~ TYPE
#     )
#   )
#----------------------to be moved

tab_style = list(
  "borderBottom" = "1px solid #d6d6d6",
  "color" = "white",
  "padding" = "6px",
  "backgroundColor" = "#010915"
  # 'fontWeight' = 'bold'
)
tab_selected_style = list(
  "borderTop" = "4px solid #d6d6d6",
  "borderBottom" = "4px solid #d6d6d6",
  "borderLeft" = "4px solid #d6d6d6",
  "borderRight" = "4px solid #d6d6d6",
  "backgroundColor" = "#010915",
  "color" = "white",
  "padding" = "6px",
  "fontWeight" = "bold",
  "fontSize" = 20
)

app$layout(dbcContainer(list(
  dccTabs(
    id = "crime_category-widget",
    value = "All",
    children = list(
      dccTab(
        label = "All",
        value = "All",
        style = tab_style,
        selected_style = tab_selected_style,
      ),
      dccTab(
        label = "Violent crimes",
        value = "Violent crimes",
        style = tab_style,
        selected_style = tab_selected_style,
      ),
      dccTab(
        label = "Property crimes",
        value = "Property crimes",
        style = tab_style,
        selected_style = tab_selected_style,
      ),
      dccTab(
        label = "Vehicle collision",
        value = "Vehicle collision",
        style = tab_style,
        selected_style = tab_selected_style,
      )
    )
  ),
  dccGraph(id = 'bar-plot-1'),
  dccDropdown(
    id = 'neighbourhood',
    options = levels(as.factor(crime$NEIGHBOURHOOD))  %>%
      purrr::map(function(col)
        list(label = col, value = col)),
    value = 'West End'
  )
)))

app$callback(output('bar-plot-1', 'figure'),
             list(
               input("crime_category-widget", "value"),
               input('neighbourhood', 'value')
             ),
             function(crime_category, neighbourhood) {
               if (crime_category == "All") {
                 p <- crime %>%
                   filter(NEIGHBOURHOOD == neighbourhood)  %>%
                   dplyr::add_count(TYPE)  %>%
                   ggplot(aes(y = reorder(TYPE, n), text = n)) +
                   geom_bar(fill = '#aec7e8') +
                   ggtitle(paste("Total Reported Cases by Crime Types in", neighbourhood)) +
                   labs(x = "Number of crime cases", y = "Type of crime") +
                   theme_classic() +
                   theme(
                     plot.background = element_rect(fill = "#010915"),
                     panel.background = element_rect(fill = "#010915"),
                     # panel.grid.major = element_blank(),
                     # panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "#FFFFFF"),
                     axis.text.y = element_text(color = "#FFFFFF"),
                     axis.title.x = element_text(face = "bold", color = "#FFFFFF"),
                     axis.title.y = element_text(face = "bold", color = "#FFFFFF"),
                     title = element_text(face = "bold", color = "#FFFFFF")
                   )
               }
               else {
                 p <- crime  %>%
                   filter(NEIGHBOURHOOD == neighbourhood &
                            CRIME_CATEGORY == crime_category)  %>%
                   dplyr::add_count(TYPE)  %>%
                   ggplot(aes(y = reorder(TYPE, n), text = n)) +
                   geom_bar(fill = '#aec7e8') +
                   ggtitle(paste(
                     crime_category,
                     ": Total Reported Cases by Crime Types in",
                     neighbourhood
                   )) +
                   labs(x = "Number of crime cases", y = "Type of crime") +
                   theme_classic() +
                   theme(
                     plot.background = element_rect(fill = "#010915"),
                     panel.background = element_rect(fill = "#010915"),
                     # panel.grid.major = element_blank(),
                     # panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "#FFFFFF"),
                     axis.text.y = element_text(color = "#FFFFFF"),
                     axis.title.x = element_text(face = "bold", color = "#FFFFFF"),
                     axis.title.y = element_text(face = "bold", color = "#FFFFFF"),
                     title = element_text(face = "bold", color = "#FFFFFF")
                   )
               }
               ggplotly(p, tooltip = 'text')
             })

app$run_server(host = '0.0.0.0')