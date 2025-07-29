# coastline data from naturalearth

require(rnaturalearth)
require(ggplot2)
require(dplyr)

coastline <- rnaturalearth::ne_states(iso_a2=c("CA","US","GL","PM"),returnclass = "sf") |> 
  select(admin,name_en,name_fr,geometry)



usethis::use_data(coastline, overwrite = TRUE)

sinew::makeOxygen(coastline, overwrite = TRUE)

# ggplot(canadanatural) +
#   geom_sf() +
#   coord_sf(xlim = c(-80, -30), ylim = c(35, 70), expand = FALSE) +
#   labs(title = "Coastline of Canada and the United States",
#        subtitle = "Natural Earth Data",
#        caption = "Source: rnaturalearth package")
