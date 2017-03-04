tags$html(
  tags$head(
    tags$title('About page')
  ),
  tags$body(
    tags$h2('The AmerifluxR package'),
    tags$p('The AmerifluxR package provides functions to easily query and visualize Level2 processed Ameriflux data from the Ameriflux servers. Data will be downloaded if available (gap filled or raw). If no data is available, please contact the site PI if the site is listed. Presence in the table does not constitute available open access data or complete data files. Additional tools will be developed over time to increase the package functionality, in particular viewing of Level3 data.'),
    tags$p('I appreciate any help in the development of the package, especially development on Windows machines is difficult due to limit access to such hardware.'),
    tags$h3('FAQ / remarks'),
    tags$ul(
      tags$li('The sites can be geographically constrained by clicking top left / bottom right on the map'),
      tags$li('The map might load slowly as it pulls in metadata from the Ameriflux server\'s javascript based site table (no API?). Subsequent loads will be faster as the data is cached, but will be automatically refreshed yearly.'),
      tags$li('All data is stored in a local "ameriflux_cache" folder in your home directory. You may delete this folder at any time (although it will be recreated when starting the gui).'),
      tags$li('Mark the "refresh" tick box to re-download all data and refresh the data stored in your cache directory.'),
      tags$li('The NEE phenology plot type displays start and end of season as well as the growing season length (SOS/EOS/GSL) based on source-sink transition dates.'),
      tags$li('The site years listed on top of the map are estimates using back filled dates. If the final date is not provided, continuous measurements until present are assumed (creating a bias towards high estimates)'),
      tags$li('For continued development consider buying me coffee by tipping my tip jar on my',tags$a(href="http://www.khufkens.com/downloads/", "software page",target="_blank"),'.'),
      tags$li('... or cite / acknowledging the package.')
    )
  )
)
