# tjsp_app

This is a shiny app, *a.k.a.* Judicial Waze. 

Given a particular filter, we make a cluster analysis to group courts with similar productivity patterns. This app can be used to compare groups of courts that are more or less productive based. 

To run this app, you will need to install the following packages

```r
install.packages("shiny")
install.packages("leaflet")
install.packages("tidyverse")
install.packages("rlang")
install.packages("sf")
install.packages("plotly")
# install.packages("devtools")
devtools::install_github("trestletech/shinyTree")
```

After that, you can run the app this way

```r
shiny::runApp()
```

You can also see it running accessing [this link](http://52.71.228.16:3838/waze/).

## TODO

- [ ] cron job to update monthly
- [ ] more areas
- [ ] more productivity metrics
- [ ] predict number of disputes by court
