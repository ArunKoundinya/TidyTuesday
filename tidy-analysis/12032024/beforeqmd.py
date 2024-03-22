import pandas as pd
from dfply import *
from plotnine import *
import ssl
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm

import spacy
nlp = spacy.load("en_core_web_sm")

ssl._create_default_https_context = ssl._create_unverified_context

data = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')


# datamanipulation

#https://regex101.com used for extracting the 

data = ( data >> 
        mutate( 
               fiscal_sponsor_since = 2022 - X.year_fiscal_sponsor,
               fee = X['fiscal_sponsorship_fee_description'].str.extract(r'(\d[-+]?%?.?\d?%)')
               )
        ) >>  ( 
               mutate(
    fee = X['fee'].str.extract(r'(^\d?+[\.]?\d?+)'))
               )

data['fee'] = data['fee'].astype(float)



## plot1

(data >> 
 ggplot(aes(x= "fiscal_sponsor_since",y="n_sponsored",color="year_fiscal_sponsor")) +
 geom_point() + 
 theme_minimal() + 
 labs(
     x = "Years since Fiscal Sponsorship",
     y = "Number of Sponsored Projects",
     title = "Relationship between Total Sponsorships and Duration of Sponsorship",
     subtitle = "Legend of colors is on Fiscal Sponsorship year",
     caption = "Axis is zoomed to have a better visualization"
 ) +
  guides(color=guide_legend(title="")) +  # Use an empty string to remove the legend title
  theme(
      legend_position=(0.5, 0.85),
      panel_grid_minor_x = element_blank(),
      panel_grid_minor_y = element_blank()
  ) + 
   coord_cartesian( 
    ylim = (0, 400),
    xlim = (0,75)
    ) + 
   annotate(
       "text",
       x = 60,
       y = 200,
       label = "Older Sponsors doesn't mean \n more sponsored projects ",
       color = "brown",
       size = 8
       )
 )


## plot2

(data >> 
 ggplot(aes(x="n_sponsored")) +
 geom_histogram(position="dodge",fill = "#4169E1") + 
 theme_minimal() +
 labs (
     x = "Number of Sponsored Projects",
     y = "",
     title = "Histogram of total Sponsored projects"
 ) + 
 theme(
      panel_grid_major_y = element_blank(),
      panel_grid_minor_x = element_blank(),
      panel_grid_minor_y = element_blank()
  ) + 
   annotate(
       "text",
       x = 200,
       y = 80,
       label = "Most of the sponsors \n sponsored only one/two project ",
       color = "brown",
       size = 8
       )
 )


## plot3

(data >> 
 ggplot(aes(x="fee")) +
 geom_histogram(position="dodge", fill = "#708090") +
 theme_minimal() +
 labs (
     x = "Sponsorship Fee%",
     y = "",
     title = "Histogram of fees associated with fiscal sponsorship",
     caption = "Axis is zoomed to have a better visualization"
 ) +  
 theme(
      panel_grid_major_y = element_blank(),
      panel_grid_minor_x = element_blank(),
      panel_grid_minor_y = element_blank()
  ) + 
   coord_cartesian( 
    xlim = (0, 15),
    ) + 
   annotate(
       "text",
       x = 13,
       y = 50,
       label = "Median range of sponsorship \n fee is b/w 5% & 10% ",
       color = "brown",
       size = 8
       )
 )

