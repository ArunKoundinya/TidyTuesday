import pandas as pd

from dfply import *

from plotnine import *

import ssl

import numpy as np

import seaborn as sns
import matplotlib.pyplot as plt

import statsmodels.api as sm


ssl._create_default_https_context = ssl._create_unverified_context

trashwheels = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

##plot1

( trashwheels >> 
 group_by(X.Year) >>
 summarize(Weights=np.sum(X.Weight), Volumes=np.sum(X.Volume)) >> 
 ggplot(aes(x = "Year",y="Weights",fill="Volumes")) + 
 geom_col() +
 theme_minimal()
 )

##plot2
(trashwheels >>
 gather("variable", "value", ["PlasticBottles", "Polystyrene", "CigaretteButts", "GlassBottles", "PlasticBags", "Wrappers", "SportsBalls"]) >>
 group_by(X.Year, X.variable) >>
 summarize(Values=np.sum(X.value)) >>
 group_by(X.variable) >>
 mutate(Standardized_Values=(X.Values - X.Values.mean()) / X.Values.std()) >>
 ggplot(aes(x="Year", y="Standardized_Values")) +
 geom_line() +
 facet_wrap('~variable') +
 theme_minimal()
)

##plot3
( trashwheels >> 
 group_by(X.Year) >>
 summarize(HomesPowered=np.sum(X.HomesPowered)) >> 
 ggplot(aes(x = "Year",y="HomesPowered")) + 
 geom_line() +
 theme_minimal()
 )


##plot4 { Picked from internet}

selected_columns = ["Weight", "PlasticBottles", "Polystyrene", "CigaretteButts", "GlassBottles", "PlasticBags", "Wrappers", "SportsBalls"] 
selected_data = trashwheels[selected_columns]

scaler = StandardScaler()
scaled_data = scaler.fit_transform(selected_data)

# Create a DataFrame with scaled data and original column names
scaled_df = pd.DataFrame(scaled_data, columns=selected_columns)

# Calculate the correlation matrix for scaled data
correlation_matrix_scaled = scaled_df.corr()

# Create a heatmap
plt.figure(figsize=(10, 8))
sns.heatmap(correlation_matrix_scaled, annot=True, cmap='coolwarm', fmt=".2f", linewidths=.5)
plt.title('Correlation Heatmap (Scaled Data)')
plt.show()



## Excluding this as no good relationship observed
selected_data = selected_data.dropna()

X = selected_data[["PlasticBottles", "Polystyrene", "CigaretteButts", "GlassBottles", "PlasticBags", "Wrappers", "SportsBalls"]]
y = selected_data["Weight"]
X_with_const = sm.add_constant(X)

model = sm.OLS(y, X_with_const)
results = model.fit()
print(results.summary())