# Author : Arun Koundinya Parasa
# Tidy Tuesday Solution : Diwali Sales Data
# Tidy Tuesday Date: 2023-11-14


# Loading the Libraries

library(tidyverse)
library(scales)
library(sf)
library(gganimate)
library(av)
library(gt)


# Loading the Data

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')

### Initial Summaries


## Recreating Unique ID as the 
house$Unique_ID <- paste(house$User_ID,"_",house$Age,"_",house$Marital_Status,"_",house$Zone,"_",house$Occupation
                         ,"_",house$Product_Category,sep="")

house <- unique(house)

Diwali_Sales <- house |>
  group_by(Unique_ID,Gender,`Age Group`,Marital_Status,State,Product_Category) |>
  summarise(Total_Orders = sum(Orders),Total_Spends = sum(Amount), .groups = "drop")

Diwali_Sales$Count <- 1


## Dashboard Value Boxes
UniqueCustomer <- round(nrow(Diwali_Sales),0)
MarriedCustomers <- sum(Diwali_Sales$Marital_Status)
GenderRatio <-  round((table(Diwali_Sales$Gender)[[1]]/nrow(Diwali_Sales))*100,2)
TotalOrders <- sum(Diwali_Sales$Total_Orders)
TotalSpendsinLacs <- round((sum(Diwali_Sales$Total_Spends,na.rm = TRUE))/100000,1)


## Plots

Diwali_Sales |>
  group_by(Total_Orders) |>
  summarise(Cust = sum(Count)) -> CustomerPerOrder

Diwali_Sales |>
  ggplot( aes(x = as.factor(Total_Orders),y = Total_Spends )) +
  geom_boxplot(staplewidth = 0.25, width = 0.35, outlier.color = "brown4",outlier.alpha = 0.5, outlier.shape = 13 ,
               fill = "green4") +
  scale_y_continuous(labels = comma) +
  theme_minimal() + 
  coord_cartesian( ylim = c(0,45000), expand = FALSE) +
  labs(
    x = "Number of Orders per Customer",
    y = "Total Spending (INR)",
    title = "Distribution of Spending per Customer Order",
    subtitle = "Boxplot shows the range of spending; numbers represent the count of customers"
  ) + 
  geom_rect(
    aes(xmin = 0.85, xmax = 1.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=1, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[1], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 1.85, xmax = 2.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=2, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[2], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 2.85, xmax = 3.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white"  
  ) +
  annotate(
    "text", 
    x=3, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[3], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 3.85, xmax = 4.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=4, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[4], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 4.87, xmax = 5.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=5, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[5], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 5.87, xmax = 6.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=6, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[6], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 6.87, xmax = 7.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=7, 
    y=42000, 
    label = format(CustomerPerOrder$Cust[7], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 3, xmax = 4, ymin = 38000, ymax = 40000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=3.5, 
    y=39000, 
    label = "No. of Customers",  
    size= 3.5, 
    color = "white" 
  ) 


## Plot2

Diwali_Sales |>
  group_by(`Age Group`) |>
  summarise(Cust = sum(Count)) -> CustomerPerOrderByAge

Diwali_Sales |>
  ggplot( aes(x = as.factor(`Age Group`),y = Total_Spends )) +
  geom_boxplot(staplewidth = 0.25, width = 0.35, outlier.color = "brown4",outlier.alpha = 0.5, outlier.shape = 13 ,
               fill = "green4") +
  
  scale_y_continuous(labels = comma) +
  theme_minimal() + 
  coord_cartesian( ylim = c(0,45000), expand = FALSE) +
  labs(
    x = "Age Bracket",
    y = "Total Spending (INR)",
    title = "Distribution of Spending by Age Group",
    subtitle = "Boxplot shows the range of spending; numbers represent the count of customers"
  ) + 
  geom_rect(
    aes(xmin = 0.85, xmax = 1.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=1, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[1], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 1.85, xmax = 2.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=2, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[2], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 2.85, xmax = 3.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white"  
  ) +
  annotate(
    "text", 
    x=3, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[3], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 3.85, xmax = 4.15, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=4, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[4], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 4.87, xmax = 5.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=5, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[5], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 5.87, xmax = 6.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=6, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[6], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white"  
  ) + 
  geom_rect(
    aes(xmin = 6.87, xmax = 7.13, ymin = 41000, ymax = 43000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=7, 
    y=42000, 
    label = format(CustomerPerOrderByAge$Cust[7], big.mark = ",", scientific = FALSE),  
    size= 3.5, 
    color = "white" 
  ) + 
  geom_rect(
    aes(xmin = 5, xmax = 6, ymin = 38000, ymax = 40000 ), 
    fill = "red4", 
    color = "white" 
  ) +
  annotate(
    "text", 
    x=5.5, 
    y=39000, 
    label = "No. of Customers",  
    size= 3.5, 
    color = "white" 
  ) 


## BarChart for Top 10 Categories

Diwali_Sales |>
  group_by(Product_Category) |>
  summarise(Sales = sum(Total_Spends,na.rm = TRUE)) |>
  arrange(desc(Sales)) |>
  head(10) |>
  ggplot(aes(y = reorder(Product_Category, Sales),x = Sales)) + 
  geom_col(width = 0.5, position = "dodge", color = "black",fill = "green4" ) +
  labs(
    title = "Total Spends in top 10 Categories", 
    x = "Total Spends in Lacs (INR)", y = "Product Categories") + 
  theme_minimal() + 
  scale_x_continuous(labels = function(x) comma(x / 1e5)) + 
  theme(panel.grid.minor = element_blank()) 

## Barchart for Gender & Marital Status

Diwali_Sales |>
  mutate(
    Gender_Married = case_when(
      Gender == "F" & Marital_Status == 1 ~ "Married_Female",
      Gender == "F" & Marital_Status == 0 ~ "Single_Female",
      Gender == "M" & Marital_Status == 1 ~ "Married_Male",
      Gender == "M" & Marital_Status == 0 ~ "Single_Male",
    )
  ) |>
  group_by(Gender_Married) |>
  summarise(Sales = sum(Total_Spends,na.rm = TRUE)) |> 
  ggplot(aes(y = reorder(Gender_Married, Sales),x = Sales)) + 
  geom_col(width = 0.35, position = "dodge", color = "black",fill = "green4" ) +
  labs(
    title = "Spends by Gender & Marital Status", 
    x = "Total Spends in Lacs (INR)", y = "Gender & Marital Status") + 
  theme_minimal() + 
  scale_x_continuous(labels = function(x) comma(x / 1e5)) + 
  theme(panel.grid.minor = element_blank()) 


## Pareto Chart

Diwali_Sales |>
  select(Total_Spends,Count) |>
  arrange(desc(Total_Spends)) |>
  mutate(
    CumSpend = cumsum(Total_Spends),
    CumCust = cumsum(Count)
  ) |>
  mutate(
    CumSpendPer = round((CumSpend/sum(Total_Spends,na.rm = TRUE))*100,1),
    CumCustPer = round((CumCust/sum(Count))*100,1)
  ) |>
  ggplot(aes(x = CumCustPer,y=CumSpendPer)) +
  geom_point(size=0.5, colour = "green4") +
  theme_minimal() +
  coord_cartesian( xlim = c(0,100),ylim = c(0,100), expand = FALSE) + 
  labs(
    x = "Percentage of Customers",
    y = "Percentage of Sales",
    title = "29% of Customers Contribute to 50% of Spends"
  ) +
  theme(panel.grid.minor = element_blank()) + 
  geom_vline(xintercept = 29)

## Spends in India Map



INDIA_MAP = st_read("States/Admin2.shp")

india_map = data.frame(INDIA_MAP)
india_map <-
  india_map |>
  select(NAME_1 = `ST_NM`,geometry=`geometry`)

Diwali_Sales |>
  group_by(State) |>
  summarise(Spends = sum(Total_Spends,na.rm = TRUE), .groups = "drop") -> Spends_By_Nation

india_map_state <- merge(india_map,Spends_By_Nation,by.x = "NAME_1",by.y = "State",all.x = TRUE)

india_map_state$SpendsinLacs = india_map_state$Spends/100000

india_map_state = st_as_sf(india_map_state)

create_india_sales_heatmap <- function(data, fill_var, title_text = "Heatmap of Diwali Sales") {
  ggplot(data) + 
    geom_sf(aes_string(fill = fill_var), color = "black") +
    scale_fill_gradient(
      low = "lightgreen",  
      high = "darkgreen",  
      na.value = "grey50"  
    ) +
    theme_void() +
    labs(
      title = title_text,
      subtitle = paste("Category",fill_var,sep=": ")
    ) +
    theme(
      legend.position = c(0.6, 0.2),
      legend.key.size = unit(0.5, "cm"),  
      legend.text = element_text(size = 7)
    )
}

create_india_sales_heatmap(india_map_state, "SpendsinLacs")


## Spends by Categories India Map -> Movie Format


Categories <- unique(Diwali_Sales$Product_Category)

plot_list <- list()

for (Category in Categories) {
  plot_list[[Category]] <- create_india_sales_heatmap(india_map_state, paste0("`", Category, "`"))
}


animation::saveGIF(
  expr = {
    for (i in seq_along(plot_list)) {
      print(plot_list[[i]])
    }
  },
  movie.name = "DiwaliSales.gif",
  interval = 4,
  ani.width = 600,
  ani.height = 700,
  res = 300,
  ani.bg = "white",
  ani.fps = 10
)

av_encode_video(
  input = "DiwaliSales.gif",
  output = "DiwaliSales.mp4",
  framerate = 0.5
)

## Table by each Age Group by Gender_MaritalStatus ;;  Top two categories


Diwali_Sales |>
  mutate(
    Gender_Married = case_when(
      Gender == "F" & Marital_Status == 1 ~ "Married_Female",
      Gender == "F" & Marital_Status == 0 ~ "Single_Female",
      Gender == "M" & Marital_Status == 1 ~ "Married_Male",
      Gender == "M" & Marital_Status == 0 ~ "Single_Male",
    )
  ) |>
  group_by(`Age Group`, Gender_Married, Product_Category) |>
  summarise(Total_Spends = sum(Total_Spends, na.rm = TRUE)) |>
  arrange(`Age Group`, Gender_Married, desc(Total_Spends)) |>
  group_by(`Age Group`, Gender_Married) |>
  slice_head(n = 2) |>
  ungroup() |>
  group_by(`Age Group`, Gender_Married) |>
  summarise(Categories = paste(Product_Category, collapse = ", "), .groups = 'drop') |>
  pivot_wider(names_from = Gender_Married, values_from = Categories) -> Category_table

Category_table |>
  gt() |>
  tab_header(
    title = "Top Categories by Age Group, Gender & Marital Status"
  ) |>
  cols_label(
    `Age Group` = "Age Group",
    `Married_Female` = "Married Female",
    `Single_Female` = "Single Female",
    `Married_Male` = "Married Male",
    `Single_Male` = "Single Male"
  ) |>
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_body(
      columns = c(1,2,3,4,5)
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      columns = c(1,2,3,4,5)
    )
  ) |>
  tab_style(
    style = list(
      cell_text(color = "darkblue", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_text(color = "green4", weight = "bold")
    ),
    locations = cells_title()
  )

