import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import Dash, dcc, html
import dash_bootstrap_components as dbc
import requests
from io import StringIO

df = pd.read_csv('diwali_sales_data.csv', encoding='ISO-8859-1')

df.columns = df.columns.str.strip().str.replace(" ", "_").str.replace(r"[^\w\s]", "")

Diwali_Sales = (
    df.groupby(['User_ID', 'Gender', 'Age_Group', 'Marital_Status', 'State', 'Product_Category'], as_index=False)
         .agg(Total_Orders=('Orders', 'sum'), Total_Spends=('Amount', 'sum'))
)

# Adding the Count column just like in R
Diwali_Sales['Count'] = 1
# --- Preprocessing ---
Diwali_Sales['Total Spends (Lacs)'] = Diwali_Sales['Total_Spends'] / 100000

df = Diwali_Sales.copy()
# --- KPI Cards ---
def kpi_card(title, value, icon, color):
    return dbc.Card([
        dbc.CardBody([
            html.H6(title, className='card-title'),
            html.H4(value, className='card-text'),
            html.I(className=f"bi bi-{icon} fa-2x text-{color}")
        ])
    ], className='m-2 shadow rounded-3')

# --- Box Plot: Total Orders vs Total Spends ---
fig1 = px.box(df, x="Total_Orders", y="Total_Spends", color_discrete_sequence=['#00A6A6'])
order_counts = df.groupby("Total_Orders").size().reset_index(name='count')

for i, row in order_counts.iterrows():
    fig1.add_trace(go.Scatter(
        x=[row['Total_Orders']], y=[df[df['Total_Orders']==row['Total_Orders']]['Total_Spends'].max()],
        mode='text',
        text=[f"n={row['count']}"],
        textposition="top center",
        showlegend=False
    ))
fig1.update_layout(title="Spending per Order (₹)", template="plotly_white")

# --- Box Plot: Age Group vs Total Spends ---
fig2 = px.box(df, x="Age_Group", y="Total_Spends", color_discrete_sequence=['#F25C54'])
age_counts = df.groupby("Age_Group").size().reset_index(name='count')

for i, row in age_counts.iterrows():
    fig2.add_trace(go.Scatter(
        x=[row['Age_Group']], y=[df[df['Age_Group']==row['Age_Group']]['Total_Spends'].max()],
        mode='text',
        text=[f"n={row['count']}"],
        textposition="top center",
        showlegend=False
    ))
fig2.update_layout(title="Spending by Age Group (₹)", template="plotly_white")

# --- Initialize App ---
app = Dash(__name__, external_stylesheets=[dbc.themes.LUX, "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.0/font/bootstrap-icons.css"])

app.layout = dbc.Container([
    html.H1("Diwali Sales Dashboard", className="text-center my-4"),
    
    dbc.Row([
        dbc.Col(kpi_card("Total Customers", df.shape[0], "people", "primary"), md=2),
        #dbc.Col(kpi_card("Gender Ratio", f"{(df['Gender'].value_counts(normalize=True)['Female']*100):.1f}% Female", "gender-ambiguous", "info"), md=2),
        dbc.Col(kpi_card("Married Customers", df[df['Marital_Status']=='Married'].shape[0], "heart-fill", "danger"), md=2),
        dbc.Col(kpi_card("Total Orders", df['Total_Orders'].sum(), "basket", "success"), md=3),
        dbc.Col(kpi_card("Spends (Lacs)", f"₹ {df['Total Spends (Lacs)'].sum():.1f}", "currency-rupee", "warning"), md=3),
    ], justify="center"),

    html.Hr(),

    dbc.Row([
        dbc.Col(dcc.Graph(figure=fig1), md=6),
        dbc.Col(dcc.Graph(figure=fig2), md=6),
    ])

], fluid=True)

if __name__ == '__main__':
    app.run(debug=True)
