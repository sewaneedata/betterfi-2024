# Stop Predatory Lending
## What is Predatory Lending?
The Federal Consumer Protection Bureau estimates that, on average, 12 million Americans each year will take out predatory loans to cover an emergency expense. Predatory lending is a practice that takes advantage of low-income communities by exploiting their need for quick cash. Many lenders provide short-term loans that tend to be misleading and often have interest rates over 300%. These loans lead to a cycle of debt and only create more financial stress on the borrowers. 

Who or what is BetterFi?
We have partnered with BetterFi, a non-profit Community Development Financial Institution organization that offers affordable installment loans and coaching to help individuals break free from debt traps and become financially independent. We have been working with BetterFi to combat the predatory lending crisis in Hamilton and Rutherford counties by determining the most susceptible areas in these counties. 

# Final Product
## Dashboard 
Our final product is a dashboard that has 3 main sections, interactive graphs, interactive maps and our interactive vulnerability model. 

### Interactive Graphs Tab
The interactive graphs tab allows the user to examine correlations between any 2 of our variables.

### Interactive Maps Tab
The interactive maps tab allows the user to examine a map of any variable by county, e.g. Percentage of people who graduated high school in Bledsoe County.

### Interactive Vulnerability Model
The Interactive Vulnerability Model tab allows the user to choose which variables to examine in our model and set weights for them as well as choosing which county, in order to determine the most vulnerable census tracts in the selection. Depending on which factors the user selects it can change which census tracts are the most vulnerable determined by those variables.


## How do I run the code?

### 1. Preparing the lender info as coordinates:
To convert data from licensed lender location information into coordinates that can be plotted on a map you need to run coordinategen.R. It will take around 10-20 minutes to get the latitude and longitude points for each address mapped out but this is necessary for the variable n_lenders later on. Additionally, we have tried to provide the user with the **total_lender_info.RData** data file that skips this entire step because it is extremely tedious.

### 2. Creating the data file for the dashboard:
In order to pull variables from the American Community Survey that the dashboard can use, you need to run **tennesseemain.R**, this generates **tn_data.RData**, one of our main datasets with all of our key variables. **tennesseemain.R** also generates **tn_tract_dash.RData** which is a dataset that is used by the dashboard and is a little cleaner for viewing purposes.

### 3. Running the dashboard:
To run the dashboard, you need the following 3 .RData files, **tn_tract_dash.RData**, **tn_data.RData**, and **total_lender_info.RData**, once those 3 are in their proper file directory for the dashboard, the dashboard can be opened in the **BetterFiDashBoard** folder with **app.R**, and once **app.R** is opened you can simply click Run App at the top and it should work.
