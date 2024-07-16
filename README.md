# Stop Predatory Lending
## What is Predatory Lending?
The Federal Consumer Protection Bureau estimates that, on average, 12 million Americans each year will take out predatory loans to cover an emergency expense. Predatory lending is a practice that takes advantage of low-income communities by exploiting their need for quick cash. Many lenders provide short-term loans that tend to be misleading and often have interest rates over 300%. These loans lead to a cycle of debt and only create more financial stress on the borrowers. 

## Who or what is BetterFi?
We have partnered with BetterFi, a non-profit Community Development Financial Institution organization that offers affordable installment loans and coaching to help individuals break free from debt traps and become financially independent. We have been working with BetterFi to combat the predatory lending crisis in Hamilton and Rutherford counties by determining the most susceptible areas in these counties. 

## Data
We obtained our data from 2 main sources: 
- the Tennessee Depart of Financial Institutions to get information on licensed lenders in Tennessee, including their: 
  - Company names
  - Street addresses
  - Company and branch IDs

- the American Community Survey for demographic data.

The demographic variables we extracted from the ACS include census-tract percentages of individuals who are: 
- Citizens
- Veterans
- Hispanic or Latino
- African American
- Divorced
- Unemployed
- High-school graduates <br>

And the area's
- Average median household income
- Median gross rent

# Final Product
## Dashboard 
Our final product is a dashboard that has 3 main sections. 
1. Interactive graphs
2. Interactive maps
3. Interactive vulnerability model

### Interactive Graphs Tab
The interactive graphs tab allows the user to examine correlations between any 2 of our variables.

### Interactive Maps Tab
The interactive maps tab allows the user to examine a map of any variable by county, e.g. Percentage of people who graduated high school in Bledsoe County.

### Interactive Vulnerability Model
The Interactive Vulnerability Model tab allows the user to choose which variables to examine in our model and set weights for them as well as choosing which county, in order to determine the most vulnerable census tracts in the selection. Depending on which factors the user selects it can change which census tracts are the most vulnerable determined by those variables.

#### How is Vulnerability Determined?
- To determine each census tract's vulnerability we took each census tract's unique attributes as it pertains to each of our variables. 
- Based on these variables, we can determine the vulnerability of each census tract and compare across the census tracts. 
- This vulnerability score can be calculated for each variable within each census tract by using the following equation: <br> 
**Vulnerability = Variable Value (tract specific) / Maximum Variable Value (all tracts).** <br>
- An **important note** is that each census tract must contain a nonzero value for each selected variable for the vulnerability score to be calculated. 

# How do I run the code?

### Setup:
1. Install `RStudio` (Learn how to do that [here](https://github.com/git-guides/install-git)).
2. Create a file that will hold all the files for this project.
2. Open `Rstudio` and click the little icon with a r in a cube on the top left.
3. Then press Version Control.
4. Then press Git.
5. Now in Github find the repository and go to the code tab.
6. Press the green code button and copy the URL.
7. Paste the URL in the repository URL selection.
8. You can either leave the Project directory name blank to auto fill or add whatever you want to name it then press create project.
9. Download the included `data` folder, and move it to the project's directory.
10. Open the `requiredpackages.R` then run it.
11. **IF YOU ARE NOT USING THE INCLUDED .RData file:** Follow 12-14
12. Open `coordinategen.R`.
13. Enter a google api key where it says PUT YOUR GOOGLE MAPS API KEY HERE! on line 53 of `coordinategen.R` and run it (you can get a geocoding API [here](https://developers.google.com/maps/documentation/geocoding/get-api-key)).
14. Run `coordinategen.R`
15. Open `tennesseemain.R` and run it.
16. In the folder `BetterFiDashBoard` open `app.R` and click `**Run App**`

### Preparing the lender info as coordinates:
- To convert data from licensed lender location information into coordinates that can be plotted on a map you need to run `coordinategen.R`.<br> 
- It will take around 10-20 minutes to get the latitude and longitude points for each address mapped out but this is necessary for the variable n_lenders later on.<br>
- Additionally, we have tried to provide the user with the `total_lender_info.RData` data file that skips this entire step because it is extremely tedious.<br>
- **IMPORTANT**: To save this dataset and call on the others, the user must have a folder in their project directory called "data".

### Creating the data file for the dashboard:
- In order to pull variables from the American Community Survey that the dashboard can use, you need to run `tennesseemain.R`. <br>
- This generates `tn_data.RData`, one of our main datasets with all of our key variables.<br>
- `tennesseemain.R` also generates `tn_tract_dash.RData` which is a dataset that is used by the dashboard and is a little cleaner for viewing purposes.

### Running the dashboard:
- To run the dashboard, you need the following 3 .RData files, `tn_tract_dash.RData`, `tn_data.RData`, and `total_lender_info.RData`. <br>
- Once those 3 are in their proper file directory for the dashboard, the dashboard can be opened in the `BetterFiDashBoard` folder with `app.R`. <br>
- Once `app.R` is opened you can simply click Run App at the top and it should work.

### File Descriptions
- `BetterFiDashBoard` - The dashboard is in here, simply open app.R and run app after running `tennesseemain.R`
- `misc scripts and files` - This includes previous R scripts where we created maps and other testing with our code and model to make sure it was functional before the dashboard.
- `.gitignore` - This file chooses what files and types of files to not push to github.
- `betterfi-2024.Rproj` - This is our Rproject file
- `requiredpackages.R` - This file installs all of the necessary packages to run all of our other scripts.
- `coordinategen.R` - This file generates `total_lender_info.RData`, which is used by our other scripts.
- `tennesseemain.R` - This file creates two datasets, `tn_data.RData` and `tn_tract_dash.RData`, which are both used by the dashboard.
