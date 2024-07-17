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
**Vulnerability = Variable Value / Maximum Variable Value** <br>
- The Variable Value is census-tract specific and the Maximum Variable Value is for all census tracts.
- To avoid inaccuracies, our model only calculates vulnerability scores for census tracts containing values for all variables. 

# How do I run the code?

### Setup:
1. Make sure you have git installed (Learn how to do that [here](https://github.com/git-guides/install-git)).  
2. Install R Studio (Learn how to do that [here](https://posit.co/download/rstudio-desktop/)).  
3. Create a folder that will hold all the files for this project.
4. Open `RStudio`.
5. In `RStudio` click the little icon with an R in a cube on the top left. (You can also create a new project under the file tab at the top of the screen.)
6. Then press Version Control.
7. Then press Git.
8. Now, in Github find the repository and go to the code tab.
9. Press the green code button and copy the URL.
10. Back in RStudio, paste the URL in the repository URL selection.
11. You can either leave the Project directory name blank to auto fill or add whatever you want to name it then press create project.
12. Download the `data` folder from Google Drive, and move it to the project's directory.
The data folder can be downloaded via [this link](https://drive.google.com/drive/folders/1ks9Fw4AURkjYdjR4NuIfG3biKIZOZfSO?usp=drive_link).
13. Open the `requiredpackages.R` then click the button in the code section that says `Source`.
14. **IF YOU ARE NOT USING THE INCLUDED .RData FILE:** Follow Steps 15-17.
15. Open `coordinategen.R`.
16. Enter a google api key where it says PUT YOUR GOOGLE MAPS API KEY HERE! on line 53 of `coordinategen.R` and click the button in the code section that says `Source`. (you can get a geocoding API [here](https://developers.google.com/maps/documentation/geocoding/get-api-key)).
17. Run `coordinategen.R` by clicking the button in the code section that says `Source`. This **will take time** which is why we **recommend against** running this script to generate `total_lender_info.RData`.
18. Open `tennesseemain.R` and click the button in the code section that says `Source`.
19. In the folder `BetterFiDashBoard`, open `app.R` and click `Run App`.

### File Descriptions
- `BetterFiDashBoard` - The dashboard is in this folder. Simply open app.R and run it after running `tennesseemain.R`.
- `misc scripts and files` - This folder contains previous R scripts used for testing code before implementing into the final script and dashboard.
- `.gitignore` - This file specifies which files should not be pushed to github.
- `betterfi-2024.Rproj` - This file stores the project settings.
- `requiredpackages.R` - This file installs all of the necessary packages to run all of our other scripts.
- `coordinategen.R` - This file generates `total_lender_info.RData`, which contains this data for our other scripts.
- `tennesseemain.R` - This file creates two datasets, `tn_data.RData`. Then `tn_data.RData` is consolidated into `tn_tract_dash.RData`, which is the dataset used by the dashboard.
