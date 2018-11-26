# Purpose: Load and process Kenya budget data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_26
# Audience: Kenya Mission


# load data ---------------------------------------------------------------

excel_sheets(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"))
budget_2015 <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"), sheet = "Budget Nos 15-16")
budget_2016 <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"), sheet = "Budget Nos 16-17")

map(list(budget_2015, budget_2016), ~str(.))


# Variable creation for analysis and visualization ------------------------

# Calculate # of categories per county
# Total for each column, per county
# 
