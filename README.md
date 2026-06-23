R-code and data for a paper titled: "How Consumers Pay for Gasoline" by Oz Shy

----------------------------------------------
Instructions:

1) Download the R-code file: "gas_2026_mm_dd.R"
2) Download 10 data files: "dcpc-YEAR-indlevel-public.RDS" and "dcpc-YEAR-tranlevel-public.RDS". YEAR should be replaced with: 2021, 2022, 2023, 2024, and 2025.
3) Place all files in the same folder on your computer.
4) Start R with the R code, and reset the working directory 5 times! To do that, search for #2025_begins, then #2024_begins, down to #2021_begins. In all 5, you will see the old setwd(~xxx/yyy) which you must change to identify where the data files that you just downloaded are located. 

Run the ENTIRE code first (don't forget to change the working directory).

If you are using RStudio, after you run the ENTIRE code first, you can jump to the beginning and end of each table and figure by clicking the list of contents on the left-lower corner.

Note: The working directory needs to be modified 5 times because I place the data from each year in a separate folder/directory. Therefore, if you put all the data files in a single directory, you can modify the code to set the working directory only once (and delete the other setwd(~xxx) commands from the code).
