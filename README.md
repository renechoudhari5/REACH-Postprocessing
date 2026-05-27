# REACH-Postprocessing

### File names
+ Processing (Python)
  + *REACH - xxV_processing.ipynb*: Python script for processing EMA data for each version of REACH.
  + *REACH - xxV_processing_requirements.txt*: List of package requirements to run Python processing script.
+ Cleaning, quality control, and compliance
  + *REACH - xxV_QC.R*: R script for cleaning, quality control, and compliance calculations for each version of REACH.
  + *REACH - QC Review Template.xlsx*: Excel template for reviewing and tracking QC changes for all versions of REACH.
+ Codebooks
  + *REACH - xxV_codebook.xlsx*: Codebook for all output variables for each version of REACH.

### Processing pipeline
1. Create a master directory with sub-directories: 'Input Files', 'Output Files', 'Cleaned Data', 'Scripts'.
2. Download the Python processing script for the applicable version of REACH and save in 'Scripts'. Use the requirements.txt file if needed to review and install required packages. Download the R cleaning script for the applicable version of REACH and save in 'Scripts'.
3. Export raw applet data from Curious and save response.csv files (‘Input Files’ > 'EMA_applet_data').
4. Following the template provided in the Sample Data > 'Input Files' folder for the applicable version of REACH, create a schedule-history.csv file with the scheduled dates of each participant's EMA. Store in 'Input Files'.
   + Ensure the participant IDs in the _secret_user_id_ column match those in your data exactly.
   + Enter start and end dates in YYYY-MM-DD format. In Excel, you can use the following to automatically format entire columns: Ctrl+1 > Date > YYYY-MM-DD.
   + If participants completed more than one epoch of EMA, create separate rows for each epoch with corresponding start and end dates.
6. Run the Python processing script using the files in 'Input Files'. The output will automatically populate in 'Output Files'.
7. Run the R cleaning script using the 'flow_final.csv' file from 'Output Files', making updates as needed.
   + Note: The R script may flag observations that need to be reviewed. Follow instructions and example from the [REACH - QC Review Template.xlsx](https://github.com/renechoudhari5/REACH-Postprocessing/blob/main/REACH%20-%20QC%20Review%20Template.xlsx). Document cases to be reviewed in a tracking spreadsheet, including solutions and notes ('Participant review' tab). Participants who need to be flagged for dropouts or technical issues should be documented as well along with an explanation for the flag ('Flag cases' tab).
   + Make any corresponding inclusion/exclusion changes in the R script.
   + Finish running the R script to obtain compliance metrics. Document compliance metrics externally or in tabs of the tracking spreadsheet as needed. If there are participants with compliance <25%, add them to the 'Low comp px' sheet of the tracking spreadsheet and add any notes.
8. After running the R script, the cleaned dataset and subject-level compliance report will automatically populate in 'Scripts' - move these files to 'Cleaned Data'.
9. Save and timestamp your Python and R scripts under 'Scripts'. Duplicate these scripts for future exports.
