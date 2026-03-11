# REACH-Postprocessing

### File Names
+ Post-processing (Python)
  + *ML_R&B_EMA_v3.ipynb*: Version 3 (most updated version) of the REACH EMA post-processing Python script. Can run both with and without the special SFTP files for the NIMH Rhythms & Blues study (to make it generalizable to other teams).
+ Quality control and compliance (R)
  + *RnB_QC_Sample_v2.R*: Version 2 (most updated version) of the R script for quality control and compliance calculation (after post-processing with ML_R&B_EMA_v3). Cleans only EMA data (and not saliva or other supplemental data).
+ Codebook
  + *REACH - FAV_Codebook.xlsx*: Codebook for Full Adult Version of REACH.

### Previous versions
+ Post-processing (Python)
  + *ML_EMA_Reformat_v2.ipynb*: Original version of the post-processing script, designed for NIMH test/dev EMA applet ("Revised NIMH EMA").
  + *HBN_EMA_Reformat.ipynb*: Post-processing script reworked for the Healthy Brain Network (HBN) EMA.
  + *ML_R&B_EMA.ipynb*: Original post-processing script for NIMH Rhythms & Blues EMA data, adapted from the Revised applet.
  + *ML_R&B_EMA_v2.ipynb*: Version 2 of the NIMH R&B EMA post-processing script to account for export changes made by MindLogger/Curious team in Spring 2025.
+ Quality control and compliance (R)
  + *RnB_QC_Sample.R*: Original script for quality control and compliance calculation of R&B EMA and saliva data (after post-processing with ML_R&B_EMA_v2).

### Processing pipeline
1. Export raw applet data from Mindlogger and save response.csv files (‘Input Files’ > 'EMA_applet_data').
2. [Only for R&B team] Export flow-items.csv, activity-schedule.csv, and flow-schedule.csv from Curious SFTP server and save (‘Input Files’).
3. Run the Input Files through the post-processing script stored in this Github repository (ML_EMA_R&B_v3.ipynb)
4. Save all output files (‘Output Files’).
5. Run the "flow_final.csv" output file through the QC script stored in this Github repository (RnB_QC_Sample_v2.R)
6. The QC script may flag observations that need to be reviewed. Follow instructions and example from this spreadsheet: https://docs.google.com/spreadsheets/d/1OtNgUgDHD3kM7j5szJLGK0Dumf4ijH-0Yrf7FvF4Gio/edit?usp=sharing. Document cases to be reviewed in a tracking spreadsheet, including solutions and notes ("Reviewing participants" tab ). Participants who need to be flagged for missing assessments, dropouts, or tech issues should be documented as well along with an explanation for the flag ("Flag cases" tab).
7. Make the corresponding inclusion/exclusion changes in the QC script.
8. Finish running the QC script and obtain compliance measures. You can see previous compliance statistics for R&B data in subsequent tabs on the above Google spreadsheet.
9. Save the cleaned dataset and compliance report (‘Cleaned data’), along with the scripts used (‘Scripts’).
10. If there are participants with compliance <25%, add them to ("Low comp px") sheet of the tracking spreadsheet and add any notes.
