# Pearson-Education-Indicator

These are codes for making the server.R and ui.R files for the Pearson Education Indicator app.

ui.R collects inputs of cnt1, cnt2 and cnt3 as well as edi1, edi2 and edi3, which are the countries selected for study as well as the indicators chosen. 

snapshot is a variable that is input via a numericInput tool in the second tab. 

range refers to the range of years selected for study.


server.R performs subsetting of the data, as well as correlation and rendering of the text, tables and ggplots for display in ui.R.
