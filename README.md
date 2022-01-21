# JoVE-MSU-COVID-EDP
Code for JoVE MSU COVID EDP manuscript.

This code is a Shiny web application. You can run the application by clicking
the 'Run App' button in R.

Find out more about building applications with Shiny here: http://shiny.rstudio.com/

Code expects three files -- a 'column' qPCR output file (plate #1), a 'row' qPCR output file (plate #2), and a master .xlsx file that provides deconvolution for sample barcodes that are pooled in the two qPCR plates.

Output is a spreadsheet with 'positive' or 'negative' calls for each deconvoluted sample. 
