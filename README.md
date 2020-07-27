# Title
Multi-Platform Social Media Use: Little Evidence of Impacts on Adult Well-Being --- Analysis Script

Lohmann, Sophie; Zagheni, Emilio

# About

This script is intended to reproduce the results in the accompanying manuscript and generate several additional figures and tables. All manuscript sentences that include numeric results are therefore reproduced in the script and the paragraph numbering in the script reflects that in the manuscript. 

The analysis is based on the 2016 General Social Survey data and the goal of this project was first, to examine socio-demographic characteristics of people who use many social media platforms (termed "intensive users") and second, to examine whether and under which circumstances multi-platform use is (or is not) associated with well-being.  

The data were obtained from https://gss.norc.org/Documents/spss/2016_spss.zip . The data come in SPSS data format with each row representing a participant and each column representing a variable. In SPSS, each variable additionally has labels as meta-data, which can prompt warning messages when being read into R, but these warning messages about the labels do not impact the content of the variables themselves.

## Installation and Usage

(1) Download and unzip data from: https://gss.norc.org/Documents/spss/2016_spss.zip 

(2) Store the dataset `GSS2016.sav` in the same folder as the scripts `GSS_helperfunctions.R` and `GSS_MultiPlatformUse_Supplementary.Rmd`. Open the `GSS_MultiPlatformUse_Supplementary.Rmd` script in R and make sure that your working directory is set to that folder.

(3) If `run_boot = FALSE`, that folder also needs to include two files with previously saved results of the bootstrap analyses (`boot_output_user` and `boot_output_intensive`). If you instead wish to rerun the bootstrap analyses from scratch, these files do not need to exist, set `run_boot = TRUE` to run the results and then store the output yourself.

(4) The script uses LaTeX to produce a PDF report. If you do not have LaTeX installed and do not wish to install it, change "pdf_document" in line 10 to "html_document" to instead produce an HTML report that does not require LaTeX. 

(5) Knit the Rmd file. In RStudio, you can click the "Knit" button or press Ctrl+Shift+K if the `rmarkdown` package is installed. In base R or other R editors, you can run `rmarkdown::render("GSS_MultiPlatformUse_Supplementary.Rmd")`.

(6) The script produces the following outputs: 

- A PDF report along with a log file from the LaTeX conversion. If you changed the output to HTML in step (4), the script will produce an HTML report instead.
- Two figures in SVG format. To convert them into EMF format that can be inserted into Office Suite documents, you can use a free image editor such as Inkscape.

## Status

Working paper, please cite as:

Lohmann, S., & Zagheni, E. (2020). _Multi-platform social media use: Little evidence of impacts on adult well-being._ PsyArXiv. https://doi.org/10.31234/osf.io/r46nd