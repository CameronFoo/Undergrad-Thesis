\# Undergraduate Thesis



This repository contains my undergraduate thesis (PDF), the LaTeX source files, and R code for analysis.



\## Contents

\- `thesis.pdf`: Final submitted dissertation

\- `latex/`: LaTeX source files (from Overleaf)

\- `scripts/`: R scripts used for data analysis

\- `data/`: Place to put public datasets (not included here)

\- `outputs/`: Generated tables and figures



\## Data

This project uses public data from \Overseas companies ownership data (OCOD).  

Download it from: \https://use-land-property-data.service.gov.uk/#ocod 

Then place it in the `data/` folder before running the R scripts.



\## How to reproduce

1\. Install R (>= 4.x).

2\. Install required packages:

&nbsp;  ```r

&nbsp;  install.packages(c("here","readr","dplyr"))



