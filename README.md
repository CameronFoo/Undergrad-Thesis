# The London Laundromat: The Effects of Transparency on The London Offshore Property Market

Following the Russian invasion of Ukraine in February 2022, investigative reports have
revealed the scale of corruption and money laundering rooted in the London property market.
In this study, I measure the impact of the Economic Crime Bill (ECB), which marks
the attempt made by the UK government to introduce greater transparency and expose
the anonymous ownership of property. Taking advantage of data on property transactions
made by overseas companies, I show that the implementation of the ECB has had a marginal
effect in altering the investment behaviours of various stakeholders based in overseas territories
in the short run. I also do not find any strong evidence of long-run behavioural
alternations following the policy introduction. This research underscores the limitations of
current legislative measures in addressing opaque ownership structures. It highlights the
need for stronger enforcement and political accountability to combat the exploitation within
the London property market.

*This repository contains my undergraduate thesis (PDF), the LaTeX source files, and R code for analysis.



## Contents

\- `thesis.pdf`: Final submitted dissertation

\- `latex/`: LaTeX source files (from Overleaf)

\- `scripts/`: R scripts used for data analysis

\- `data/`: Place to put public datasets (not included here)

\- `outputs/`: Generated tables and figures



## Data

This project uses public data from \Overseas companies ownership data (OCOD).  

Download it from: \https://use-land-property-data.service.gov.uk/#ocod 

Then place it in the `data/` folder before running the R scripts.



## How to reproduce

1\. Install R (>= 4.x).
2\. Install required packages:

&nbsp;  ```r

&nbsp;  install.packages(c("here","readr","dplyr"))



