# Introduction

## Context
Enetwild is a European consortium (https://enetwild.com/) funded by the European Food Safety Authority involving 10 agencies or universities. It aims at aggregating outputs of disease host wildlife monitoring in Europe and harmonising them. The first focus of the consortium is wild boar, due to the spread of African swine fever through Europe. Outputs correspond to data about species occurrences, population abundances, and hunting bags at different scales. To harmonise data, Enetwild adopted the Darwin core standard and proposed some innovation to adjust it to its need, forming the Wildlife Data Model (WLDM) (https://efsa.onlinelibrary.wiley.com/doi/abs/10.2903/sp.efsa.2020.EN-1841 ; https://biss.pensoft.net/article/59120/list/19/). Enetwild project developed a tool to ease regional coordinators in formating productors data into the WLDM. It has been thought to be generic to have the ability to be applied to the Darwin core standard.
  
## App objectives
The WLDM app is developed in the R Shiny language. The standardisation process follows a sequential approach is divided into two main steps: (i) the generic standardisation aims at transforming a user dataset into a tidy data frame according to database standards (i.e. one observation per row, one variable per column), (ii) the specific standardisation aims at transforming the dataset into the WLDM format. 
The generic standardisation into a tidy data frame. Several options are proposed to reformat the data frame (i.e. add new columns, duplicate columns, convert a wide-format data frame into a long-format data frame, convert column values). The step of specific standardisation, more complex, consists in:
•	Converting user field names into Darwin Core standards. This step is very similar to the bddwc unit from the bdverse package. An intermediate concept-based filter helps the user in selecting the right standard name.
•	Dividing the fields into Event, Occurrence and Measurement, which are the WLDM components.
•	For both Event and Occurrence sets of fields:
o	Identifying the different levels
o	Enriching the information by adding standard variables with free or controlled values. The last step allows to visualise and export the formatted dataset, including a view of the structure based on a diagram format.
A last steps to visualise the standardised data frame and the structure.


The full readme is available here:

[User_guide.pdf](https://github.com/fja062/WLDM.standardisation/files/8134116/User_guide.pdf)
