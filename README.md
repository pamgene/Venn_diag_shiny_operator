# Venn_diag_shiny_operator

##### Description

`Venn_diag_shiny_operator` is an application that allows users to interact with the venn graphics. 

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, y value per cell 
`row`           | factor, variables
`column`        | factor, observation IDs 


##### Details

* Typically this operator is used with a cross-tab view containing p-values, but other values will work as well.
* Each column of the cross-tab view should contain an observation (e.g. experimental treatment).
* If the app is run an interactive viewer opens with the Venn diagram. Here the user can edit the threshold (e.g. p < 0.05) for including observations for creating the Venn diagram.
* The Venn diagram can be saved as an image by right clicking on the image and clicking "Save image".
* Creating a Venn diagram is feasible for up to 4 observations, so the cross-tab view cannot contain more than 4 columns. The app will not work if there are any missing values in the data.

##### See Also

[Venn_diag_shiny_operator](https://github.com/tercen/Venn_diag_shiny_operator)
