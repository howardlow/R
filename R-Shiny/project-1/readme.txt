This assignment will demonstrate
a.	your ability to self-discover aspects of R esp. how to summarise / visualise datasets
b.	your grasp of Shiny (I am not looking for advanced Shiny skills here)
c.	your ability to communicate clearly.
The only limitation is that you need to work within the range of packages already loaded onto the workstations. The Internet, Shiny Gallery examples and this course’s examples will serve as the study material for this assignment.

Steps:
1.	Select a dataset. There are several in most R packages - read the package help for information. There are lots in an R package called “datasets”, likewise “carData”. Look for one that is potentially interesting. Maybe it has many variables. Maybe it has mixed types: factors and numeric. The dataset must have more than 5 columns. Please, no synthetic data for this assignment.
2.	Discover what ways exist to produce summary statistics. Which are most appropriate for this task. How do you want to present this - perhaps the summary stats can be conveyed visually? Is counts of observations with missing data important? Can you anticipate what the Y variable is? Do you do anything special with the Y variable? Have you thought about covariance (or correlation) matrix? Is the data full of outliers? - then maybe some “Robust” statistics might be useful.. Feel free to use more than one set of summary statistics.
3.	Discover what R packages and tools are appropriate for visualising an entire dataset. Is there something unusual about this data that needs to visually stand out? Have you thought about some way of showing outliers in (say) a 2 dimensional plot? Would a hierarchical dendrogram be useful? Feel free to use more than one visualisation. Data that is all factors might use a "mosaic" visualisation.
4.	Create a Shiny App that will serve as a showcase for your dataset. Maybe a “tabset” will be a good way of keeping everything organised on a single screen without scrolling. Will you need any checkboxes, selectors etc?
5.	When you are finished test it out on a fresh instance of R, (use Session / Restart R in the Rstudio menu)
6.	Use the course LEARN website to upload the ui.R & server.R files.

