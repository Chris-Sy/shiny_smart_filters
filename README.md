# Smart Filters for R Shiny
Module to enable user-selectable cascading filters in R Shiny

Tools like Tableau Desktop or Excel Pivot Tables allow users to easily select what fields they want to filter their data by without any hard coding.  The smart filters here allow you to provide that functionality within a shiny app. 

Shiny typically requires you to manually create an input/filter for every field you allow to be filtered. If you have 2 filters and want the selection of filter 1 to limit the options of filter 2, you have to follow a pattern like:

filter1 -> reactive() filtered by filter 1 -> filter2 -> reactive() filtered by filter2. 

This module's methodology allows you to skip creating that reactive chain and only requires you to specify: 
1. fields you want to allow filtering 
2. required fields that cannot be filtered.

The filters are cascading so the selection of one filter will determine the options available within the subsequent filters

# One filter selected
![image](https://user-images.githubusercontent.com/22123843/158874469-a23f07c6-6ac5-4a1c-ab47-c6356fb95a70.png)

# Two filters selected
![image](https://user-images.githubusercontent.com/22123843/158874696-75cc58bd-56e8-492f-b765-6e6806337d1b.png)

# Methods
Typically with modules in Shiny you will have 1 UI/input function and 1 corresponding server-side function.  Here I am using 1 UI function: smartFilterInput and 2 server-side functions: smartFilterServer and smartDfServer.

smartFilterInput is really only displaying the output of smartFilterServer

smartFilterServer is doing all of the work to generate the options available in the filters and to render the corresponding UI elements

smartDfServer is 'picking up' the filters and applying them to the dataframe. The output of callModule(module = smartFilterServer, ... ) is a reactive data frame that can be used however needed.  In the demo app it can be downloaded or viewed in the table.

![image](https://user-images.githubusercontent.com/22123843/158876321-239f1658-3550-4367-a7b3-9f8e94967405.png)

