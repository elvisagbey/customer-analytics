# Customer Segmentation - R Shiny App

<img src="https://i.imgur.com/5CD2fij.png" />

<h2>Description</h2>
Customer segmentation is the process of dividing a broad consumer or business market, normally consisting of existing and potential customers, into sub-groups up based on common characteristics – such as demographics or behaviors, so your marketing team or sales team can reach out to those customers more effectively.
<br></br>
This project demonstrates how R Shiny Apps may be used to automate customer segmentation using advanced clustering algorithms such as K-means.

<h2>Languages and Utilities Used</h2>

- R programming language 
- [List of libraries](https://github.com/graphshade/Customer-segmentation---R-Shiny-App/blob/master/renv.lock)

<h2>Environments Used </h2>

- <b>Ubuntu 18.04.1 LTS</b>

<h2>Program walk-through:</h2>

<p align="left">

1. [Install R and RStudio](https://techvidvan.com/tutorials/install-r/)
 
2. Clone the project: Run this from the command line
 
 ```commandline
 git clone https://github.com/graphshade/customer_segmentation_shiny_app.git
 ```
 
3. Install Required Libraries Using Virtual Environment: 
   
   You may install the libraries directly on your computer however, using the virtual environment library `renv`. [Follow this guide to install renv](https://www.youtube.com/watch?v=yc7ZB4F_dc0)
   1. Open the app.R file in RStudio
   2. In the RStudio console run `renv::init()` to initiate the renv virtual environment and install the required libraries from the [renv.lock](https://github.com/graphshade/Customer-segmentation---R-Shiny-App/blob/master/renv.lock) file 

4. Run the app
 From the left corner of your RStudio, click on <kbd> <br> Run App
 
   When the app run properly, you'll see

   <img src="https://i.imgur.com/tLHZa7K.png" />

 5. Following the instructions from the left panel, you may upload the `sample_dataset.csv` file
 
 6. After uploading the dataset, you can use tha tabs in the right panel to view the results
 
    <b>Customer Segmentation Results Tab</b>
 
    <img src="https://i.imgur.com/GI53Iel.png" />
 
    <b>Cluster Visualization Tab</b>
 
    <img src="https://i.imgur.com/5CD2fij.png" />
 </p>
 
