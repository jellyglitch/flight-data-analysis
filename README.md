# COMP 4710 Project: Group 7

**Requirements:**
- [R](https://www.r-project.org/)
- [R Studio Desktop](https://www.rstudio.com/products/rstudio/download/#download)

**Executing our code:**
1. Extract the project folder from the zip file.
2. Install the required software listed above.
3. Extract the zip file that contains the project folder
4. Once installation is complete, in R Studio, click ```File -> New Project``` menu and select ```Existing Project``` from the options. After you click ```Create project```, the project’s files and folders should look like this:

``` bash
.
├── .gitattributes
├── .gitignore
├── .Rhistory
├── jankies.Rproj
├── README.md
├── Console Output - Rules Log.txt
├── src
│   └── script.R
└── training data
    └── DEC_2018.csv
    └── DEC_2019.csv
```
6. From the Files window, open the file ```src/script.R``` and press ```Source``` on the upper right corner of the Source Pane to run the script.

    - Note: Re-running the entire script for multiple times may result in Updating Loading Packages error. If the error message pops up, click ```No``` and run the script line by line.
    - The console in RStudio will show the script's output, which includes the list of rules generated. The output has also been logged into the text file: "Console Output - Rules Log.txt".
