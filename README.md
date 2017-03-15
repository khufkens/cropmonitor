# cropmonitor

The IFPRI crop monitor R package calculate greenness indices (Gcc and GRVI) for cellphone based data acquisitions within the context of cellphone based index-based micro-insurance.

The tool allows for the visualization of the database, but requires the population of a local database (see below).


## Installation

clone the project to your home computer using the following command (with git installed)

```R
require(devtools)
install_github("khufkens/cropmonitor") # install the package
library(cropmonitor) # load the package
```

## Use

Data is generated based upon a database dump provided as a .dta file (this format can, and should be changed for transparency reasons).

If no local database exists the local database is generated and populated. The local database is called cropmonitor.json and by default located in the your home directory in an automatically generated data folder called **~/cropmonitor**. If the cropmonitor.json file exits, it is compared with the provided .dta file and updated if necessary.

In addition, the raw image data will be downloaded from the IFPRI servers onto your workstation. The raw data is stored in a subfolder in the data folder. This folder is called **'images'** and matching thumbnails are stored in a **'thumbs'** folder. A final **'output'** folder is also created to hold the output of plotting functions. This folder will not be generated until the plotting function is called for the first time. The file structure is outlined in the graph below. A different path for data storage can be specified, but is not recommended.

```
/user/testuser/cropmonitor
	│   cropmonitor.json
	└─── images
	│   └───user_id
	│       │───cropsite_id
	│       │   ...
	|
	└─── thumbs
	|   └───user_id
	|       │───cropsite_id
	|       │   ...
	|
	└─── output
 		    figure.pdf
			 ...
	
```

The thumbnails have annotations on them pertaining to the automatically selected region of interest (ROI) and the horizon line which assists in this process.

![](https://github.com/khufkens/cropmonitor/blob/master/inst/data/thumb.jpg?raw=true)

```R
# How to update the local database
batch.process.database(database = "Pictures Data CLEAN 03_03_17.dta")

```

Once the database is generated operations on the data are quickly plotted using the following function call.

```R
# Plotting the data without additional information
plot.sites(database = "~/cropmonitor/cropmonitor.json")
```

In addition to plotting data as simple time series, one can include field surveys as collected together with the image acquisitions in the field. If an XLSX file is provided, this data will be merged and visualized.

```R
# Plotting the data without additional information
plot.sites(database = "~/cropmonitor/cropmonitor.json",
			 questionaire = "~/cropmonitor/questionaire.xlsx)
```

Finally to access the interactive graphical user interface for exploring the data execute the following command.

```R
# Start the graphical user interface, to explore the data
cropmonitor()
```