# Motivation
I propose a project to forecast energy production from hydropower dams, specifically in regions where the water resources is dominated by snowpack dynamics. There are over 2200 hydropower dams in the United States and energy production is of interest to a broad community including government, utility companies, and commodity traders. Proper management of energy is critical for the economy and citizens' daily lives. Some dams use water resources from neighboring states and countries to produce electricity, thus posing potential for conflict. Accurate characterization of existing hydropower potential will help minimize unnecessary power production from fossil fuel powerplants thereby reducing greenhouse gas emissions. Hydropower is only one part of the energy landscape, but it is particularly sensitive to climate change as evidenced by new weather extremes that affect reservoir operations. Very dry years require reservoir operators to conserve inflows from melting snowpack to maintain an adequate water supply through the summer. In contrast, in very wet years, reservoir operations must manage downstream flood risk. Too much inflow requires operators to spill water rather than allow water more slowly through the turbines to generate electricity; this comes at the cost of millions of dollars. This project proposes to use spatially explicit estimates of snowpack water volume that are publicly available to forecast monthly hydropower production.  The use of this relatively simple predictor is rooted in first principles and a successful model has several implications including for energy markets and power grid operations.

# Data Sources
I downloaded the Snow Data Assimilation System (SNODAS) data product published daily by the National Weather Service's National Operational Hydrologic Remote Sensing Center (https://nsidc.org/data/g02158). This data product includes snow water equivalent (SWE), or the amount of water present in a snowpack, as output from a complex modeling and data assimilation system. The data product is published daily at 1 km resolution for the conterminous United States. It is available 2003-2017. 


I also downloaded the historic monthly electric power generation from the U.S. Energy Information Administration (https://www.eia.gov/electricity/data/eia923/). This provides the monthly power generation in Megawatt-hours for every power plant in the United States from 2006-2017.

# Concept Demonstration Site
I chose to explore the potential for this analysis in the Tuolumne River basin of California. It receives most (>80\%) of its water from snowfall every winter, and the basin drains into the Hetch Hetchy reservoir. The reservoir is controlled by the R C Kirkwood dam. The basin is about 1,000 km**2 and is the primary water supply for the city of San Fransisco. 


# Methods
At a most basic understanding, the magnitude of the snow water volume present on the landscape will dictate the future inflow to a reservoir and potential for hydropower generation. However, as mentioned above, there are several competing factors including the need to store water for use through the summer, the desire to maximize hydropower generation, and the need to mitigate downstream flood risk due to the reservoir overtopping. 


The SWE dataset was extracted from compressed files and cropped and masked to the Tuolumne basin using GIS tools. The power generation data was imported to R from Excel files. The analysis was performed on a water year basis, which spans Oct-Sept to include complete snow seasons, and was limited to 2006-2017 due to data availability.
I investigated the hydropower generation and SWE using graphs and derived two variables from the SWE dataset that are intuitive predictors of hydropower generation: 1. SWE volume in the basin on the first of the month; 2. the change in SWE volume from the month before. Visual inspection also showed a monthly trend so month was included as a third variable. I used bootstrap resampling (with replacement) with a regression model in a randomForest framework to test the model's ability to predict monthly hydropower generation. 


The code, results, and comments from the exploratory analysis can be seen at GitHub (https://github.com/dschneiderch/swe_hydrogeneration/blob/master/scripts/04analyze_swevol-netgen.Rmd). 

