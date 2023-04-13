# Inland Empire Assembly Bill Analysis Dashboard

## Introduction

This dashboard is largely based off of the [Warehouse CITY](https://radicalresearch.shinyapps.io/WarehouseCITY/) tool developed in collaboration by [Redford Conservancy at Pitzer College](https://www.pitzer.edu/redfordconservancy/) and [Radical Research LLC](http://radicalresearch.llc). That tool allows additional quantification capabilities. 

This dashboard provides a visualization of the warehouses in the Inland Empire that meet the criteria to be included in proposed AB 1000 or AB 1748 for warehouse setback legislation.  

## Legislative Assumptions 

AB 1000 requires 1,000 foot setbacks from warehouses over 100,000 square feet. 
AB 1748 requires 300 foot setbacks from warehouses over 400,000 square feet.

## Dashboard Assumptions

The Warehouse database we have assembled is based on parcel lot line measurements of area. Warehouse building square footage can be calculated or estimated based on the floor area ratio of the building to other lot uses (docking doors, parking lots, trailer lots, and landscaping).  The user can provide a floor area ratio between 0.4 and 0.65 to see how the summary statistics and warehouse map changes based on those assumptions.  

Buffer polygons around warehouses are based on parcel polygons (i.e., lot lines) not docking doors or building locations.  This assumption is based solely on data available the parcel lot lines are the only information directly available. This is the most conservative assumption possible to assess the potential impact on neighboring sensitive receptors, as distances to dock doors or buildings would necessarily be further away and thus the buffers will overestimate the restrictions.  

This map also displays planned warehouses. Each maroon polygon is either an individual warehouse or warehouse complex which will contain multiple warehouses (e.g., World Logistics Center, Stoneridge Commerce Center, and Speedway Commerce Center). A full database of planned warehouses and their footprints are based on CEQANET queries and a collaborative open source effort to draw over 120 individual projects.  This is not a comprehensive list for the entire Inland Empire but does have all known large projects over 4 million square feet.  If you know additional projects that you'd like added to this list, please contact us and we'll try to add it.   

## Contact and Support

-   If you have questions or suggestions, please email [mikem@radicalresearch.llc](mailto:mikem@radicalresearch.llc){.email}

-   If you are interested in supporting or becoming a member of local organizations opposing warehouse development adjacent to residential communities, please check out [RNOW](https://tinyurl.com/RIVNOW), Center for Community Action and Environmental Justice [(CCAEJ)](https://www.ccaej.org/), and the [Redford Conservancy at Pitzer College](https://www.pitzer.edu/redfordconservancy/).
