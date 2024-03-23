Welcome to `censusprofileR`, a package designed to simplify regionalized census data capture. While `censusprofileR` can perform several functions, at its core, it takes a geographic point, draws a radius, and makes calls to the census api for geographical units around within that radius, and provides output that is more suited for presentation.

All census calls are made via the US Census API. As `censusprofileR` relies on the census API, you will need a census API key. 

Currently, censusprofileR uses the American Community Survey 5-Year estimates (ACS). Other data is available (Decennial, ACS 1-Year, PUMS data). Implementation of these additional data sources remains a TODO for the package.

This package is currently under development.
