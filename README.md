# Canvas LMS Data Pipeline

This R repository provides a data pipeline for accessing and processing data from the Canvas LMS (Learning Management System) using the [vvcanvas](https://github.com/vusaverse/vvcanvas)  R package. The `vvcanvas` package is maintained by the same organization as this repository and provides a convenient interface to the Canvas LMS API.

## Getting Started

To use this project, you'll need to have the following set up:

-   R and RStudio: This project is designed to be used with R and RStudio.
-   Packages: The project's start-up script will install and load the necessary packages, including `vvcanvas`.
-   Environment Variables: You'll need to set the CANVAS_BASE_URL and CANVAS_API_KEY environment variables in your .Renviron file. These are required for the `vvcanvas` package to authenticate with the Canvas LMS API.

## Data Pipeline

The data pipeline in this project consists of the following steps:

-   `00_download`: Fetch data from the Canvas LMS API and saves it as R objects.
-   `01_audit`: Read the downloaded data and performs various auditing tasks to ensure data quality.
-   `02_edit`: Transform the data, such as converting time-stamps to academic year using a specialized function. (Note: The academic year convention of September 1st to August 31st may not be universal, and other organizations may use different conventions.)
-   `03_aggregate`: Generate analysis sets, such as course-level, department-level, and student-course-year-level data.
-   `05_report`: Load the aggregated data-sets with the purpose of data analysis and visualization.

## Contributions

Contributions to this repository are welcome. We hope to receive input from users at other organizations to help grow this project into a best practice for working with R and the Canvas LMS API. If you encounter any issues or have suggestions for improvements, please feel free to submit them as GitHub issues in this repository.

For issues regarding the package, please submit your issues there.
