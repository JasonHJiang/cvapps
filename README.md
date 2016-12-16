# cvapps

An overview of the data framework and the applications.

Todo:
- [ ] ensure all writes are made using hcwriter, and then grant read access to hcreader (see GPS table code)
- [ ] ensure all reads made using hcreader, and the `pool` library?

## Fetching the data ([data_import](data_import))
Input to these scripts are a data source, the output is data tables being uploaded to the Postgres database. These could be automated in the future for automatic updating of data sources.

- [data_import/meddra_import.R](data_import/meddra_import.R) needs to be run in order to collect the different versions of MedDRA hierarchies (from zip files) and have them in a searchable table. This table is currently uploaded to the PostGRES database.
(In this script, it is also joined to the cv_drug_rxn table so that we can quickly group reports by HLT. SOC is already present in CV data.)

- [data_import/cv_import.R](data_import/cv_import.R) scrapes the CV extract landing page for the link to the extracts zip file, downloads it locally, and parses through the text files, constructs the `AGE_GROUP, DATRECEIVED_CLEAN, DATRECEIVED_CHAR, DATINTRECEIVED_CLEAN, DATINTRECEIVED_CHAR` columns, and uploads all the tables to the Postgres database with the date suffix

## Fetching the data ([data_processing](data_processing))
These scripts read tables from Postgres, do processing and calculations, some table joins, and uploads tables back to Postgres. These tables are the tables which AREN'T straight from the raw data.

- [data_processing/main_calcs.R](data_processing/main_calcs.R) The main script for calculations of disproportionality statistics. Some may be directly from formulas, some may be based on the `PhViD` library. Currently uses only reports after 2006 to calculate PRR, ROR, RRR, RFET, BCPNN, and GPS for both preferred terms (PT) and high-level terms (HLT)

- [data_processing/stats_functions.R](data_processing/stats_functions.R) Contains refactored versions of functions from PhViD. Either gives the same result (with less overhead, and strings are not cast into factors) or adds additional data columns, so then we don't have to run multiple times for multiple stats (can be calculated at same time) or to get an upper bound.

- [data_processing/quarterly_script.R](data_processing/quarterly_script.R) A lot of commented out code that should work to calculate cumulative quarterly count tables. Results not currently used.

## Apps ([apps](apps))
These are used for exploring and analyzing the data.
