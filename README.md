### What I have done 

1) Imported the different sheets from #KeepitOn STOP Data 2016-2022 (https://docs.google.com/spreadsheets/d/1DvPAuHNLp5BXGb0nnZDGNoiIwEeu2ogdXEIDvT4Hyfk/edit#gid=798303217)
2) Gone through all 1978 obesevation and extracted “state” and “districts” based on “area_name_string.” Manually controlled automated matching, added GDAM level 2 names: https://gadm.org/download_country.html. For state-wide shutdowns, districts have been identified by triangulating information based on URLs supplied in #keepiton’s original data set. If districts where identified they were added manually to the "districts"-column.
3) Merged all files into a full time series from 2016 to 2022, cleaned inconsistencies in each column. 
4) Calculated duration of each shutdown where data was available. 

### Description of Data set

start_date: start date of shutdown in "%Y/%m/%d" format.

end_date: end date of shutdown in "%Y/%m/%d" format.

duration_days: the duration of the shutdown in days (1 day is 24 hrs).

duration_hours: the duration of the shutdown in hours.

country: India.

state: state in India. GDAM level 1 naming. 

districts: district in India. GDAM level 2 naming. 

event: what happened where the shutdown took place. 

area_name_string: original string denoting the area where it took place by #KeepitOn. 

ordered_by: the government authority (Local, State etc.) who issued the shutdown. 

gov_justification: the government justification for the shutdown (if any). 

affected_network: the network affected by the shutdown.

actual_cause: actual cause of the shutdown as estimated by #KeepitOn. 

source_link: source of information (URL).

gov_ack_source: government acknowledgement or official document describing the shutdown (URL).

### Replication 
R code can be run sequentially, starting with script “1_(...)” and ending with script “4_(...)”. This codebook refers to the .rds file "shutdowns_india_2016_22.rds".
