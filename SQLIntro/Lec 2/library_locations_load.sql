drop table library_locations;

load data local infile "/Users/katieovens/Documents/DATA 604/Week 2/Demos_and_Exercises/Calgary_Public_Library_Locations_and_Hours.csv"
into table library_locations
fields terminated by ',' optionally enclosed by '"'
lines terminated by '\r\n'
ignore 1 rows;
