select library, Postal_code, phone_number from library_locations;

select monday_open from library_locations;

select distinct monday_open from library_locations;

select library, square_feet from
(select * from library_locations where address like '%NW%' 
or address like '%SW%') as west_libs;

SELECT library FROM library_locations;
with west_libs as (
    select * from library_locations
    where address like '%NW' or address like '%SW%'
)

select library, square_feet from west_libs;


select count(*) from library_locations;

select sum(square_feet) from library_locations; 

select count(distinct monday_open) from library_locations;