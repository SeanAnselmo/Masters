select * from licensed_pets limit 100;

select count(*) from licensed_pets;

select community_name, sum(license_volume) from licensed_pets
group by community_name;

select avg(license_volume) from licensed_pets
where date between "2019-10-01" and "2019-12-01";

select animal_type, avg(license_volume) from licensed_pets
where date between "2019-10-01" and "2019-12-01"
group by animal_type;

select animal_type, community_name, avg(license_volume) from licensed_pets
where date between "2019-10-01" and "2019-12-01"
group by animal_type, community_name;

select animal_type, community_name, avg(license_volume) from licensed_pets
where date between "2019-10-01" and "2019-12-01" and animal_type="CATS"
group by animal_type, community_name
order by avg(license_volume) desc;

select animal_type, community_name, avg(license_volume) from licensed_pets
where date between "2019-10-01" and "2019-12-01" and animal_type="CATS"
group by animal_type, community_name
having avg(license_volume) > 500
order by avg(license_volume) desc;

-- example windowing function
select rank() over (partition by animal_type order by license_volume desc) as pet_rank, animal_type, license_volume, community_name
from licensed_pets
where date = "2019-10-01";


-- find the closest communities to a particular point
select distinct community_name, ST_Distance(point, point(-114.240790659013, 51.121889851117)) as distance from licensed_pets order by distance;


-- Subquery Returning an Aggregate Value in a WHERE Clause
SELECT community_name FROM licensed_pets
WHERE license_volume < (SELECT avg(license_volume) from licensed_pets);

-- example subqueries and nested queries
-- Subquery in a FROM clause using a GROUP BY to calculate aggregate values
select community_name, sum_volume 
 from 
(
  select community_name, SUM(license_volume) as sum_volume from licensed_pets
  group by community_name
) as s
where sum_volume < 300;

-- HAVING clause

-- Using group by ing this scenario will not work. Why is that?
select community_name, animal_type, avg(license_volume) from licensed_pets
where date between '2019-10-01' and '2019-12-01' and avg(license_volume) < 10
group by community_name, animal_type;

-- Using Having instead

select community_name, animal_type, avg(license_volume) from licensed_pets
where date between '2019-10-01' and '2019-12-01'
group by community_name, animal_type
having avg(license_volume) < 10;