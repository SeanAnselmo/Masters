-- creating per the schema seen on the City of Calgary webpage: 
-- https://data.calgary.ca/Services-and-Amenities/Licensed-Pets/5dgy-88cq

CREATE TABLE IF NOT EXISTS `licensed_pets` (
  `DATE` DATETIME DEFAULT NULL,
  `COMMUNITY_CODE` text DEFAULT NULL,
  `COMMUNITY_NAME` text DEFAULT NULL,
  `ANIMAL_TYPE` text DEFAULT NULL,
  `LICENSE_VOLUME` INT DEFAULT NULL,
  `ROW_ID` text, 
  `POINT` point 
) ENGINE=InnoDB;