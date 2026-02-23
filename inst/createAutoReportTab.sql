/* Database: Autoreport */
CREATE TABLE autoreport (
  id varchar(255) NOT NULL,
  synopsis varchar(255) DEFAULT NULL,
  package varchar(255) DEFAULT NULL,
  fun varchar(255) DEFAULT NULL,
  params varchar(1025),
  owner varchar(255) DEFAULT NULL,
  email varchar(255) DEFAULT NULL,
  organization varchar(255) DEFAULT NULL,
  terminateDate varchar(255) DEFAULT NULL,
  `interval` varchar(255) DEFAULT NULL,
  intervalName varchar(255) DEFAULT NULL,
  runDayOfYear varchar(255) DEFAULT NULL,
  type varchar(255) DEFAULT NULL,
  ownerName varchar(255) DEFAULT NULL,
  startDate varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
);
