CREATE DATABASE IF NOT EXISTS `db_log`;

USE `db_log`;

CREATE TABLE IF NOT EXISTS `appLog` (
  id int NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(255) DEFAULT NULL,
  name varchar(255) DEFAULT NULL,
  `group` varchar(255) DEFAULT NULL,
  role varchar(255) DEFAULT NULL,
  resh_id varchar(255) DEFAULT NULL,
  message varchar(2047) DEFAULT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS `reportLog` (
  id int NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(255) DEFAULT NULL,
  name varchar(255) DEFAULT NULL,
  `group` varchar(255) DEFAULT NULL,
  role varchar(255) DEFAULT NULL,
  resh_id varchar(255) DEFAULT NULL,
  environment varchar(255) DEFAULT NULL,
  `call` varchar(2047) DEFAULT NULL,
  message varchar(2047) DEFAULT NULL,
  PRIMARY KEY (id)
);
