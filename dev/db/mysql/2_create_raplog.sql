CREATE DATABASE IF NOT EXISTS `raplog`;

USE `raplog`;

CREATE TABLE IF NOT EXISTS `appLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `reportLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `environment` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `call` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
