CREATE DATABASE IF NOT EXISTS `autoreport`;

USE `autoreport`;

CREATE TABLE IF NOT EXISTS `autoreport` (
  `id` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `synopsis` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `package` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `fun` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `params` text COLLATE utf8mb4_danish_ci,
  `owner` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `email` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `organization` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `terminateDate` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `interval` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `intervalName` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `runDayOfYear` text COLLATE utf8mb4_danish_ci,
  `type` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `ownerName` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `startDate` varchar(255) COLLATE utf8mb4_danish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci;
