CREATE TABLE IF NOT EXISTS `data` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `mtime` timestamp DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `registry` varchar(255) NOT NULL COLLATE utf8_danish_ci,
  `name` varchar(255) NOT NULL COLLATE utf8_danish_ci,
  `data` longblob,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
