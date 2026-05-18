IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = N'mydatabase')
BEGIN
    CREATE DATABASE [mydatabase];
END
GO

USE [mydatabase];
GO

CREATE TABLE autoreport (
  [id] varchar(255) DEFAULT NULL,
  [synopsis] varchar(255) DEFAULT NULL,
  [package] varchar(255) DEFAULT NULL,
  [fun] varchar(255) DEFAULT NULL,
  [params] varchar(max),
  [owner] varchar(255) DEFAULT NULL,
  [email] varchar(255) DEFAULT NULL,
  [organization] varchar(255) DEFAULT NULL,
  [terminateDate] varchar(255) DEFAULT NULL,
  [interval] varchar(255) DEFAULT NULL,
  [intervalName] varchar(255) DEFAULT NULL,
  [runDayOfYear] varchar(max),
  [type] varchar(255) DEFAULT NULL,
  [ownerName] varchar(255) DEFAULT NULL,
  [startDate] varchar(255) DEFAULT NULL
);

CREATE TABLE appLog (
  [id] int check (id > 0) NOT NULL IDENTITY,
  [time] datetime2 DEFAULT NULL,
  [user] varchar(127) DEFAULT NULL,
  [name] varchar(255) DEFAULT NULL,
  [group] varchar(63) DEFAULT NULL,
  [role] varchar(63) DEFAULT NULL,
  [resh_id] varchar(31) DEFAULT NULL,
  [message] varchar(2047) DEFAULT NULL,
  PRIMARY KEY (id)
) ;

CREATE TABLE reportLog (
  [id] int check (id > 0) NOT NULL IDENTITY,
  [time] datetime2 DEFAULT NULL,
  [user] varchar(127) DEFAULT NULL,
  [name] varchar(255) DEFAULT NULL,
  [group] varchar(63) DEFAULT NULL,
  [role] varchar(63) DEFAULT NULL,
  [resh_id] varchar(31) DEFAULT NULL,
  [environment] varchar(63) DEFAULT NULL,
  [call] varchar(2047) DEFAULT NULL,
  [message] varchar(2047) DEFAULT NULL,
  PRIMARY KEY (id)
) ;

GO
