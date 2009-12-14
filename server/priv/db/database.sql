-- Database structure

CREATE SEQUENCE lines_id_seq;
CREATE TABLE lines (
  id            integer PRIMARY KEY DEFAULT nextval('lines_id_seq'),
  bounding_box  box,
  points        text,
  color         integer,
  size          integer,
  ip            varchar(15),
  time          bigint
);