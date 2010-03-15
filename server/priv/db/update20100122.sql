ALTER TABLE lines ADD COLUMN painted boolean NOT NULL DEFAULT false;

CREATE TABLE tiles (
  x             integer,
  y             integer,
  last_painted  bigint
);