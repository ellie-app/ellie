-- Deploy ellie:uuid_extension to pg
-- requires: ellie_schema

begin;

create extension "uuid-ossp";

commit;
