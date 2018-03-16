-- Revert ellie:ellie_schema from pg

begin;

drop schema ellie;

commit;
