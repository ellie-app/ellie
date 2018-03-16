-- Revert ellie:revisions from pg

begin;

drop table ellie.revisions;
drop type ellie.package;
drop type ellie.name;
drop type ellie.version;

commit;
