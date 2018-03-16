-- Revert ellie:users from pg

begin;

drop table ellie.users;
drop type ellie.settings;
drop type ellie.theme;

commit;
