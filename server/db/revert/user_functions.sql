-- Revert ellie:user_functions from pg

begin;

drop function ellie.retrieve_user(json);
drop function ellie.create_user(json);
drop function ellie.update_user(json);

commit;
