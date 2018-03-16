-- Revert ellie:uuid_extension from pg

begin;

drop extension "uuid-ossp";

commit;
