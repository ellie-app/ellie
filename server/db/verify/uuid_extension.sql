-- Verify ellie:uuid_extension on pg

begin;

select 1/count(*) from pg_extension where extname = 'uuid-ossp';
select has_function_privilege('uuid_generate_v4()', 'execute');

rollback;
