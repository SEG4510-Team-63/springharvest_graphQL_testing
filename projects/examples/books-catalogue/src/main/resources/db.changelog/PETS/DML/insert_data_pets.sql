--liquibase formatted sql
--changeset NeroNemesis:insert-pets
INSERT INTO pets
    (id,
     name,
     author_id)
 values
    ('00000000-0000-0000-0000-000000000001',
     'Greyson',
     '00000000-0000-0000-0000-000000000001');
INSERT INTO pets
    (id,
     name,
     author_id)
values
    ('00000000-0000-0000-0000-000000000002',
     'Rex',
     '00000000-0000-0000-0000-000000000002');