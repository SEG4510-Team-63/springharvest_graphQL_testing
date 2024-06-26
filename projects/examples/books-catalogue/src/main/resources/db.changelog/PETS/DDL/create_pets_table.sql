--liquibase formatted sql
--changeset NeroNemesis:create-pets-table

DROP TABLE IF EXISTS pets CASCADE;
CREATE TABLE pets
(
    id           UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    name        VARCHAR(100) NOT NULL,
    author_id    UUID        NOT NULL,
    UNIQUE (name, author_id)
) INHERITS (base_trace_parent);