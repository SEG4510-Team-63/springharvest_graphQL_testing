--liquibase formatted sql
--changeset BillyBolton:create-authors-table

DROP TABLE IF EXISTS authors CASCADE;
CREATE TABLE authors
(
    id   UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    name VARCHAR(40) NOT NULL,
    pet_id UUID,
    UNIQUE (pet_id, name)
) INHERITS (base_trace_parent);