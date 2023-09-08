package dev.springharvest.library.authors.persistence;

import dev.springharvest.crud.persistence.IBaseCrudRepository;
import dev.springharvest.library.authors.models.entities.AuthorEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IAuthorRepository extends IBaseCrudRepository<AuthorEntity, UUID> {
}
