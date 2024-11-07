package dev.springharvest.library.domains.authors.persistence;

import dev.springharvest.crud.domains.base.persistence.IQueryCrudRepository;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IAuthorQueryCrudRepository extends IQueryCrudRepository<AuthorEntity, UUID> {
}
