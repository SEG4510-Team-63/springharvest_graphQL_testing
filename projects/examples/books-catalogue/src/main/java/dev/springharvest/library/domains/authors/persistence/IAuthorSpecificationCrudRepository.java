package dev.springharvest.library.domains.authors.persistence;

import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IAuthorSpecificationCrudRepository extends ISpecificationCrudRepository<AuthorEntity, UUID> {
}
