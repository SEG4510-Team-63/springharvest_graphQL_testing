package dev.springharvest.library.domains.books.persistence;

import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IBookSpecificationCrudRepository extends ISpecificationCrudRepository<BookEntity, UUID> {
}
