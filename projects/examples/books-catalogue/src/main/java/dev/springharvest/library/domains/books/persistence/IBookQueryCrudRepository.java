package dev.springharvest.library.domains.books.persistence;

import dev.springharvest.crud.domains.base.persistence.IQueryCrudRepository;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IBookQueryCrudRepository extends IQueryCrudRepository<BookEntity, UUID> {
}
