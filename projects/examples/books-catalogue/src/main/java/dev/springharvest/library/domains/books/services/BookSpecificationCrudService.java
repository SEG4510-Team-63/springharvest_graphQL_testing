package dev.springharvest.library.domains.books.services;

import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.library.domains.books.persistence.IBookSpecificationCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.UUID;

@Service
public class BookSpecificationCrudService extends AbstractSpecificationCrudService<BookEntity, UUID> {

    @Autowired
    protected BookSpecificationCrudService(IBookSpecificationCrudRepository crudRepository) {
        super(crudRepository);
    }
}
