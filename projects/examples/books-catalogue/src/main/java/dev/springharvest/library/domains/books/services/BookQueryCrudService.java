package dev.springharvest.library.domains.books.services;

import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.library.domains.books.persistence.IBookQueryCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.UUID;

@Service
public class BookQueryCrudService extends AbstractQueryCrudService<BookEntity, UUID> {

    @Autowired
    protected BookQueryCrudService(IBookQueryCrudRepository crudRepository) {
        super(crudRepository);
    }
}
