package dev.springharvest.library.domains.authors.services;

import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.persistence.IAuthorQueryCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class AuthorQueryCrudService extends AbstractQueryCrudService<AuthorEntity, UUID> {

    @Autowired
    protected AuthorQueryCrudService(IAuthorQueryCrudRepository baseRepository) {
        super(baseRepository);
    }
}
