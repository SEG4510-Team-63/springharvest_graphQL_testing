package dev.springharvest.library.domains.authors.services;

import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.persistence.IAuthorCrudRepository;
import dev.springharvest.library.domains.authors.persistence.IAuthorSpecificationCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class AuthorSpecificationCrudService extends AbstractSpecificationCrudService<AuthorEntity, UUID> {

    @Autowired
    protected AuthorSpecificationCrudService(IAuthorSpecificationCrudRepository baseRepository) {
        super(baseRepository);
    }
}
