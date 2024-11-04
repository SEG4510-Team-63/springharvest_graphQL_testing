package dev.springharvest.library.domains.publishers.services;

import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.persistence.IPublisherQueryCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class PetQueryCrudService extends AbstractQueryCrudService<PublisherEntity, UUID> {
    @Autowired
    protected PetQueryCrudService(IPublisherQueryCrudRepository baseRepository) {
        super(baseRepository);
    }
}
