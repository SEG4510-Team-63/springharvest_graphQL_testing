package dev.springharvest.library.domains.publishers.services;

import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.persistence.IPublisherSpecificationCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class PetSpecificationCrudService extends AbstractSpecificationCrudService<PublisherEntity, UUID> {
    @Autowired
    protected PetSpecificationCrudService(IPublisherSpecificationCrudRepository baseRepository) {
        super(baseRepository);
    }
}
