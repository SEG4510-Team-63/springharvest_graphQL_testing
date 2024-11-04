package dev.springharvest.library.domains.pet.service;

import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import dev.springharvest.library.domains.pet.persistence.IPetQueryCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service("petSpecificationService")
public class PetQueryCrudService extends AbstractQueryCrudService<PetEntity, UUID> {
    @Autowired
    protected PetQueryCrudService(IPetQueryCrudRepository baseRepository) {
        super(baseRepository);
    }
}
