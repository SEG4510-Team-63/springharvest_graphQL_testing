package dev.springharvest.library.domains.pet.service;

import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import dev.springharvest.library.domains.pet.persistence.IPetSpecificationCrudRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service("petSpecificationService")
public class PetSpecificationCrudService extends AbstractSpecificationCrudService<PetEntity, UUID> {
    @Autowired
    protected PetSpecificationCrudService(IPetSpecificationCrudRepository baseRepository) {
        super(baseRepository);
    }
}
