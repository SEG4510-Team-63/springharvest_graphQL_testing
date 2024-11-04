package dev.springharvest.library.domains.pet.persistence;

import dev.springharvest.crud.domains.base.persistence.IQueryCrudRepository;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IPetQueryCrudRepository extends IQueryCrudRepository<PetEntity, UUID> {
}
