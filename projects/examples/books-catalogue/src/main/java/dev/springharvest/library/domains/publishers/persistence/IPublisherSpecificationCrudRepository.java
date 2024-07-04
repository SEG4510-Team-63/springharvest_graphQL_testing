package dev.springharvest.library.domains.publishers.persistence;

import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface IPublisherSpecificationCrudRepository extends ISpecificationCrudRepository<PublisherEntity, UUID> {
}
