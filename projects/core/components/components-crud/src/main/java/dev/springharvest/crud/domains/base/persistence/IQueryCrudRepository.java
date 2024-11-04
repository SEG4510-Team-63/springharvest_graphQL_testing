package dev.springharvest.crud.domains.base.persistence;

import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.data.repository.ListPagingAndSortingRepository;

import java.io.Serializable;

/**
 * This interface is used to define the contract for a base specification repository. It wraps the ICrudRepository interface.
 *
 * @param <E> The type of the entity.
 * @param <K> The type of the id (primary key) field.
 * @see ICrudRepository
 */
public interface IQueryCrudRepository<E extends BaseEntity<K>, K extends Serializable>
        extends ListCrudRepository<E, K>, ListPagingAndSortingRepository<E, K>, JpaSpecificationExecutor<E> {
}
