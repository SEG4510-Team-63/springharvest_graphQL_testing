package dev.springharvest.library.domains.publishers.models.entities;


import dev.springhavest.common.models.entities.BaseEntity;
import jakarta.persistence.AttributeOverride;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Entity
@Table(name = "publishers")
@AttributeOverride(name = "id", column = @Column(name = "id"))
public class PublisherEntity extends BaseEntity<UUID> {

    @Column(name = "name")
    protected String name;

}
