package dev.springharvest.library.domains.authors.integration.utils.factories;

import dev.springharvest.library.config.TestComponentScanningConfig;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.models.queries.AuthorFilterDTO;
import dev.springharvest.library.domains.authors.models.queries.AuthorFilterRequestDTO;
import dev.springharvest.search.domains.base.models.entities.EntityMetadata;
import dev.springharvest.search.domains.base.models.queries.parameters.filters.CriteriaOperator;
import dev.springharvest.search.domains.base.models.queries.parameters.filters.FilterParameterDTO;
import dev.springharvest.testing.domains.integration.search.factories.AbstractSearchModelFactoryImpl;
import java.util.Set;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.stereotype.Component;

@Component
@Import(value = {TestComponentScanningConfig.class})
public class AuthorSearchModelFactory
    extends AbstractSearchModelFactoryImpl<AuthorEntity, AuthorFilterRequestDTO> {

  @Autowired
  public AuthorSearchModelFactory(EntityMetadata<AuthorEntity> entityMetadata) {
    super(entityMetadata);
  }

  @Override
  public AuthorFilterRequestDTO buildValidFilters() {
    return AuthorFilterRequestDTO.builder()
        .author(AuthorFilterDTO.builder()
                    .id(FilterParameterDTO.builder()
                            .values(Set.of(UUID.fromString(
                                "00000000-0000-0000-0000-000000000001")))
                            .operator(CriteriaOperator.EQUALS)
                            .build())
                    .build())
        .build();
  }

}
