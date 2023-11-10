package dev.springharvest.testing.domains.integration.search.factories;

import dev.springharvest.search.domains.base.models.entities.EntityMetadata;
import dev.springharvest.search.domains.base.models.entities.IEntityMetadata;
import dev.springharvest.search.domains.base.models.queries.parameters.filters.CriteriaOperator;
import dev.springharvest.search.domains.base.models.queries.parameters.selections.SelectionDTO;
import dev.springharvest.search.domains.base.models.queries.requests.filters.BaseFilterRequestDTO;
import dev.springharvest.search.domains.base.models.queries.requests.search.SearchRequestDTO;
import dev.springharvest.shared.domains.DomainModel;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Getter;

@Getter
public abstract class AbstractSearchModelFactoryImpl<D extends DomainModel, E extends DomainModel, B extends BaseFilterRequestDTO>
    implements ISearchModelFactory<D, B> {

  protected final IEntityMetadata<E> entityMetadata;
  protected final String idPath;

  protected AbstractSearchModelFactoryImpl(EntityMetadata<E> entityMetadata) {
    this.entityMetadata = entityMetadata;
    this.idPath = entityMetadata.getDomainName() + "." + "id";
  }

  @Override
  public List<SearchRequestDTO<B>> buildValidSearchRequestDTOs(CriteriaOperator operator, List<D> models) {

    List<SearchRequestDTO<B>> requests = new LinkedList<>();
    models.forEach(model -> {
      requests.add(SearchRequestDTO.<B>builder()
                       .selections(buildValidSelections(false))
                       .filters(Set.of(buildValidUniqueFilters(operator, List.of(model))))
                       .build());
    });

    return requests;
  }

  @Override
  public List<SelectionDTO> buildValidSelections(boolean isEmpty) {
    return isEmpty ? List.of() : entityMetadata.getPaths().stream().map(path -> SelectionDTO.builder().alias(path).build()).collect(Collectors.toList());
  }


}
