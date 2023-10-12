package dev.springharvest.testing.integration.search.factories;

import dev.springharvest.search.model.entities.EntityMetadata;
import dev.springharvest.search.model.entities.IEntityMetadata;
import dev.springharvest.search.model.queries.parameters.selections.SelectionDTO;
import dev.springharvest.search.model.queries.requests.filters.BaseFilterRequestDTO;
import dev.springharvest.search.model.queries.requests.search.SearchRequestDTO;
import dev.springhavest.common.models.domains.DomainModel;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Getter;

@Getter
public abstract class AbstractSearchModelFactoryImpl<M extends DomainModel, B extends BaseFilterRequestDTO>
    implements ISearchModelFactory<B> {

  protected final IEntityMetadata<M> entityMetadata;
  protected final String idPath;

  protected AbstractSearchModelFactoryImpl(EntityMetadata<M> entityMetadata) {
    this.entityMetadata = entityMetadata;
    this.idPath = entityMetadata.getDomainName() + "." + "id";
  }

  @Override
  public List<SelectionDTO> buildValidSelections(boolean isEmpty) {
    return isEmpty ? List.of() : entityMetadata.getPaths().stream().map(path -> SelectionDTO.builder().alias(path).build()).collect(Collectors.toList());
  }

  @Override
  public List<SearchRequestDTO<B>> buildValidSearchRequestDTOs() {

    List<SearchRequestDTO<B>> requests = new LinkedList<>();
    requests.add(SearchRequestDTO.<B>builder()
                     .selections(buildValidSelections(false))
                     .filters(Set.of(buildValidFilters()))
                     .build());

    return requests;
  }

}