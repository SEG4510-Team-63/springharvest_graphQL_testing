package dev.springharvest.library.domains.publishers.controllers.graphql;

import dev.springharvest.library.domains.publishers.constants.PublisherSearchInput;
import dev.springharvest.library.domains.publishers.models.dtos.PublisherDTO;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.services.PublisherCrudService;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

/**
 * This is the GraphQL controller for the Publisher domain.
 * It provides query operations for publishers.
 *
 */
@Controller
public class PublisherGraphQLController {

  private final PublisherCrudService baseService;
  protected IBaseModelMapper<PublisherDTO, PublisherEntity, UUID> modelMapper;

  /**
   * Constructs a new PublisherGraphQLController with the given service and model mapper.
   *
   * @param baseService the service to use for publisher operations
   * @param modelMapper the model mapper to use for converting between DTOs and entities
   */
  @Autowired
  protected PublisherGraphQLController(PublisherCrudService baseService, IBaseModelMapper<PublisherDTO, PublisherEntity, UUID> modelMapper) {
    this.baseService = baseService;
    this.modelMapper = modelMapper;
  }

  /**
   * Returns a list of publishers based on the given search input.
   *
   * @param input the search input
   * @return a list of publishers
   */
  @QueryMapping()
  List<PublisherDTO> publishers(@Argument @NotNull PublisherSearchInput input) {
    PageRequest publishersToFind = PageRequest.of(input.page(), input.size(), input.sortDirection() == "A" ? Sort.by(input.sortOrder()).ascending() : Sort.by(input.sortOrder()).descending());
    Page<PublisherEntity> publishersPage;

    if (StringUtils.isEmpty(input.name())) {
      publishersPage = baseService.findAll(publishersToFind);
    } else {
      publishersPage = baseService.findByNameContaining(input.name(), publishersToFind);
    }

    Page<PublisherDTO> dtos = publishersPage.hasContent() ? modelMapper.pagedEntityToPagedDto(publishersPage) : Page.empty(publishersToFind);
    return dtos.getContent();
  }

  /**
   * Returns a publisher by its ID.
   *
   * @param id the ID of the publisher
   * @return the publisher, or an empty Optional if no publisher was found with the given ID
   */
  @QueryMapping()
  Optional<PublisherEntity> publisherById(@Argument UUID id) {
    return baseService.findById(id);
  }

}
