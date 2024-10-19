package dev.springharvest.library.domains.authors.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.authors.models.dtos.AuthorDTO;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import java.util.*;

import dev.springharvest.library.domains.authors.services.AuthorSpecificationCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import graphql.schema.DataFetchingEnvironment;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
public class AuthorGraphQLController extends AbstractGraphQLCrudController<AuthorDTO, AuthorEntity, UUID> {

  @Autowired
  protected AuthorGraphQLController(AuthorSpecificationCrudService baseService, IBaseModelMapper<AuthorDTO, AuthorEntity, UUID> modelMapper) {
    super(modelMapper, baseService, AuthorEntity.class);
  }

  @QueryMapping
  public List<AuthorDTO> searchAuthors(@Argument Map<String, Object> filter, @Argument Map<String, Object> operation, @Argument DataPaging paging, DataFetchingEnvironment environment) {
    return paging != null ? search(filter, operation, paging, environment) : search(filter, operation, environment);
  }

  @QueryMapping
  public String countAuthors(@Argument Map<String, Object> filter, @Argument Map<String, Object> operation) {
    return count(filter, operation);
  }


//  @MutationMapping
//  AuthorDTO authorMutation(@Argument @NotNull UUID id, @Argument @NotNull AuthorMutationInput input)
//  {
//    var op = baseService.findById(id);
//    if (op.isPresent())
//    {
//      var author = op.get();
//      author.setName(input.name());
//      author.getTraceData().getTraceDates().setDateUpdated(LocalDate.now());
//      author.getTraceData().getTraceUsers().setUpdatedBy(input.updatedBy());
//      baseService.update(author);
//      return modelMapper.entityToDto(author);
//    }
//    else {
//      throw new EntityNotFoundException(String.format("No entity found with id: %s", id));
//    }
//  }
}
