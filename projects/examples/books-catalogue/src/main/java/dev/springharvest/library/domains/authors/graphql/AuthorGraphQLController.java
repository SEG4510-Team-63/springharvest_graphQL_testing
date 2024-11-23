package dev.springharvest.library.domains.authors.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import java.util.*;

import dev.springharvest.library.domains.authors.services.AuthorQueryCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import graphql.schema.DataFetchingEnvironment;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
public class AuthorGraphQLController extends AbstractGraphQLCrudController<AuthorEntity, UUID> {

  @Autowired
  protected AuthorGraphQLController(AuthorQueryCrudService baseService) {
    super(baseService, AuthorEntity.class, UUID.class);
  }

  @QueryMapping
  public PageData<AuthorEntity> searchAuthors(@Argument Map<String, Object> filter, @Argument Map<String, Object> clause, @Argument DataPaging paging, DataFetchingEnvironment environment) {
    return search(filter, clause, paging, environment);
  }

  @QueryMapping
  public long countAuthors(@Argument Map<String, Object> filter, @Argument Map<String, Object> clause, @Argument List<String> fields) {
    return count(filter, clause, fields);
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
