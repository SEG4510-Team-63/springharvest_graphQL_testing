package dev.springharvest.library.domains.books.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.books.models.dtos.BookDTO;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import java.util.*;
import dev.springharvest.library.domains.books.services.BookQueryCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import graphql.schema.DataFetchingEnvironment;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
public class BookGraphQLController extends AbstractGraphQLCrudController<BookDTO, BookEntity, UUID> {

  @Autowired
  protected BookGraphQLController(IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper, BookQueryCrudService baseService) {
      super(modelMapper, baseService, BookEntity.class);
  }

    @QueryMapping
    public List<BookDTO> searchBooks(@Argument Map<String, Object> filter, @Argument Map<String, Object> operation, @Argument DataPaging paging, DataFetchingEnvironment environment) {
        return paging != null ? search(filter, operation, paging, environment) : search(filter, operation, environment);
    }

    @QueryMapping
    public String countBooks(@Argument Map<String, Object> filter, @Argument Map<String, Object> operation, @Argument List<String> fields) {
        return count(filter, operation, fields);
    }
}
