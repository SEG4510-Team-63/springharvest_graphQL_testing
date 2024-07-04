package dev.springharvest.library.domains.books.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.books.models.dtos.BookDTO;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import java.util.*;
import dev.springharvest.library.domains.books.services.BookSpecificationCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
public class BookGraphQLController extends AbstractGraphQLCrudController<BookDTO, BookEntity, UUID> {

  @Autowired
  protected BookGraphQLController(IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper, BookSpecificationCrudService baseService) {
      super(modelMapper, baseService, BookEntity.class);
  }

    @QueryMapping
    public List<BookDTO> searchBooks(@Argument @NotNull Map<String, Object> filter, @Argument @NotNull DataPaging paging) {
        return search(filter, paging);
    }

    @QueryMapping
    public List<BookDTO> searchBooksSimple(@Argument @NotNull Map<String, Object> filter) {
        return search(filter);
    }
}
