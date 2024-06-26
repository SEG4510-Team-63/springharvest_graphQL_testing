package dev.springharvest.library.domains.pet.controllers.graphql;

import dev.springharvest.expressions.helpers.JpaSpecificationHelper;
import dev.springharvest.library.domains.pet.models.dtos.PetDTO;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import dev.springharvest.library.domains.pet.service.PetCrudService;
import dev.springharvest.library.global.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Controller
public class PetGraphQLController {

    private final PetCrudService baseService;
    protected IBaseModelMapper<PetDTO, PetEntity, UUID> modelMapper;
    private Specification<PetEntity> specification;

    @Autowired
    public  PetGraphQLController(IBaseModelMapper<PetDTO, PetEntity, UUID> modelMapper, PetCrudService baseService){
        this.baseService = baseService;
        this.modelMapper = modelMapper;
    }

    @QueryMapping
    List<PetDTO> searchPets(@Argument @NotNull Map filter, @Argument DataPaging paging) {
        var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection() == "A" ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());

        if (filter.isEmpty())
        {
            Page<PetEntity> petsPage  = baseService.findAll(pageRequest);

            Page<PetDTO> dtos = petsPage.hasContent() ? modelMapper.pagedEntityToPagedDto(petsPage) : Page.empty(pageRequest);
            return dtos.getContent();
        }

        specification = JpaSpecificationHelper.parseFilterExpression(filter);

        Page<PetEntity> petsPage  = baseService.findAll(specification, pageRequest);

        Page<PetDTO> dtos = petsPage.hasContent() ? modelMapper.pagedEntityToPagedDto(petsPage) : Page.empty(pageRequest);
        return dtos.getContent();
    }
}
