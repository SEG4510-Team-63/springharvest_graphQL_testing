package dev.springharvest.library.domains.pet.controllers.graphql;

import dev.springharvest.library.domains.pet.service.PetCrudService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
public class PetGraphQLController {

    private final PetCrudService baseService;

    @Autowired
    public  PetGraphQLController(PetCrudService baseService){
        this.baseService = baseService;
    }
}
