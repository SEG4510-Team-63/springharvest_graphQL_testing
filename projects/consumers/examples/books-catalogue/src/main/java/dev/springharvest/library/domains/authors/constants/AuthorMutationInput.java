package dev.springharvest.library.domains.authors.constants;

import java.util.UUID;

public record AuthorMutationInput(String name, UUID updatedBy) {
}
