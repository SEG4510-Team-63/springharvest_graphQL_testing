package dev.springharvest.shared.constants;

import java.util.List;

public record DataPaging(int page,
                         int size,
                         String sortOrders,
                         SortDirections sortDirection
                         ) {}
