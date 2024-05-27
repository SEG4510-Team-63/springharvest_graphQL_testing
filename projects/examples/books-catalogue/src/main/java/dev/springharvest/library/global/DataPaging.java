package dev.springharvest.library.global;

import java.util.List;

public record DataPaging(int page,
                         int size,
                         String sortOrders,
                         String sortDirection
                         ) {}
