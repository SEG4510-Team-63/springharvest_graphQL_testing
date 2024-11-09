package dev.springharvest.shared.constants;

import lombok.Data;

import java.util.List;

@Data
public class PageData<D> {
    private List<D> dtos;
    private int currentPage;
    private int pageSize;
    private int total;

    public PageData(List<D> dtos, int currentPage, int pageSize, int total){
        this.dtos = dtos;
        this.currentPage = currentPage;
        this.pageSize = pageSize;
        this.total = total;
    }
}
