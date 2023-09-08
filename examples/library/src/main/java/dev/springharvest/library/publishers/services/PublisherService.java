package dev.springharvest.library.publishers.services;

import dev.springharvest.crud.service.AbstractBaseCrudService;
import dev.springharvest.library.publishers.mappers.IPublisherMapper;
import dev.springharvest.library.publishers.models.dtos.PublisherDTO;
import dev.springharvest.library.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.publishers.persistence.IPublisherRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class PublisherService extends AbstractBaseCrudService<PublisherDTO, PublisherEntity, UUID> {

    @Autowired
    protected PublisherService(IPublisherMapper baseMapper, IPublisherRepository baseRepository) {
        super(baseMapper, baseRepository);
    }

}
