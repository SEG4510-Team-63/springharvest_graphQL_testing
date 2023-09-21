package dev.springharvest.library.domains.publishers.integration.utils.clients;

import dev.springharvest.library.config.TestComponentScanningConfig;
import dev.springharvest.shared.domains.publishers.constants.PublisherConstants;
import dev.springharvest.shared.domains.publishers.models.dtos.PublisherDTO;
import dev.springharvest.testing.integration.crud.clients.AbstractCrudClientImpl;
import dev.springharvest.testing.integration.shared.clients.RestClientImpl;
import dev.springharvest.testing.integration.shared.uri.UriFactory;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.stereotype.Component;

@Component
@Import(value = {TestComponentScanningConfig.class})
public class PublishersCrudClient extends AbstractCrudClientImpl<PublisherDTO, UUID> {

  @Autowired(required = true)
  protected PublishersCrudClient(RestClientImpl clientHelper) {
    super(clientHelper, new UriFactory(PublisherConstants.Controller.DOMAIN_CONTEXT), PublisherDTO.class);
  }

}