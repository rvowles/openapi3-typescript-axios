package com.fluxfederation;

import org.junit.Test;
import org.openapitools.codegen.OpenAPIGenerator;

import java.util.Arrays;

public class SampleTest {
  @Test
  public void runGenerator() {
    String location = getClass().getResource("/sample2.yaml").getFile();
    OpenAPIGenerator.main(Arrays.asList("generate",
      "--input-spec", location,
      "--generator-name", "typescript-axios-enhanced",
      "--additional-properties", "useEnhancedSerializer",
      "--additional-properties", "npmName=sample",
      "--additional-properties", "useCoalesceReturnTypes",
      "--additional-properties", "withSeparateModelsAndApi=true",
      "--api-package", "api",
      "--model-package", "model",
      "--output", "target/" + getClass().getSimpleName())
      .toArray(new String[0]));
  }

//  // https://esi.evetech.net/_latest/swagger.json
//  @Test
//  public void runGeneratorOnEsiApi() {
//    String location = getClass().getResource("/esi.evetech.net.swagger.json").getFile();
//    OpenAPIGenerator.main(Arrays.asList("generate",
//      "--input-spec", location,
//      "--generator-name", "typescript-axios-enhanced",
//      "--additional-properties", "useEnhancedSerializer",
//      "--additional-properties", "useCoalesceReturnTypes",
//      "--additional-properties", "npmName=eveTech",
//      "--additional-properties", "withSeparateModelsAndApi=true",
//      "--api-package", "api",
//      "--model-package", "model",
//      "--output", "target/evetech")
//      .toArray(new String[0]));
//  }
}
