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
//      "--additional-properties", "useEnhancedSerializer=true",
//      "--additional-properties", "useCoalesceReturnTypes=true",
//      "--additional-properties", "withSeparateModelsAndApi=true",
      "--api-package", "api",
      "--model-package", "model",
      "--output", "target/" + getClass().getSimpleName())
      .toArray(new String[0]));
  }
}
