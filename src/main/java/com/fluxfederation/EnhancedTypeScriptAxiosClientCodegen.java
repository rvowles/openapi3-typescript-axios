package com.fluxfederation;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class EnhancedTypeScriptAxiosClientCodegen extends AbstractTypeScriptClientCodegen {
  public static final String NPM_REPOSITORY = "npmRepository";
  public static final String WITH_INTERFACES = "withInterfaces";
  public static final String SEPARATE_MODELS_AND_API = "withSeparateModelsAndApi";
  public static final String WITHOUT_PREFIX_ENUMS = "withoutPrefixEnums";
  public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
  public static final String USE_ENHANCED_SERIALIZER = "useEnhancedSerializer";
  public static final String USE_COALESCE_RETURN_TYPES = "useCoalesceReturnTypes";
  public static final String DEOPTIMIZE_DESERIALIZATION = "useNonOptimalDeserialization";

  public static final String TEMPLATE_FOLDER = "enhanced-axios-ts";
  private static final String X_TS_DESERIALIZE_TYPE = "x-ts-deserialize-type";
  private static final String X_TS_OPTIMIZE = "x-ts-optimize";
  private static final String X_TS_RECORD_TYPE = "x-ts-record-type";
  private static final String X_TS_ADDITIONAL_PROPS = "x-ts-additional-props";

  protected String npmRepository = null;

  private String tsModelPackage = "";
  private boolean optimized = true;

  public EnhancedTypeScriptAxiosClientCodegen() {
    super();

    modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

    // clear import mapping (from default generator) as TS does not use it
    // at the moment
    importMapping.clear();

    outputFolder = "generated-code/typescript-axios";
    embeddedTemplateDir = templateDir = TEMPLATE_FOLDER;
    
    typeMapping.put("DateTime", "Date");
    typeMapping.put("date", "Date");
    typeMapping.put("AnyType", "any");

    this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url of your private npmRepo in the package.json"));
    this.cliOptions.add(new CliOption(WITH_INTERFACES,
      "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(SEPARATE_MODELS_AND_API, "Put the model and api in separate folders and in separate classes", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(WITHOUT_PREFIX_ENUMS, "Don't prefix enum names with class names", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(USE_SINGLE_REQUEST_PARAMETER, "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(USE_ENHANCED_SERIALIZER, "Setting this will ensure Axios complies with " +
      "serialization and validation rules and it generates classes to match interfaces"));
    this.cliOptions.add(new CliOption(USE_COALESCE_RETURN_TYPES, "Make a function return all of the  types it " +
      "actually returns wrapped in an AxiosResponse."));
    this.cliOptions.add(new CliOption(DEOPTIMIZE_DESERIALIZATION, "This allows you to force all deserialization and " +
      "serialization through the ObjectSerializer"));
  }

  @Override
  public String getName() {
    return "typescript-axios-enhanced";
  }

  @Override
  public String getHelp() {
    return "Generates a TypeScript client library using axios.";
  }

  public String getNpmRepository() {
    return npmRepository;
  }

  public void setNpmRepository(String npmRepository) {
    this.npmRepository = npmRepository;
  }

  private static String getRelativeToRoot(String path) {
    StringBuilder sb = new StringBuilder();
    int slashCount = path.split("/").length;
    if (slashCount == 0) {
      sb.append("./");
    } else {
      for (int i = 0; i < slashCount; ++i) {
        sb.append("../");
      }
    }
    return sb.toString();
  }


  @Override
  public void processOpts() {
    super.processOpts();
    tsModelPackage = modelPackage.replaceAll("\\.", "/");
    String tsApiPackage = apiPackage.replaceAll("\\.", "/");

    String modelRelativeToRoot = getRelativeToRoot(tsModelPackage);
    String apiRelativeToRoot = getRelativeToRoot(tsApiPackage);

    additionalProperties.put("tsModelPackage", tsModelPackage);
    additionalProperties.put("tsApiPackage", tsApiPackage);
    additionalProperties.put("apiRelativeToRoot", apiRelativeToRoot);
    additionalProperties.put("modelRelativeToRoot", modelRelativeToRoot);

    boolean generateApis = additionalProperties.containsKey(CodegenConstants.GENERATE_APIS);
    boolean generateModels = additionalProperties.containsKey(CodegenConstants.GENERATE_MODELS);
    boolean separateModelsAndApi = additionalProperties.containsKey(SEPARATE_MODELS_AND_API);
    optimized = !additionalProperties.containsKey(DEOPTIMIZE_DESERIALIZATION);
    supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));

    if (generateApis) { // this obviates Axios completely
      supportingFiles.add(new SupportingFile("baseApi.mustache", "", "base.ts"));
      supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.ts"));
    }

    if (!separateModelsAndApi) {
      supportingFiles.add(new SupportingFile("api.mustache", "", "api.ts"));
    }

    supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
    supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    supportingFiles.add(new SupportingFile("npmignore", "", ".npmignore"));

    if (separateModelsAndApi) {
      if (StringUtils.isBlank(modelPackage) && generateModels) {
        throw new RuntimeException("apiPackage and modelPackage must be defined");
      }
      if (StringUtils.isBlank(apiPackage) && generateApis) {
        throw new RuntimeException("apiPackage and modelPackage must be defined");
      }

      if (generateApis) {
        apiTemplateFiles.put("apiInner.mustache", ".ts");
        supportingFiles.add(new SupportingFile("apiIndex.mustache", tsApiPackage, "index.ts"));
      }

      if (generateModels) {
        supportingFiles.add(new SupportingFile("modelIndex.mustache", tsModelPackage, "index.ts"));
        additionalProperties.put("apiModelRelativeToRoot", apiRelativeToRoot);
      }

      if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
        if (generateModels) {
          modelTemplateFiles.put("enhancedModel.mustache", ".ts");
          supportingFiles.add(new SupportingFile("js_serializer.mustache", tsModelPackage, "model_serializer.ts"));
        }
      } else if (generateModels) {
        modelTemplateFiles.put("model.mustache", ".ts");
      }
    }

    if (additionalProperties.containsKey(NPM_NAME)) {
      addNpmPackageGeneration();
    }

  }

  // primitive types, anys and arrays of such as never deserialize, so may as well optiize them
  private static final List<String> optimizeDataTypes =Arrays.asList("string", "int", "integer", "double", "float",
    "num", "any",
    "number", "boolean", "object",
    "Array<any>", "Array<object>", "Array<string>", "Array<int>", "Array<integer>",
    "Array<any>", "Array<object>", "Array<string>", "Array<int>", "Array<integer>",
    "Set<double>", "Set<float>", "Set<num>", "Set<number>", "Set<boolean>",
    "Set<double>", "Set<float>", "Set<num>", "Set<number>", "Set<boolean>"
    );
  private void enhanceDataTarget(String dataType, String dataFormat, Map<String, Object> vendorExtensions) {
    if (dataType != null) {
      if ("string".equals(dataType.toLowerCase())) {
        if (optimized) {
          vendorExtensions.put(X_TS_OPTIMIZE, Boolean.TRUE);
        }
        if (dataFormat == null) {
          vendorExtensions.put("x-ts-string-type", Boolean.TRUE);
          vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  "string");
        } else {
          vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  dataFormat);
        }
      } else {
        if ("date".equals(dataType.toLowerCase())) {
          vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  dataFormat);
        } else {
          vendorExtensions.put(X_TS_DESERIALIZE_TYPE, dataType);
        }
        if (optimized && optimizeDataTypes.contains(dataType)) {
          vendorExtensions.put(X_TS_OPTIMIZE, Boolean.TRUE);
        }
      }
    } else {
      vendorExtensions.put(X_TS_DESERIALIZE_TYPE, "object");
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
    objs = super.postProcessOperationsWithModels(objs, allModels);
    Map<String, Object> vals = (Map<String, Object>) objs.getOrDefault("operations", new HashMap<>());
    List<CodegenOperation> operations = (List<CodegenOperation>) vals.getOrDefault("operation", new ArrayList<>());
        /*
            Filter all the operations that are multipart/form-data operations and set the vendor extension flag
            'multipartFormData' for the template to work with.
         */
    operations.stream()
      .filter(op -> op.hasConsumes)
      .filter(op -> op.consumes.stream().anyMatch(opc -> opc.values().stream().anyMatch("multipart/form-data"::equals)))
      .forEach(op -> op.vendorExtensions.putIfAbsent("multipartFormData", true));

    Set<String> initialImportedClasses;

    if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
      initialImportedClasses = Collections.singleton("ObjectSerializer");
      operations.stream()
        .filter(op -> op.hasProduces)
        .forEach(op -> {
          Set<String> responseTypes = op.responses.stream()
            .map(r -> (r.dataType == null) ? "void" : r.dataType)
            .collect(Collectors.toSet());
          if (responseTypes.isEmpty()) {
            responseTypes = new HashSet<>(Collections.singletonList("void"));
          }
          op.vendorExtensions.put("x-ts-responseTypes", String.join("|", responseTypes));

          op.responses.forEach(bp -> {
            enhanceDataTarget(bp.dataType, null, bp.vendorExtensions);
            bp.vendorExtensions.put("x-ts-is-error", bp.is4xx || bp.is5xx);
            if (bp.dataType != null && "Set<".startsWith(bp.dataType)) {
              bp.setUniqueItems(true);
            }
          });
        });

      operations.stream()
        .filter(op -> op.hasConsumes)
        .filter(op -> op.bodyParam != null)
        .map(op -> op.bodyParam)
        .forEach(bp -> {
          enhanceDataTarget(bp.dataType, bp.dataFormat, bp.vendorExtensions);
          if (bp.dataType != null && "Set<".startsWith(bp.dataType)) {
            bp.uniqueItems = true;
          }
        });

      operations.stream()
        .filter(CodegenOperation::getHasFormParams)
        .forEach(op -> {
          // correct the unique items
          op.formParams.stream().filter(p -> p.dataType != null && p.dataType.startsWith("Set<")).forEach(p -> {
            p.uniqueItems = true;
          });
        } );
    } else {
      initialImportedClasses = new HashSet<>();
    }

    // import the actual imported classes
    Set<String> importedClasses = (Set<String>)additionalProperties.computeIfAbsent("x-ts-imported-classes-set",
      (k) -> new HashSet<String>(initialImportedClasses));


    ((List<Map<String, String>>)objs.get("imports")).forEach(i -> importedClasses.add(i.get("classname")));
    additionalProperties.put("x-ts-imported-classes", String.join(", ", importedClasses));


    return objs;
  }

  @Override
  public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
    Map<String, Object> result = super.postProcessAllModels(objs);

    Set<String> modelClassnames = new HashSet<>();
    for (Map.Entry<String, Object> entry : result.entrySet()) {
      Map<String, Object> inner = (Map<String, Object>) entry.getValue();
      List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
      for (Map<String, Object> model : models) {
        CodegenModel codegenModel = (CodegenModel) model.get("model");
        if (!("any".equals(codegenModel.dataType) && codegenModel.vars.size() == 0) && !"Date".equals(codegenModel.dataType) ) {
          // classes that are "any" and have no values are just generic and they don't get generated by the
          // generator, they stay tagged as "any". 
          // Date types (format: date and format: date-time) are the same.
          modelClassnames.add(codegenModel.classname);
        }
        model.put("hasAllOf", codegenModel.allOf.size() > 0);
        model.put("hasOneOf", codegenModel.oneOf.size() > 0);
        codegenModel.vendorExtensions.put("hasInterfaceModels", (codegenModel.interfaceModels != null && codegenModel.interfaceModels.size() > 0));
        model.put("usesDiscriminator", codegenModel.oneOf.size() > 0 || codegenModel.discriminator != null);
      }
    }

    if (objs.size() > 0) {
      additionalProperties.put("x-ts-has-models", "true");
      additionalProperties.put("x-ts-all-models",
        String.join(", ", modelClassnames));

    }


    return result;
  }


  @Override
  protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
    codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(openAPI, schema));
    addImport(codegenModel, codegenModel.additionalPropertiesType);
  }

  private void postProcessCodegenProperty(CodegenProperty var, CodegenModel cm) {
    if (var.dataType == null) {
      var.vendorExtensions.put(X_TS_DESERIALIZE_TYPE, "object");
    } else {
      if (var.dataType.startsWith("{")) {
        var.vendorExtensions.put(X_TS_RECORD_TYPE,  "Record<string, " + var.items.dataType + ">");
        var.vendorExtensions.put(X_TS_ADDITIONAL_PROPS, var.items.dataType);
        cm.vendorExtensions.put(X_TS_ADDITIONAL_PROPS, Boolean.TRUE);

        if (optimized && optimizeDataTypes.contains(var.items.dataType)) {
          var.vendorExtensions.put(X_TS_OPTIMIZE, Boolean.TRUE);
        }
      } else if ("string".equals(var.dataType.toLowerCase())) {
        if (optimized) {
          var.vendorExtensions.put(X_TS_OPTIMIZE, Boolean.TRUE);
        }
        if (var.dataFormat == null) {
          var.vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  "string");
        } else {
          var.vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  var.dataFormat);
        }
      } else {
        if ("date".equals(var.dataType.toLowerCase())) {
          var.vendorExtensions.put(X_TS_DESERIALIZE_TYPE,  var.dataFormat);
        } else {
          var.vendorExtensions.put(X_TS_DESERIALIZE_TYPE, var.dataType);
          if (optimized && optimizeDataTypes.contains(var.dataType)) {
            var.vendorExtensions.put(X_TS_OPTIMIZE, Boolean.TRUE);

          }
        }
      }
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");

    boolean withoutPrefixEnums = false;
    if (additionalProperties.containsKey(WITHOUT_PREFIX_ENUMS)) {
      withoutPrefixEnums = Boolean.parseBoolean(additionalProperties.get(WITHOUT_PREFIX_ENUMS).toString());
    }

    for (Object _mo  : models) {
      Map<String, Object> mo = (Map<String, Object>) _mo;
      CodegenModel cm = (CodegenModel) mo.get("model");

      // tell this specific model it has no additional props fields by default. Inherited models may be different
      cm.vendorExtensions.put(X_TS_ADDITIONAL_PROPS, Boolean.FALSE);

      // Deduce the model file name in kebab case
      cm.classFilename = cm.classname.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);


      if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
        for (CodegenProperty var : cm.vars) {
          postProcessCodegenProperty(var, cm);
        }
      }

      //processed enum names
      if(!withoutPrefixEnums) {
        cm.imports = new TreeSet(cm.imports);
        // name enum with model name, e.g. StatusEnum => PetStatusEnum
        for (CodegenProperty var : cm.vars) {
          if (Boolean.TRUE.equals(var.isEnum)) {
            var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
            var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
          }
        }
        if (cm.parent != null) {
          for (CodegenProperty var : cm.allVars) {
            if (Boolean.TRUE.equals(var.isEnum)) {
              var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
              var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
            }
          }
        }
      }
    }

    final List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");

    if (modelPackage != null) {
      String searchingFor = modelPackage + ".Set";
      // remove the Set class from the imports as it is already in the std library
      (new ArrayList<>(imports)).stream().filter(i -> searchingFor.equals(i.get("import"))).findFirst().ifPresent(imports::remove);
    }


    // Apply the model file name to the imports as well
    for (Map<String, String> m : imports) {
      String javaImport = m.get("import").substring(modelPackage.length() + 1);
      String tsImport = tsModelPackage + "/" + javaImport;
      m.put("tsImport", tsImport);
      m.put("class", javaImport);
      m.put("filename", javaImport.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT));
    }
    return objs;
  }

  /**
   * Overriding toRegularExpression() to avoid escapeText() being called,
   * as it would return a broken regular expression if any escaped character / metacharacter were present.
   */
  @Override
  public String toRegularExpression(String pattern) {
    return addRegularExpressionDelimiter(pattern);
  }

  @Override
  public String toModelFilename(String name) {
    return super.toModelFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
  }

  @Override
  public String toVarName(String name) {
    name = name
      .replaceAll("-", "_")
      .replaceAll("\\$", "__");

    if (name.matches("^[A-Z_]*$")) {
      return name;
    } else {
      name = org.openapitools.codegen.utils.StringUtils.camelize(name, true);
      if (name.matches("^\\d.*")) {
        name = "n" + name;
      }

      if (this.isReservedWord(name)) {
        name = this.escapeReservedWord(name);
      }

      return name.replaceAll("@", "_");
    }
  }

  @Override
  public String toApiFilename(String name) {
    return super.toApiFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
  }

  private void addNpmPackageGeneration() {

    if (additionalProperties.containsKey(NPM_REPOSITORY)) {
      this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
    }

    //Files for building our lib
    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
    supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
  }
}
