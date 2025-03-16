package com.bluetrainsoftware.maven.openapi

import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.parser.util.SchemaTypeUtil
import org.apache.commons.lang3.StringUtils
import org.openapitools.codegen.*
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen
import org.openapitools.codegen.meta.FeatureSet
import org.openapitools.codegen.meta.features.DocumentationFeature
import org.openapitools.codegen.model.ModelMap
import org.openapitools.codegen.model.ModelsMap
import org.openapitools.codegen.model.OperationsMap
import org.openapitools.codegen.utils.CamelizeOption
import org.openapitools.codegen.utils.ModelUtils
import java.util.*

class EnhancedTypeScriptAxiosClientCodegen : AbstractTypeScriptClientCodegen() {
  private var npmRepository: String? = null

  private var tsModelPackage = ""
  private var optimized = true

  override fun getName(): String {
    return "typescript-axios-enhanced"
  }

  override fun getHelp(): String {
    return "Generates a TypeScript client library using axios."
  }

  override fun processOpts() {
    super.processOpts()
    tsModelPackage = modelPackage.replace("\\.".toRegex(), "/")
    val tsApiPackage = apiPackage.replace("\\.".toRegex(), "/")

    val modelRelativeToRoot = getRelativeToRoot(tsModelPackage)
    val apiRelativeToRoot = getRelativeToRoot(tsApiPackage)

    additionalProperties["tsModelPackage"] = tsModelPackage
    additionalProperties["tsApiPackage"] = tsApiPackage
    additionalProperties["apiRelativeToRoot"] = apiRelativeToRoot
    additionalProperties["modelRelativeToRoot"] = modelRelativeToRoot
    if (!additionalProperties.containsKey("package-axios-version")) { additionalProperties["package-axios-version"] = "^1.7.3" }
    if (!additionalProperties.containsKey("package-typescript-version")) { additionalProperties["package-typescript-version"] = "^4.9.5" }
    if (!additionalProperties.containsKey("package-types-node-version")) { additionalProperties["package-types-node-version"] = "^20.0.0" }


    val generateApis = true == additionalProperties[CodegenConstants.GENERATE_APIS]
    val generateModels = true == additionalProperties[CodegenConstants.GENERATE_MODELS]
    val separateModelsAndApi = additionalProperties.containsKey(SEPARATE_MODELS_AND_API)
    optimized = !additionalProperties.containsKey(DEOPTIMIZE_DESERIALIZATION)
    if (!additionalProperties.containsKey(DONT_GENERATE_INDEX)) {
      supportingFiles.add(SupportingFile("index.mustache", "", "index.ts"))
    }

    if (generateApis) { // this obviates Axios completely
      supportingFiles.add(SupportingFile("baseApi.mustache", "", "base.ts"))
      supportingFiles.add(SupportingFile("configuration.mustache", "", "configuration.ts"))
    }

    if (!separateModelsAndApi) {
      supportingFiles.add(SupportingFile("api.mustache", "", "api.ts"))
    }

    supportingFiles.add(SupportingFile("git_push.sh.mustache", "", "git_push.sh"))
    supportingFiles.add(SupportingFile("gitignore", "", ".gitignore"))
    supportingFiles.add(SupportingFile("npmignore", "", ".npmignore"))

    if (separateModelsAndApi) {
      if (StringUtils.isBlank(modelPackage) && generateModels) {
        throw RuntimeException("modelPackage must be defined")
      }
      if (StringUtils.isBlank(apiPackage) && generateApis) {
        throw RuntimeException("apiPackage must be defined")
      }

      if (generateApis) {
        apiTemplateFiles["apiInner.mustache"] = ".ts"
        supportingFiles.add(SupportingFile("apiIndex.mustache", tsApiPackage, "index.ts"))
      }

      if (generateModels) {
        supportingFiles.add(SupportingFile("modelIndex.mustache", tsModelPackage, "index.ts"))
        additionalProperties["apiModelRelativeToRoot"] = apiRelativeToRoot
      }

      if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
        if (generateModels) {
          modelTemplateFiles["enhancedModel.mustache"] = ".ts"
          supportingFiles.add(SupportingFile("js_serializer.mustache", tsModelPackage, "model_serializer.ts"))
        }
      } else if (generateModels) {
        modelTemplateFiles["model.mustache"] = ".ts"
      }
    }

    if (additionalProperties.containsKey(NPM_NAME)) {
      addNpmPackageGeneration()
    }
  }

  init {
    modifyFeatureSet { features: FeatureSet.Builder -> features.includeDocumentationFeatures(DocumentationFeature.Readme) }

    // clear import mapping (from default generator) as TS does not use it
    // at the moment
    importMapping.clear()

    outputFolder = "generated-code/typescript-axios"
    templateDir = TEMPLATE_FOLDER
    embeddedTemplateDir = templateDir

    typeMapping["DateTime"] = "Date"
    typeMapping["date"] = "Date"
    typeMapping["AnyType"] = "any"

    cliOptions.add(
      CliOption(
        NPM_REPOSITORY,
        "Use this property to set an url of your private npmRepo in the package.json"
      )
    )
    cliOptions.add(
      CliOption(
        WITH_INTERFACES,
        "Setting this property to true will generate interfaces next to the default class implementations.",
        SchemaTypeUtil.BOOLEAN_TYPE
      ).defaultValue(
        false.toString()
      )
    )
    cliOptions.add(
      CliOption(
        SEPARATE_MODELS_AND_API,
        "Put the model and api in separate folders and in separate classes",
        SchemaTypeUtil.BOOLEAN_TYPE
      ).defaultValue(
        false.toString()
      )
    )
    cliOptions.add(
      CliOption(
        WITHOUT_PREFIX_ENUMS,
        "Don't prefix enum names with class names",
        SchemaTypeUtil.BOOLEAN_TYPE
      ).defaultValue(
        false.toString()
      )
    )
    cliOptions.add(
      CliOption(
        USE_SINGLE_REQUEST_PARAMETER,
        "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.",
        SchemaTypeUtil.BOOLEAN_TYPE
      ).defaultValue(
        false.toString()
      )
    )
    cliOptions.add(
      CliOption(
        USE_ENHANCED_SERIALIZER, "Setting this will ensure Axios complies with " +
          "serialization and validation rules and it generates classes to match interfaces"
      )
    )
    cliOptions.add(
      CliOption(
        USE_COALESCE_RETURN_TYPES, "Make a function return all of the  types it " +
          "actually returns wrapped in an AxiosResponse."
      )
    )
    cliOptions.add(
      CliOption(
        DEOPTIMIZE_DESERIALIZATION, "This allows you to force all deserialization and " +
          "serialization through the ObjectSerializer"
      )
    )
    cliOptions.add(
      CliOption(
        DONT_GENERATE_INDEX, "If you are generating in the same directory as other " +
          "code, don't overwrite the index file."
      )
    )
    cliOptions.add(
      CliOption(
        EXPOSE_TRANSFORMERS, "Expose all type transformers directly. Use if you are " +
          "manually triggering serialization."
      )
    )
  }

  private fun enhanceDataTarget(dataType: String?, dataFormat: String?, vendorExtensions: MutableMap<String, Any?>) {
    if (dataType != null) {
      if ("string" == dataType.lowercase()) {
        if (optimized) {
          vendorExtensions[X_TS_OPTIMIZE] = true
        }
        if (dataFormat == null) {
          vendorExtensions["x-ts-string-type"] = true
          vendorExtensions[X_TS_DESERIALIZE_TYPE] = "string"
        } else {
          vendorExtensions[X_TS_DESERIALIZE_TYPE] = dataFormat
        }
      } else {
        if ("date" == dataType.lowercase()) {
          vendorExtensions[X_TS_DESERIALIZE_TYPE] = dataFormat
        } else {
          vendorExtensions[X_TS_DESERIALIZE_TYPE] = dataType
        }
        if (optimized && optimizeDataTypes.contains(dataType)) {
          vendorExtensions[X_TS_OPTIMIZE] = true
        }
      }
    } else {
      vendorExtensions[X_TS_DESERIALIZE_TYPE] = "object"
    }
  }

  override fun postProcessOperationsWithModels(originalObjs: OperationsMap, allModels: List<ModelMap>): OperationsMap {
    val objs = super.postProcessOperationsWithModels(originalObjs, allModels)
    val vals = objs.operations
    val operations = vals.operation
    /*
        Filter all the operations that are multipart/form-data operations and set the vendor extension flag
        'multipartFormData' for the template to work with.
     */
    operations
      .filter { op: CodegenOperation -> op.hasConsumes }
      .filter { op: CodegenOperation ->
        op.consumes.any { opc: Map<String?, String?> ->
          opc.values.any { anObject: String? -> "multipart/form-data" == anObject }
        }
      }
      .forEach { op: CodegenOperation -> op.vendorExtensions.putIfAbsent("multipartFormData", true) }

    val initialImportedClasses = mutableSetOf<String>()

    if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
      initialImportedClasses.add("ObjectSerializer")

      operations
        .filter { op: CodegenOperation -> op.hasProduces }
        .forEach { op: CodegenOperation ->
          var responseTypes = op.responses
            .map { r: CodegenResponse -> if ((r.dataType == null)) "void" else r.dataType }
            .toMutableSet()
          if (responseTypes.isEmpty()) {
            responseTypes = HashSet(listOf("void"))
          }

          op.vendorExtensions["x-ts-responseTypes"] = java.lang.String.join("|", responseTypes)
          op.responses.forEach { bp: CodegenResponse ->
            enhanceDataTarget(bp.dataType, null, bp.vendorExtensions)
            bp.vendorExtensions["x-ts-is-error"] = bp.is4xx || bp.is5xx
            if (bp.dataType != null && "Set<".startsWith(bp.dataType)) {
              bp.uniqueItems = true
            }
          }
        }

      // if they don't produce anything, indicate void
      operations
        .filter { op: CodegenOperation -> !op.hasProduces }
        .forEach { op: CodegenOperation ->
          op.vendorExtensions["x-ts-responseTypes"] = java.lang.String.join(
            "|", HashSet(
              listOf("void")
            )
          )
        }

      operations
        .filter { op: CodegenOperation -> op.hasConsumes }
        .filter { op: CodegenOperation -> op.bodyParam != null }
        .map { op: CodegenOperation -> op.bodyParam }
        .forEach { bp: CodegenParameter ->
          enhanceDataTarget(bp.dataType, bp.dataFormat, bp.vendorExtensions)
          if (bp.dataType != null && "Set<".startsWith(bp.dataType)) {
            bp.uniqueItems = true
          }
        }

      operations
        .filter { obj: CodegenOperation -> obj.hasFormParams }
        .forEach { op: CodegenOperation ->
          // correct the unique items
          op.formParams
            .filter { p: CodegenParameter -> p.dataType != null && p.dataType.startsWith("Set<") }
            .forEach { p: CodegenParameter ->
              p.uniqueItems = true
            }
        }
    }

    // import the actual imported classes
    val importedClasses = initialImportedClasses.toMutableSet()
    additionalProperties["x-ts-imported-classes-set"]?.let { importedClasses.addAll(it as Set<String>) }


    //     // import the actual imported classes
    //    Set<String> importedClasses = (Set<String>)additionalProperties.computeIfAbsent("x-ts-imported-classes-set",
    //      (k) -> new HashSet<String>(initialImportedClasses));
    //
    //
    //    ((List<Map<String, String>>)objs.get("imports")).forEach(i -> importedClasses.add(i.get("classname")));
    //    additionalProperties.put("x-ts-imported-classes", String.join(", ", importedClasses));
    objs.imports.forEach { i ->
      i["classname"]?.let { importedClasses.add(it) }
    }

    additionalProperties["x-ts-imported-classes"] = importedClasses.joinToString(", ")

    return objs
  }

  override fun postProcessAllModels(objs: Map<String, ModelsMap>): Map<String, ModelsMap> {
    val result = super.postProcessAllModels(objs)

    val modelClassnames: MutableSet<String> = HashSet()
    for ((key, info) in result) {
      val extraImports = checkForMapKeyOverride(key, result)

      val models = info.models
      for (model in models!!) {
        val codegenModel = model.model

        if (codegenModel.name == "BaseRolloutStrategy") {
          println("blah")
        }

        // lets fix the isInherited flag first
        codegenModel.interfaceModels?.forEach { inheritedModel ->
          if (fixInheritedFieldsAndMarkInherited(inheritedModel, codegenModel.vars)) {
            codegenModel.vendorExtensions["x-additional-properties-inherited"] = true
          }
        }

        if (!("any" == codegenModel!!.dataType && codegenModel.vars.size == 0) && "Date" != codegenModel.dataType) {
          // classes that are "any" and have no values are just generic, and they don't get generated by the
          // generator, they stay tagged as "any".
          // Date types (format: date and format: date-time) are the same.
          modelClassnames.add(codegenModel.classname)
        }
        model["hasAllOf"] = codegenModel.allOf.size > 0
        model["hasOneOf"] = codegenModel.oneOf.size > 0
        codegenModel.vendorExtensions["hasInterfaceModels"] =
          codegenModel.interfaceModels != null && codegenModel.interfaceModels.size > 0
        model["usesDiscriminator"] = codegenModel.oneOf.size > 0 || codegenModel.discriminator != null
      }

      // now walk through all the imports and re-write them
      val importStatements = info.imports.toMutableList()

      importStatements.forEach { statement: Map<String, String> ->
        val iStatement = statement["import"]
        extraImports.remove(iStatement) // no dupes
      }

      extraImports.forEach { i: String? ->
        importStatements.add(
          tsImport(
            i!!
          )
        )
      }

      info.imports = importStatements
    }

    if (objs.isNotEmpty()) {
      additionalProperties["x-ts-has-models"] = "true"
      additionalProperties["x-ts-all-models"] = java.lang.String.join(", ", modelClassnames)
    }

    return result
  }

  private fun tsImport(importModel: String): Map<String, String> {
    val importMap: MutableMap<String, String> = HashMap()

    importMap["import"] = importModel
    importMap["tsImport"] = importModel.replace('.', '/')
    if (importModel.contains(".")) {
      val className = importModel.substring(importModel.lastIndexOf(".") + 1)
      importMap["class"] = className
      importMap["filename"] = toModelFilename(className)
    } else {
      importMap["class"] = importModel
      importMap["filename"] = toModelFilename(importModel)
    }

    return importMap
  }

  private class XPropertyRef(var model: CodegenModel, var importPath: String)

  private fun checkForMapKeyOverride(modelName: String, modelMap: Map<String, ModelsMap>): MutableSet<String?> {
    val extraImports: MutableSet<String?> = HashSet()

    val info = modelMap[modelName]!!
    val models = info.models
    if (models.size == 1) {
      val model = models[0].model
      if (model != null) {
        model.allVars.forEach { p: CodegenProperty -> resetMapOverrideKey(modelMap, extraImports, p) }
        model.vars.forEach { p: CodegenProperty -> resetMapOverrideKey(modelMap, extraImports, p) }
      }
    }

    return extraImports
  }

  private fun resetMapOverrideKey(
    modelMap: Map<String, ModelsMap>,
    extraImports: MutableSet<String?>,
    p: CodegenProperty
  ) {
    if (p.isMap) {
      var keyType = "string"
      if (!p.getVendorExtensions().containsKey("x-property-ref")) {
        p.getVendorExtensions()["x-property-ref"] = keyType
      } else {
        val ref = p.getVendorExtensions()["x-property-ref"].toString()
        val refName = if (ref.startsWith("#/components")) extractModelFromRef(
          modelMap,
          ref
        ) else extractModelFromShortName(modelMap, ref)
        if (refName != null) {
          extraImports.add(refName.importPath)
          keyType = refName.model.classname
          p.getVendorExtensions()["x-property-ref"] = keyType
          p.dataType = p.dataType.replace("<string,", "<$keyType,")
          p.datatypeWithEnum = p.datatypeWithEnum.replace("<string,", "<$keyType,")
        }
      }
    }
  }

  private fun extractModelFromShortName(info: Map<String, ModelsMap>, ref: String): XPropertyRef? {
    val modelInfo = info[ref]

    if (modelInfo != null) {
      val models = modelInfo.models
      if (models != null && models.size == 1) {
        val model = models[0]["model"] as CodegenModel?
        val importPath = models[0]["importPath"] as String?
        if (importPath != null && model != null) {
          return XPropertyRef(model, importPath)
        }
      }
    }

    return null
  }

  /**
   * here we have to cut off the stuff and then return the model from the short name
   */
  private fun extractModelFromRef(info: Map<String, ModelsMap>, ref: String): XPropertyRef? {
    val shortName = ref.substring(ref.lastIndexOf("/") + 1)
    return extractModelFromShortName(info, shortName)
  }


  override fun getTypeDeclaration(p: Schema<*>?): String {
    val `val` = super.getTypeDeclaration(p)

    if (ModelUtils.isMapSchema(p)) {
      val inner = this.getSchemaAdditionalProperties(p)
      val nullSafeSuffix = if (this.getNullSafeAdditionalProps()) " | undefined" else ""
      return "Record<string, " + this.getTypeDeclaration(
        ModelUtils.unaliasSchema(
          this.openAPI, inner
        )
      ) + nullSafeSuffix + ">"
    }

    return `val`
  }

  override fun getParameterDataType(parameter: Parameter, p: Schema<*>?): String {
    var `val` = super.getParameterDataType(parameter, p)

    if (ModelUtils.isMapSchema(p)) {
      val inner = ModelUtils.getAdditionalProperties(p)
      `val` = "Record<string, " + this.getParameterDataType(parameter, inner) + ">"
    }

    return `val`
  }

  override fun addAdditionPropertiesToCodeGenModel(codegenModel: CodegenModel, schema: Schema<*>?) {
    codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema))
    addImport(codegenModel, codegenModel.additionalPropertiesType)
  }

  private fun postProcessCodegenProperty(`var`: CodegenProperty, cm: CodegenModel?) {
    if (`var`.dataType == null) {
      `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = "object"
    } else {
      if (`var`.dataType.startsWith("Record<")) {
        `var`.vendorExtensions[X_TS_ADDITIONAL_PROPS] = `var`.items.dataType
        cm!!.vendorExtensions[X_TS_ADDITIONAL_PROPS] = true

        if (optimized && optimizeDataTypes.contains(`var`.items.dataType)) {
          `var`.vendorExtensions[X_TS_OPTIMIZE] = true
        }
      } else if ("string" == `var`.dataType.lowercase(Locale.getDefault())) {
        if (optimized) {
          `var`.vendorExtensions[X_TS_OPTIMIZE] = true
        }
        if (`var`.dataFormat == null) {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = "string"
        } else {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = `var`.dataFormat
        }
      } else {
        if ("Array<Date>" == `var`.dataType && "date-time" == `var`.dataFormat) {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = "Array<DateTime>"
        } else if ("Set<Date>" == `var`.dataType && "date-time" == `var`.dataFormat) {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = "Set<DateTime>"
        } else if ("date" == `var`.dataType.lowercase(Locale.getDefault())) {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = `var`.dataFormat
        } else {
          `var`.vendorExtensions[X_TS_DESERIALIZE_TYPE] = `var`.dataType
          if (optimized && optimizeDataTypes.contains(`var`.dataType)) {
            `var`.vendorExtensions[X_TS_OPTIMIZE] = true
          }
        }
      }
    }
  }

  fun fixInheritedFieldsAndMarkInherited(model: CodegenModel, fields: List<CodegenProperty>): Boolean {
    model.vars.forEach { field ->
      fields.firstOrNull { f -> f.baseName == field.baseName }?.let { it.isInherited = true }
    }

    var inheritedAdditionalProperies = model.additionalPropertiesType != null

    model.interfaceModels?.forEach { parentModel ->
      inheritedAdditionalProperies = inheritedAdditionalProperies || fixInheritedFieldsAndMarkInherited(parentModel, fields)
    }

    return inheritedAdditionalProperies
  }

  override fun postProcessModels(objs: ModelsMap): ModelsMap {
    val models = postProcessModelsEnum(objs).models

    var withoutPrefixEnums = false
    if (additionalProperties.containsKey(WITHOUT_PREFIX_ENUMS)) {
      withoutPrefixEnums = additionalProperties[WITHOUT_PREFIX_ENUMS].toString().toBoolean()
    }

    for (mo in models) {
      val cm = mo.model

      cm.vars.forEach { p: CodegenProperty ->
        if (p.getVendorExtensions().containsKey("x-basename")) {
          p.setBaseName(p.getVendorExtensions()["x-basename"].toString())
        }
      }

      // tell this specific model it has no additional props fields by default. Inherited models may be different
      cm.vendorExtensions[X_TS_ADDITIONAL_PROPS] = false

      // Deduce the model file name in kebab case
      cm.classFilename = cm.classname.replace("([a-z0-9])([A-Z])".toRegex(), "$1-$2").lowercase()


      if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
        for (`var` in cm.vars) {
          postProcessCodegenProperty(`var`, cm)
        }
      }

      //processed enum names
      if (!withoutPrefixEnums) {
        // name enum with model name, e.g. StatusEnum => PetStatusEnum
        for (`var` in cm.vars) {
          if (true == `var`.isEnum) {
            `var`.datatypeWithEnum =
              `var`.datatypeWithEnum.replace(`var`.enumName, cm.classname + `var`.enumName)
            `var`.enumName = `var`.enumName.replace(`var`.enumName, cm.classname + `var`.enumName)
          }
        }
        if (cm.parent != null) {
          for (`var` in cm.allVars) {
            if (true == `var`.isEnum) {
              `var`.datatypeWithEnum =
                `var`.datatypeWithEnum.replace(`var`.enumName, cm.classname + `var`.enumName)
              `var`.enumName = `var`.enumName.replace(`var`.enumName, cm.classname + `var`.enumName)
            }
          }
        }
      }
    }

    val imports = objs.imports

    if (modelPackage != null) {
      val searchingFor = "$modelPackage.Set"
      // remove the Set class from the imports as it is already in the std library
      imports.firstOrNull { searchingFor == it["import"] }?.let { imports.remove(it) }
    }


    // Apply the model file name to the imports as well
    for (m in imports!!) {
      val javaImport = m["import"]!!.substring(modelPackage.length + 1)
      val tsImport = "$tsModelPackage/$javaImport"
      m["tsImport"] = tsImport
      m["class"] = javaImport
      m["filename"] = javaImport.replace("([a-z0-9])([A-Z])".toRegex(), "$1-$2").lowercase()
    }
    return objs
  }

  /**
   * Overriding toRegularExpression() to avoid escapeText() being called,
   * as it would return a broken regular expression if any escaped character / metacharacter were present.
   */
  override fun toRegularExpression(pattern: String?): String? {
    return addRegularExpressionDelimiter(pattern)
  }

  override fun toModelFilename(name: String): String {
    return super.toModelFilename(name).replace("([a-z0-9])([A-Z])".toRegex(), "$1-$2").lowercase()
  }

  override fun toVarName(varName: String): String {
    var name = varName
      .replace("-".toRegex(), "_")
      .replace("\\$".toRegex(), "__")

    if (name.matches("^[A-Z_]*$".toRegex())) {
      return name
    } else {
      name = org.openapitools.codegen.utils.StringUtils.camelize(name, CamelizeOption.LOWERCASE_FIRST_CHAR)
      if (name.matches("^\\d.*".toRegex())) {
        name = "n$name"
      }

      if (this.isReservedWord(name)) {
        name = this.escapeReservedWord(name)
      }

      return name.replace("@".toRegex(), "_")
    }
  }

  override fun toApiFilename(name: String): String {
    return super.toApiFilename(name).replace("([a-z0-9])([A-Z])".toRegex(), "$1-$2").lowercase()
  }

  private fun addNpmPackageGeneration() {
    if (additionalProperties.containsKey(NPM_REPOSITORY)) {
      this.npmRepository = additionalProperties[NPM_REPOSITORY].toString()
    }

    //Files for building our lib
    supportingFiles.add(SupportingFile("README.mustache", "", "README.md"))
    supportingFiles.add(SupportingFile("package.mustache", "", "package.json"))
    supportingFiles.add(SupportingFile("tsconfig.mustache", "", "tsconfig.json"))
  }

  companion object {
    const val NPM_REPOSITORY: String = "npmRepository"
    const val WITH_INTERFACES: String = "withInterfaces"
    const val SEPARATE_MODELS_AND_API: String = "withSeparateModelsAndApi"
    const val WITHOUT_PREFIX_ENUMS: String = "withoutPrefixEnums"
    const val USE_SINGLE_REQUEST_PARAMETER: String = "useSingleRequestParameter"
    const val USE_ENHANCED_SERIALIZER: String = "useEnhancedSerializer"
    const val USE_COALESCE_RETURN_TYPES: String = "useCoalesceReturnTypes"
    const val DEOPTIMIZE_DESERIALIZATION: String = "useNonOptimalDeserialization"
    private const val DONT_GENERATE_INDEX = "generateWithoutIndex"
    private const val EXPOSE_TRANSFORMERS = "exposeTransformers"

    const val TEMPLATE_FOLDER: String = "enhanced-axios-ts"
    private const val X_TS_DESERIALIZE_TYPE = "x-ts-deserialize-type"
    private const val X_TS_OPTIMIZE = "x-ts-optimize"
    private const val X_TS_RECORD_TYPE = "x-ts-record-type"
    private const val X_TS_ADDITIONAL_PROPS = "x-ts-additional-props"

    private fun getRelativeToRoot(path: String): String {
      val sb = StringBuilder()
      val slashCount = path.split("/".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray().size
      if (slashCount == 0) {
        sb.append("./")
      } else {
        for (i in 0 until slashCount) {
          sb.append("../")
        }
      }
      return sb.toString()
    }


    // primitive types, anys and arrays of such as never deserialize, so may as well optiize them
    private val optimizeDataTypes: List<String> = mutableListOf(
      "string", "int", "integer", "double", "float",
      "num", "any",
      "number", "boolean", "object",
      "Array<any>", "Array<object>", "Array<string>", "Array<int>", "Array<integer>",
      "Array<any>", "Array<object>", "Array<string>", "Array<int>", "Array<integer>"
    )
  }
}
