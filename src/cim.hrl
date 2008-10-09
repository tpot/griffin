%% CIM error codes

-define(CIM_ERR_FAILED, 1).
-define(CIM_ERR_ACCESS_DENIED, 2).
-define(CIM_ERR_INVALID_NAMESPACE, 3).
-define(CIM_ERR_INVALID_PARAMETER, 4).
-define(CIM_ERR_INVALID_CLASS, 5).
-define(CIM_ERR_NOT_FOUND, 6).
-define(CIM_ERR_NOT_SUPPORTED, 7).
-define(CIM_ERR_CLASS_HAS_CHILDREN, 8).
-define(CIM_ERR_CLASS_HAS_INSTANCES, 9).
-define(CIM_ERR_INVALID_SUPERCLASS, 10).
-define(CIM_ERR_ALREADY_EXISTS, 11).
-define(CIM_ERR_NO_SUCH_PROPERTY, 12).
-define(CIM_ERR_TYPE_MISMATCH, 13).
-define(CIM_ERR_QUERY_LANGUAGE_NOT_SUPPORTED, 14).
-define(CIM_ERR_INVALID_QUERY, 15).
-define(CIM_ERR_METHOD_NOT_AVAILABLE, 16).
-define(CIM_ERR_METHOD_NOT_FOUND, 17).

%% CIM objects

-record(instancename, {
	  classname,
	  keybindings = [],
	  host,
	  namespace
	 }).

-record(keybinding, {
	  name,
	  value
	 }).

-record(keyvalue, {
	  valuetype,
	  cimtype,
	  value
	 }).

-record(instance, {
	  classname,
	  path,
	  qualifiers = [],
	  properties = []
	 }).

-record(property, {
	  name,
	  classorigin,
	  propagated,
	  type,
      qualifiers = [],
	  value
	 }).

-record(property_array, {
	  name,
	  type,
	  arraysize,
	  classorigin,
	  propagated,
	  qualifiers = [],
	  value
	 }).

-record(property_reference, {
	  name,
	  referenceclass,
	  classorigin,
	  propagated,
	  qualifiers = [],
	  value
	 }).

-record(qualifier, {
	  name,
	  value,
      type,
	  propagated,
	  overridable,
	  tosubclass,
	  toinstance,
	  translatable
	 }).

-record(qualifier_declaration, {
	  name,
	  type,
	  scope = [],
	  value,
	  isarray,
	  arraysize,
	  overridable,
      tosubclass,
      toinstance,
      translatable
	 }).

-record(class, {
      name,
      superclass,
      qualifiers = [],
      properties = [],
      methods = []
     }).

-record(classname, {
      name
     }).

-record(method, {
      name,
      type,
      classorigin,
      propagated,
      qualifiers = [],
      parameters = []
     }).

-record(parameter_reference, {
      name,
      referenceclass,
      qualifiers = []
     }).

-record(parameter_array, {
      name,
      type,
      arraysize,
      qualifiers = []
     }).

-record(parameter, {
      name,
      type,
      qualifiers = []
     }).

-record(parameter_refarray, {
      name,
      referenceclass,
      arraysize,
      qualifiers = []
     }).   
