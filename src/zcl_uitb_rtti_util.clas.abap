"! <p class="shorttext synchronized" lang="en">RTTI Util</p>
CLASS zcl_uitb_rtti_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_comp_type,
        component TYPE string,
        type      TYPE string,
      END OF ty_comp_type .
    TYPES:
      tt_comp_type TYPE STANDARD TABLE OF ty_comp_type WITH DEFAULT KEY .

    CLASS-METHODS class_constructor.

    CLASS-METHODS create_table_for_table_type
      IMPORTING
        iv_table_type_name   TYPE tabname
      EXPORTING
        VALUE(er_table_data) TYPE REF TO data
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr .
    CLASS-METHODS create_table_for_table
      IMPORTING
        iv_tabname           TYPE tabname
      EXPORTING
        VALUE(er_table_data) TYPE REF TO data
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr .

    CLASS-METHODS describe_table_by_data
      IMPORTING
        !it_data             TYPE any
      EXPORTING
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr .
    CLASS-METHODS extend_struct_by_components
      IMPORTING
        !ir_struct_descr     TYPE REF TO cl_abap_structdescr
        !it_component_append TYPE tt_comp_type
      EXPORTING
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr .
    CLASS-METHODS extend_table_by_components
      IMPORTING
        !iv_ttype_name       TYPE string OPTIONAL
        !it_data             TYPE any OPTIONAL
        !it_component_append TYPE tt_comp_type
      EXPORTING
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr .
    CLASS-METHODS describe_table_by_name
      IMPORTING
        !iv_tabname            TYPE tabname
      RETURNING
        VALUE(rr_struct_descr) TYPE REF TO cl_abap_structdescr .
    CLASS-METHODS describe_table_type_by_name
      IMPORTING
        iv_table_type        TYPE tabname
      EXPORTING
        VALUE(er_line_type)  TYPE REF TO cl_abap_structdescr
        VALUE(er_table_type) TYPE REF TO cl_abap_tabledescr.
    CLASS-METHODS describe_dtel_by_name
      IMPORTING
        !iv_rollname         TYPE rollname
      RETURNING
        VALUE(rr_elem_descr) TYPE REF TO cl_abap_elemdescr .
    CLASS-METHODS get_table_components
      IMPORTING
        !iv_tabname          TYPE tabname
      RETURNING
        VALUE(rt_components) TYPE zuitb_tab_component_itab .
    CLASS-METHODS describe_class_by_name
      IMPORTING
        !iv_classname         TYPE seoclsname
      RETURNING
        VALUE(rr_class_descr) TYPE REF TO cl_abap_classdescr .
    CLASS-METHODS describe_class_by_ref
      IMPORTING
        !ir_object            TYPE REF TO object
      RETURNING
        VALUE(rr_class_descr) TYPE REF TO cl_abap_classdescr .
    CLASS-METHODS describe_intf_by_name
      IMPORTING
        !iv_intfname         TYPE seoclsname
      RETURNING
        VALUE(rr_intf_descr) TYPE REF TO cl_abap_intfdescr .
    "! <p class="shorttext synchronized" lang="en">Returns components of structure</p>
    CLASS-METHODS get_struct_components
      IMPORTING
        !is_data             TYPE any
      RETURNING
        VALUE(rt_components) TYPE zuitb_abap_comp_type_itab .
    CLASS-METHODS get_elemdescr_by_kind
      IMPORTING
        iv_type_kind     TYPE abap_typekind
        iv_length        TYPE i OPTIONAL
        iv_decimals      TYPE i OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO cl_abap_elemdescr
      RAISING
        cx_parameter_invalid_range.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: gv_typekind_dynamic TYPE string.
    CONSTANTS:
      BEGIN OF c_typekinds,
        int8    TYPE abap_typekind VALUE '8',
        int1    TYPE abap_typekind VALUE 'b',
        int2    TYPE abap_typekind VALUE 's',
        utclong TYPE abap_typekind VALUE 'p',
      END OF c_typekinds.
    CONSTANTS:
      BEGIN OF c_elemdescr_type_getter,
        int1    TYPE abap_methname VALUE 'GET_INT1',
        int2    TYPE abap_methname VALUE 'GET_INT2',
        int8    TYPE abap_methname VALUE 'GET_INT8',
        utclong TYPE abap_methname VALUE 'GET_UTCLONG',
      END OF c_elemdescr_type_getter.
ENDCLASS.



CLASS zcl_uitb_rtti_util IMPLEMENTATION.

  METHOD class_constructor.
    DATA(lo_elem_descr_type) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'CL_ABAP_ELEMDESCR' ) ).
    IF line_exists( lo_elem_descr_type->methods[ name = c_elemdescr_type_getter-int1 ] ).
      gv_typekind_dynamic = gv_typekind_dynamic && c_typekinds-int1.
    ENDIF.
    IF line_exists( lo_elem_descr_type->methods[ name = c_elemdescr_type_getter-int2 ] ).
      gv_typekind_dynamic = gv_typekind_dynamic && c_typekinds-int2.
    ENDIF.
    IF line_exists( lo_elem_descr_type->methods[ name = c_elemdescr_type_getter-int8 ] ).
      gv_typekind_dynamic = gv_typekind_dynamic && c_typekinds-int8.
    ENDIF.
    IF line_exists( lo_elem_descr_type->methods[ name = c_elemdescr_type_getter-utclong ] ).
      gv_typekind_dynamic = gv_typekind_dynamic && c_typekinds-utclong.
    ENDIF.
  ENDMETHOD.

  METHOD create_table_for_table.
    er_line_type = describe_table_by_name( iv_tabname ).
    er_table_type = cl_abap_tabledescr=>create( p_line_type = er_line_type ).
    CREATE DATA er_table_data TYPE HANDLE er_table_type.
  ENDMETHOD.

  METHOD create_table_for_table_type.
    describe_table_type_by_name(
      EXPORTING iv_table_type = iv_table_type_name
      IMPORTING er_line_type  = er_line_type
                er_table_type = er_table_type ).
    CREATE DATA er_table_type TYPE HANDLE er_table_type.
  ENDMETHOD.

  METHOD describe_class_by_name.
    rr_class_descr = CAST #( cl_abap_typedescr=>describe_by_name( iv_classname ) ) .
  ENDMETHOD.


  METHOD describe_class_by_ref.
    rr_class_descr = CAST #( cl_abap_typedescr=>describe_by_object_ref( ir_object ) ).
  ENDMETHOD.


  METHOD describe_dtel_by_name.
    rr_elem_descr = CAST #( cl_abap_typedescr=>describe_by_name( iv_rollname ) ) .
  ENDMETHOD.


  METHOD describe_intf_by_name.
    rr_intf_descr = CAST #( cl_abap_typedescr=>describe_by_name( iv_intfname ) ) .
  ENDMETHOD.


  METHOD describe_table_by_data.
    er_table_type = CAST #( cl_abap_typedescr=>describe_by_data( it_data ) ).

    " get the line type to get the components
    er_line_type = CAST #( er_table_type->get_table_line_type( ) ).
  ENDMETHOD.


  METHOD describe_table_by_name.
    rr_struct_descr = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( to_upper( iv_tabname ) ) ) .
  ENDMETHOD.

  METHOD describe_table_type_by_name.
    er_table_type = CAST #( cl_abap_typedescr=>describe_by_name( to_upper( iv_table_type ) ) ).

    " get the line type to get the components
    er_line_type = CAST #( er_table_type->get_table_line_type( ) ).
  ENDMETHOD.


  METHOD extend_struct_by_components.
    DATA(lt_components) = ir_struct_descr->get_components( ).

    LOOP AT it_component_append ASSIGNING FIELD-SYMBOL(<ls_new_comp>).
      CHECK NOT line_exists( lt_components[ name = <ls_new_comp>-component ] ).
      lt_components = VALUE #( BASE lt_components
        ( name = <ls_new_comp>-component
          type = CAST #( cl_abap_typedescr=>describe_by_name( <ls_new_comp>-type ) ) ) ).
    ENDLOOP.

    " now create the new type
    DATA(lr_line_type_new) = cl_abap_structdescr=>create( p_components = lt_components ).
    DATA(lr_table_type_new) = cl_abap_tabledescr=>create(
      p_line_type = lr_line_type_new ).

    er_line_type = lr_line_type_new.
    er_table_type = lr_table_type_new.
  ENDMETHOD.


  METHOD extend_table_by_components.
    DATA(lr_table_descr) = COND #(
      WHEN it_data IS SUPPLIED THEN
        CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_data ) )
      ELSE
        CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( iv_ttype_name ) ) ).

    extend_struct_by_components(
      EXPORTING ir_struct_descr     = CAST #( lr_table_descr->get_table_line_type( ) )
                it_component_append = it_component_append
      IMPORTING er_line_type        = er_line_type
                er_table_type       = er_table_type ).
  ENDMETHOD.


  METHOD get_struct_components.
    DATA(lr_struct_describer) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_data ) ).

    rt_components = VALUE #(
      FOR comp IN lr_struct_describer->components
      ( name = comp-name ) ).
  ENDMETHOD.


  METHOD get_table_components.
    DATA(lr_tab_describer) = zcl_uitb_rtti_util=>describe_table_by_name( iv_tabname ).

    rt_components = VALUE #(
        FOR comp IN lr_tab_describer->get_ddic_field_list( )
        WHERE ( rollname <> 'MANDT' )
        ( CORRESPONDING #( comp ) ) ).
  ENDMETHOD.

  METHOD get_elemdescr_by_kind.
    IF gv_typekind_dynamic CA iv_type_kind.
      IF iv_type_kind = c_typekinds-int1.
        CALL METHOD cl_abap_elemdescr=>(c_elemdescr_type_getter-int1)
          RECEIVING
            p_result = ro_result.
      ELSEIF iv_type_kind = c_typekinds-int2.
        CALL METHOD cl_abap_elemdescr=>(c_elemdescr_type_getter-int2)
          RECEIVING
            p_result = ro_result.
      ELSEIF iv_type_kind = c_typekinds-int8.
        CALL METHOD cl_abap_elemdescr=>(c_elemdescr_type_getter-int8)
          RECEIVING
            p_result = ro_result.
      ELSEIF iv_type_kind = c_typekinds-utclong.
        CALL METHOD cl_abap_elemdescr=>(c_elemdescr_type_getter-utclong)
          RECEIVING
            p_result = ro_result.
      ELSE.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'IV_TYPE_KIND'
            value     = CONV #( iv_type_kind ).
      ENDIF.
    ELSE.
      ro_result = SWITCH #( iv_type_kind
        WHEN cl_abap_typedescr=>typekind_char THEN
          cl_abap_elemdescr=>get_c( p_length = iv_length )
        WHEN cl_abap_typedescr=>typekind_num THEN
          cl_abap_elemdescr=>get_n( p_length = iv_length )
        WHEN cl_abap_typedescr=>typekind_hex THEN
          cl_abap_elemdescr=>get_x( p_length = iv_length )
        WHEN cl_abap_typedescr=>typekind_packed THEN
          cl_abap_elemdescr=>get_p(
            p_length   = iv_length
            p_decimals = iv_decimals )
        WHEN cl_abap_typedescr=>typekind_date THEN
          cl_abap_elemdescr=>get_d( )
        WHEN cl_abap_typedescr=>typekind_time THEN
          cl_abap_elemdescr=>get_t( )
        WHEN cl_abap_typedescr=>typekind_int THEN
          cl_abap_elemdescr=>get_i( )
        WHEN cl_abap_typedescr=>typekind_int1 THEN
          " Fallback if the GET_INT1( ) does not exist
          cl_abap_elemdescr=>get_i( )
        WHEN cl_abap_typedescr=>typekind_int2 THEN
          " Fallback if GET_INT2( ) does not exist
          cl_abap_elemdescr=>get_i( )
        WHEN c_typekinds-int8 THEN
          cl_abap_elemdescr=>get_i( )
        WHEN cl_abap_typedescr=>typekind_float THEN
          cl_abap_elemdescr=>get_f( )
        WHEN cl_abap_typedescr=>typekind_decfloat16 THEN
          cl_abap_elemdescr=>get_decfloat16( )
        WHEN cl_abap_typedescr=>typekind_decfloat34 THEN
          cl_abap_elemdescr=>get_decfloat34( )
        WHEN cl_abap_typedescr=>typekind_string THEN
          cl_abap_elemdescr=>get_string( )
        WHEN cl_abap_typedescr=>typekind_xstring THEN
          cl_abap_elemdescr=>get_xstring( )
        ELSE
          THROW cx_parameter_invalid_range(
            parameter = 'IV_TYPE_KIND'
            value     = CONV #( iv_type_kind ) ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
