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
    CLASS-METHODS get_struct_components
      IMPORTING
        !is_data             TYPE any
      RETURNING
        VALUE(rt_components) TYPE zuitb_abap_comp_type_itab .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_rtti_util IMPLEMENTATION.


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
    rr_struct_descr = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( iv_tabname ) ) .
  ENDMETHOD.


  METHOD extend_struct_by_components.
    DATA(lt_components) = ir_struct_descr->get_components( ).

    lt_components = VALUE #(
      BASE lt_components
      FOR <ls_comp> IN it_component_append
      ( name = <ls_comp>-component
        type = CAST #( cl_abap_typedescr=>describe_by_name( <ls_comp>-type ) ) )
    ).

    " now create the new type
    DATA(lr_line_type_new) = cl_abap_structdescr=>create( p_components = lt_components ).
    DATA(lr_table_type_new) = cl_abap_tabledescr=>create(
                    p_line_type          = lr_line_type_new
*                    p_table_kind         = TABLEKIND_STD
*                    p_unique             = ABAP_FALSE
*                    p_key                =
*                    p_key_kind           = KEYDEFKIND_DEFAULT
                ).
*                  CATCH cx_sy_table_creation.  "

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
                er_table_type       = er_table_type
    ).
  ENDMETHOD.


  METHOD get_struct_components.
    DATA(lr_struct_describer) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_data ) ).

    rt_components = VALUE #(
      FOR comp IN lr_struct_describer->components
      ( name = comp-name )
    ).
  ENDMETHOD.


  METHOD get_table_components.
    DATA(lr_tab_describer) = zcl_uitb_rtti_util=>describe_table_by_name( iv_tabname ).

    rt_components = VALUE #(
        FOR comp IN lr_tab_describer->get_ddic_field_list( )
        WHERE ( rollname <> 'MANDT' )
        ( CORRESPONDING #( comp ) )
    ).
  ENDMETHOD.
ENDCLASS.
